##' AURN csv file data import for openair
##' 
##' Function for importing common 1 hour average (hourly) UK Automatic Urban
##' and Rural Network (AURN) Air Quality Archive data files previously
##' downloaded in ".csv" format for use with the openair package.  The function
##' uses \code{read.table} (in \code{utils}) and \code{rbind} (in
##' \code{reshape}).
##' 
##' The importAURN() function was developed for use with air quality monitoring
##' site data files downloaded in standard hourly (or 1 hour average) format
##' using the Air Quality Archive email service. Argument defaults are set to
##' common values to simplify both the import operation and use with openair.
##' 
##' Similar file structures can also be imported using this function with
##' argument modification.
##' 
##' @param file The name of the AURN file to be imported. Default,
##'   \code{file.choose} opens browser. Use of \code{read.table} (in
##'   \code{utils}) also allows this to be a readable text-mode connection or
##'   url (although these options are currently not fully tested).
##' @param header.at The file row holding header information. This is used to
##'   set names for the resulting imported data frame, but may be subject to
##'   further modifications depending on following argument settings.
##' @param data.at The first file row holding actual data. When generating the
##'   data frame, the function will ignore all information before this row, and
##'   attempt to include all data from this row onwards.
##' @param na.strings Strings of any terms that are to be interpreted as NA
##'   values within the file.
##' @param date.name Header name of column holding date information. Combined
##'   with time information as single date column in the generated data frame.
##' @param date.break The break character separating days, months and years in
##'   date information. For example, "-" in "01-01-2009".
##' @param time.name Header name of column holding time information. Combined
##'   with date information as single date column in the generated data frame.
##' @param misc.info Row numbers of any additional information that may be
##'   required from the original file. Each line retained as a character vector
##'   in the generated data frame comment.
##' @param is.site Header name of column holding site information. Setting to
##'   NULL turns this option off.
##' @param bad.24 Reset AURN 24 time stamp. AURN time series are logged as
##'   00:00:01 to 24:00:00 as opposed to the more conventional 00:00:00 to
##'   23:59:59. \code{bad.24 = TRUE} resets the time stamp which is not allowed
##'   in some time series classes or functions.
##' @param correct.time Numerical correction (in seconds) for imported date.
##'   AURN data is logged retrospectively. For 1 hour average data,
##'   \code{correct.time = -3600} resets this to the start of the sampling
##'   period.
##' @param output Output style. Default "final" using \code{import()}.
##' @param data.order A vector of names defining the order of data types. AURN
##'   files typically include three data types, actual data and associated data
##'   quality and measurement unit reports. Here, these are defined as "value",
##'   "status" and "unit", respectively.
##' @param simplify.names A logical (default TRUE) prompting the function to
##'   try to simply data frame names using common chemical shorthand.  FALSE
##'   retains names from original file, although these may be modified if they
##'   contain unallowed characters or non-unique names.
##' @param ... Other parameters. Passed onto and handled by \code{import()}.
##' @export
##' @return The function returns a data frame for use in openair. By comparison
##'   to the original file, the resulting data frame is modified as follows:
##'   Time and date information will combined in a single column "date",
##'   formatted as a conventional timeseries (\code{\link{as.POSIXct}}). Time
##'   adjustments may also be made, subject to "bad.24" and "correct.time"
##'   argument settings.  Using default settings, the argument
##'   \code{correct.time = - 3600} resets the time stamp to the start of the
##'   measurement period. If name simplification was requested
##'   (\code{simplify.names = TRUE}), common chemical names will be simplified.
##'   For example, "carbon monoxide" will be reset to "co". Currently, this
##'   option only applies to inorganics and particulates, not organics.
##'   Non-value information will be rationalised according to data.order. For
##'   example, for the default, \code{data.order = c("value", "status",
##'   "unit")}, the status and unit columns following the "co" column will be
##'   automatically renamed "unit.co" and "status.co", respectively.  An
##'   additional "site" column will be generated. Multiple "site" files are
##'   allowed.  Additional information (as defined in "misc.info") and data
##'   adjustments (as defined by "bad.24" and "correct.time") are retained in
##'   the data frame comment.
##' @author Karl Ropkins
##' @seealso Generic import function \code{\link{import}}, or direct (on-line)
##'   data import function \code{\link{importAURN}}. Other dedicated import
##'   functions available for other file types, e.g.: \code{\link{importKCL}},
##'   \code{\link{importADMS}}, etc.
##' @keywords methods
##' @examples
##' 
##' 
##' ##########
##' #example 1
##' ##########
##' #data obtained from email service:
##' #http://www.airquality.co.uk/archive/data_selector.php
##' #or
##' #http://www.airquality.co.uk/archive/data_and_statistics.php?action=step_pre_1
##' #example file "AirQualityDataHourly.csv" Brighton Roadside and Brighton Preston Park 2008.
##' 
##' #import data as mydata
##' ## mydata <- importAURN.csv("AirQualityDataHourly.csv")
##' 
##' #read additional information retained by importAURN
##' ## comment(mydata)
##' 
##' #analysis data by site
##' ## boxplot(nox ~ site, data = mydata)
##' 
##' ##########
##' #example 2
##' ##########
##' #example using data from url
##' 
##' #import data as otherdata
##' ## otherdata <- importAURN.csv(
##' ##  "http://www.airquality.co.uk/archive/data_files/site_data/HG1_2007.csv")
##' 
##' #use openair function
##' ## summarise(otherdata)
##' 
##' ##########
##' #example 3
##' ##########
##' #example of importing other similar data formats
##' 
##' #import 15 min average so2 data from Bexley using url
##' ## so2.15min.data <- importAURN.csv(
##' ##  "http://www.airquality.co.uk/archive/data_files/15min_site_data/BEX_2008.csv",
##' ##  correct.time = -900)
##' 
##' #note: correct.time amended for 15 min offset/correction.
##' 
##' #additional comments
##' ## comment(so2.15min.data)
##' 
##' #analysis
##' ## diurnal.error(so2.15min.data, pollutant="so2")
##' 
##' #wrapper for above operation
##' ##(e.g. if you have to do this -or similar- a lot of time)
##' ## my.import.wrapper <- function(file, correct.time = -900, ...)
##' ##  { importAURN.csv(file = file, correct.time = correct.time, ...) }
##' 
##' #same as above
##' ## so2.15min.data.again <- my.import.wrapper(
##' ##  "http://www.airquality.co.uk/archive/data_files/15min_site_data/BEX_2008.csv")
##' 
##' #analysis
##' ## timeVariation(so2.15min.data.again, pollutant="so2")
##' 
##' 
importAURNCsv <- function (file = file.choose(), header.at = 5, data.at = 7, na.strings = c("No data", 
    "", "NA"), date.name = "Date", date.break = "-", time.name = "time", 
    misc.info = c(1, 2, 3, 4), is.site = 4, bad.24 = TRUE, correct.time = -3600, 
    output = "final", data.order = c("value", "status", "unit"), 
    simplify.names = TRUE, ...) 
{
    initial.ans <- import(file = file, header.at = header.at, 
        na.strings = na.strings, data.at = data.at, date.name = date.name, 
        date.break = date.break, time.name = time.name, misc.info = misc.info, 
        is.site = NULL, bad.24 = bad.24, correct.time = correct.time, 
        output = "working", ...)
    date.name <- make.names(date.name)
    time.name <- make.names(time.name)
    site.1 <- read.table(file, header = FALSE, sep = initial.ans$ops$sep, 
        skip = (is.site - 1), nrows = 1, colClasses = "character", 
        col.names = initial.ans$names, fill = TRUE, flush = TRUE)
    site.1 <- site.1[1:length(initial.ans$names)]
    names(site.1) <- make.names(initial.ans$names, unique = TRUE)
    site.1 <- site.1[!names(site.1) == date.name]
    site.1 <- site.1[!names(site.1) == time.name]

#revised site handler

    site.2 <- as.character(site.1)
    site.2 <- c(1:length(site.2))[gsub(" ", "", site.2) != ""]

#    site.2 <- as.vector(sapply(site.1[!as.character(site.1) == 
#        "" & !as.character(site.1) == " "], function(x) {
#        grep(x, as.character(site.1), fixed = TRUE)
#    }))

    if (length(site.2) > 1) {
        site.3 <- c(site.2[2:length(site.2)] - 1, ncol(site.1))
    }
    else {
        site.3 <- ncol(site.1)
    }
    site.names <- as.character(as.vector(site.1[site.2]))
    #space at name start
    site.names <- gsub("(^ +)|( +$)", "", site.names)

    initial.ans$data <- lapply(1:(length(site.2)), function(x) {
        ans <- initial.ans$data[site.2[x]:site.3[x]]
        ans.names <- names(ans)
        if (simplify.names == TRUE) {
            ans.names[grep("carbon.monoxide", ans.names, ignore.case = TRUE)] <- "co"
            ans.names[grep("pm10.particulate.matter", ans.names, 
                ignore.case = TRUE)] <- "pm10"
            ans.names[grep("non.volatile.pm10", ans.names, ignore.case = TRUE)] <- "nv.pm10"
            ans.names[grep("volatile.pm10", ans.names, ignore.case = TRUE)] <- "v.pm10"
            ans.names[grep("pm2.5.particulate.matter", ans.names, 
                ignore.case = TRUE)] <- "pm2.5"
            ans.names[grep("non.volatile.pm2.5", ans.names, ignore.case = TRUE)] <- "nv.pm2.5"
            ans.names[grep("volatile.pm2.5", ans.names, ignore.case = TRUE)] <- "v.pm2.5"
            ans.names[grep("nitric.oxide", ans.names, ignore.case = TRUE)] <- "no"
            ans.names[grep("nitrogen.oxides", ans.names, ignore.case = TRUE)] <- "nox"
            ans.names[grep("nitrogen.dioxide", ans.names, ignore.case = TRUE)] <- "no2"
            ans.names[grep("ozone", ans.names, ignore.case = TRUE)] <- "o3"
            ans.names[grep("sulphur.dioxide", ans.names, ignore.case = TRUE)] <- "so2"
        }
        for (i in 1:length(data.order)) {
            if (data.order[i] == "value") {
            }
            else {
                ans.names[grep(data.order[i], ans.names, ignore.case = TRUE)] <- paste(data.order[i], 
                  ".", ans.names[(grep(data.order[i], ans.names, 
                    ignore.case = TRUE)) - (i - 1)], sep = "")
            }
        }
        names(ans) <- ans.names
        site <- rep(site.names[x], nrow(initial.ans$data))
        ans <- cbind(date = initial.ans$date, site = site, ans)
    })
    initial.ans$data <- do.call(bind_rows, initial.ans$data)
    if (simplify.names == TRUE) {
        initial.ans$misc <- c(initial.ans$misc, "importAURN operation: simplify names applied")
    }
    if (!output == "working") {
        ans <- initial.ans$data
        if (!is.null(misc.info)) {
            comment(ans) <- initial.ans$misc
        }
        ids <- which(is.na(ans$date))
        if (length(ids) > 0) {
            ans <- ans[-ids, ]
            warning(paste("Missing dates detected, removing", 
                length(ids), "lines"))
        }
        print(unlist(sapply(ans, class)))
        return(ans)
    }
    else {
        return(initial.ans)
    }
}
