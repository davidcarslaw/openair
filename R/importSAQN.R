##' Scottish Air Quality Network data import for openair
##'
##' Function for importing hourly mean Scottish Air Quality Network (SAQN)
##' archive data files for use with the \code{openair} package. Files are
##' imported from a remote server operated by AEA that provides air quality data
##' files as R data objects.
##'
##' The \code{importSAQN} function has been written to make it easy to import
##' data from the Scottish Air Quality Network (SAQN) ---
##' \url{http://www.scottishairquality.co.uk/index.php}. AEA have provided
##' .RData files (R workspaces) of all individual sites and years for the SAQN.
##' These files are updated on a daily basis. This approach requires a link to
##' the Internet to work.
##'
##' For a list of up to date site codes and site information, see
##' \code{\link{importMeta}} and in particular \code{importMeta(source =
##' "saqn")}.
##'
##' There are several advantages over the web portal approach where .csv files
##' are downloaded. First, it is quick to select a range of sites, pollutants
##' and periods (see examples below). Second, storing the data as .RData objects
##' is very efficient as they are about four times smaller than .csv files ---
##' which means the data downloads quickly and saves bandwidth. Third, the
##' function completely avoids any need for data manipulation or setting time
##' formats, time zones etc. Finally, it is easy to import many years of data
##' beyond the current limit of about 64,000 lines. The final point makes it
##' possible to download several long time series in one go. The function also
##' has the advantage that the proper site name is imported and used in
##' \code{openair} functions.
##'
##' The site codes and pollutant names can be upper or lower case. The function
##' will issue a warning when data less than six months old is downloaded, which
##' may not be ratified.
##'
##' The data are imported by stacking sites on top of one another and will have
##' field names \code{site}, \code{code} (the site code) and \code{pollutant}.
##' Sometimes it is useful to have columns of site data. This can be done using
##' the \code{reshape} function --- see examples below.
##'
##' All units are expressed in mass terms for gaseous species (ug/m3 for NO,
##' NO2, NOx (as NO2), SO2; and mg/m3 for CO). PM10 concentrations are provided
##' in gravimetric units of ug/m3 or scaled to be comparable with these units.
##' Over the years a variety of instruments have been used to measure
##' particulate matter and the technical issues of measuring PM10 are complex.
##' In recent years the measurements rely on FDMS (Filter Dynamics Measurement
##' System), which is able to measure the volatile component of PM. In cases
##' where the FDMS system is in use there will be a separate volatile component
##' recorded as 'v10', which is already included in the absolute PM10
##' measurement. Prior to the use of FDMS the measurements used TEOM (Tapered
##' Element Oscillating. Microbalance) and these concentrations have been
##' multiplied by 1.3 to provide an estimate of the total mass including the
##' volatile fraction.
##'
##' The few BAM (Beta-Attenuation Monitor) instruments that have been
##' incorporated into the network throughout its history have been scaled by 1.3
##' if they have a heated inlet (to account for loss of volatile particles) and
##' 0.83 if they do not have a heated inlet. The few TEOM instruments in the
##' network after 2008 have been scaled using VCM (Volatile Correction Model)
##' values to account for the loss of volatile particles. The object of all
##' these scaling processes is to provide a reasonable degree of comparison
##' between data sets and with the reference method and to produce a consistent
##' data record over the operational period of the network, however there may be
##' some discontinuity in the time series associated with instrument changes.
##'
##' No corrections have been made to the PM2.5 data. The volatile component of
##' FDMS PM2.5 (where available) is shown in the 'v2.5' column.
##'
##'
##' @param site Site code of the SAQN site to import e.g. "gla4" is Glasgow
##'   Kerbside. Several sites can be imported with \code{site = c("gla4", "ed")}
##'   --- to import Glasgow Kerbside and Edinbrugh Centre for example.
##' @param year Year or years to import. To import a sequence of years from 1990
##'   to 2000 use \code{year = 1990:2000}. To import several specfic years use
##'   \code{year = c(1990, 1995, 2000)} for example.
##' @param pollutant Pollutants to import. If omitted will import all pollutants
##'   ffrom a site. To import only NOx and NO2 for example use \code{pollutant =
##'   c("nox", "no2")}.
##' @return Returns a data frame of hourly mean values with date in POSIXct
##'   class and time zone GMT.
##' @author David Carslaw and Trevor Davies (AEA)
##' @seealso See \code{\link{importAURN}} for data elsewhere in the UK and
##'   \code{\link{importKCL}} for importing comprehensive data in and around
##'   London.
##' @keywords methods
##' @export
##' @examples
##'
##'
##' ## import all pollutants from Glasgow Roadside
##' \dontrun{glas <- importSAQN(site = "gla4", year = 2000:2009)}
##'
##' ## import all pollutants from Lerwick rural site (O3)
##' \dontrun{ler <- importSAQN(site = "lerw", year = 2005:2010)}
##'
##' ## import all pollutants from Glasgow/Dundee Centre for 2009
##' \dontrun{all <- importSAQN(site = c("gla3", "dun3"), year = 2009)}
##'
##' 
importSAQN <- function(site = "gla4", year = 2009, pollutant = "all") {
  site <- toupper(site)


  files <- lapply(site, function(x) paste(x, "_", year, sep = ""))

  files <- do.call(c, files)


  loadData <- function(x) {
    tryCatch({
      fileName <- paste("http://www.scottishairquality.co.uk/openair/R_data/", x, ".RData", sep = "")
      con <- url(fileName)
      load(con, envir = .GlobalEnv)
      x
    },
    error = function(ex) {
      cat(x, "does not exist - ignoring that one.\n")
    },
    finally = {
        close(con)
    }
    )
  }

  thedata <- lapply(files, loadData)

  # Return if no data
  if (length(thedata) == 0) return() ## no data

  theObjs <- unlist(thedata)
  ## note unlist will drop NULLs from non-existant sites/years
  mylist <- lapply(theObjs, get)

  thedata <- suppressWarnings(do.call(bind_rows, mylist))
  if (is.null(thedata) || nrow(thedata) == 0) stop("No data to import - check site codes and year.", call. = FALSE)

  thedata$site <- factor(thedata$site, levels = unique(thedata$site))

  ## change names
  names(thedata) <- tolower(names(thedata))

  ## change nox as no2
  id <- which(names(thedata) %in% "noxasno2")
  if (length(id) == 1) names(thedata)[id] <- "nox"

  ## set class to integer for post-processing convenience
  if ("nox" %in% names(thedata)) class(thedata$nox) <- "integer"
  if ("no" %in% names(thedata)) class(thedata$no) <- "integer"
  if ("no2" %in% names(thedata)) class(thedata$no2) <- "integer"
  if ("o3" %in% names(thedata)) class(thedata$o3) <- "integer"
  if ("so2" %in% names(thedata)) class(thedata$so2) <- "integer"
  if ("pm10" %in% names(thedata)) class(thedata$pm10) <- "integer"
  if ("pm2.5" %in% names(thedata)) class(thedata$pm2.5) <- "integer"
  if ("v10" %in% names(thedata)) class(thedata$v10) <- "integer"
  if ("v2.5" %in% names(thedata)) class(thedata$v2.5) <- "integer"


  ## should hydrocarbons be imported?

  ## no hydrocarbons - therefore select conventional pollutants
  theNames <- c(
    "date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
    "v10", "v2.5", "ws", "wd", "code", "site"
  )

  thedata <- thedata[, which(names(thedata) %in% theNames)]


  ## if particular pollutants have been selected
  if (!missing(pollutant) && pollutant != "all") {
    thedata <- thedata[, c("date", pollutant, "site", "code")]
  }

  rm(list = theObjs, pos = 1)

  ## warning about recent, possibly unratified data
  timeDiff <- difftime(Sys.time(), max(thedata$date), units = "days")
  if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")
  }

  ## make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"

  thedata
}
