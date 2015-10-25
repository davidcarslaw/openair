##' Generic data import for openair
##'
##' This function is mostly used to simplify the importing of csv and
##' text file in \code{openair}. In particular it helps to get the
##' date or date/time into the correct format. The file can contain
##' either a date or date/time in a single column or a date in one
##' column and time in another.
##'
##' The function uses \code{\link{strptime}} to parse dates and
##' times. Users should consider the examples for use of these
##' formats.
##'
##' The function can either deal with combined date-time formats
##' e.g. 10/12/1999 23:00 or with two separate columns that deal with
##' date and time. Often there is a column for the date and another
##' for hour. For the latter, the option \code{time.format = "\%H"}
##' should be supplied. Note that R considers hours 0 to 23. However,
##' if hours 1 to 24 are detected \code{import} will correct the hours
##' accordingly.
##'
##' \code{import} will also ensure wind speed and wind direction are
##' correctly labelled (i.e. "ws", "wd") if \code{ws} or \code{wd} are
##' given.
##'
##' Note that it is assumed that the input data are in GMT (UTC)
##' format and in particular there is no consideration of daylight
##' saving time i.e. where in the input data set an hour is missing in
##' spring and duplicated in autumn.
##'
##' Examples of use are given in the openair manual.
##'
##' @param file The name of the file to be imported. Default, \code{file =
##'   file.choose()}, opens browser. Alternatively, the use of
##'   \code{read.table} (in \code{utils}) also allows this to be a character
##'   vector of a file path, connection or url.
##' @param file.type The file format, defaults to common \sQuote{csv} (comma
##'   delimited) format, but also allows \sQuote{txt} (tab delimited).
##' @param sep Allows user to specify a delimiter if not \sQuote{,} (csv) or
##' TAB (txt). For example \sQuote{;} is sometimes used to delineate separate
##' columns.
##' @param header.at The file row holding header information or \code{NULL} if
##'   no header to be used.
##' @param data.at The file row to start reading data from. When generating the
##'   data frame, the function will ignore all information before this row, and
##'   attempt to include all data from this row onwards.
##' @param date Name of the field containing the date. This can be a
##' date e.g. 10/12/2012 or a date-time format e.g. 10/12/2012 01:00.
##' @param date.format The format of the date. This is given in \sQuote{R}
##' format according to \code{strptime}. For example, a date format
##' such as 1/11/2000 12:00 (day/month/year hour:minutes) is given the
##' format \dQuote{\%d/\%m/\%Y \%H:\%M}. See examples below and \code{strptime}
##' for more details.
##' @param time The name of the column containing a time --- if there
##' is one. This is used when a time is given in a separate column and
##' \code{date} contains no information about time.
##' @param time.format If there is a column for \code{time} then the
##' time format must be supplied. Common examples include \dQuote{\%H:\%M}
##' (like 07:00) or an integer giving the hour, in which case the
##' format is \dQuote{\%H}. Again, see examples below.
##' @param tzone The time zone for the data. In order to avoid the
##' complexities of DST (daylight savings time), openair assumes the
##' data are in GMT (UTC) or a constant offset from GMT. Users can set
##' a positive or negative offset in hours from GMT. For example, to
##' set the time zone of the data to the time zone in New York (EST, 5
##' hours behind GMT) set \code{tzone = "Etc/GMT+5"}. To set the time
##' zone of the data to Central European Time (CET, 1 hour ahead of
##' GMT) set \code{tzone = "Etc/GMT-1"}. \emph{Note that the positive
##' and negative offsets are opposite to what most users expect.}
##' @param na.strings Strings of any terms that are to be interpreted
##' as missing (NA). For example, this might be \dQuote{-999}, or
##' \dQuote{n/a} and can be of several items.
##' @param quote String of characters (or character equivalents) the
##' imported file may use to represent a character field.
##' @param ws Name of wind speed field if present if different from
##' \dQuote{ws} e.g. \code{ws = "WSPD"}.
##' @param wd Name of wind direction field if present if different
##' from \dQuote{wd} e.g. \code{wd = "WDIR"}.
##' @param correct.time Numerical correction (in seconds) for imported
##' date.  Default \code{NULL} turns this option off. This can be useful if
##' the hour is represented as 1 to 24 (rather than 0 to 23 assumed by
##' R). In which case \code{correct.time = -3600} will correct the
##' hour.
##' @param ... Other arguments passed to \code{read.table}.
##'
##' @return A data frame formatted for openair use.
##' @author David Carslaw
##' @export
##' @seealso Dedicated import functions available for selected file types, e.g.
##'   : \code{\link{importAURN}}, \code{\link{importAURNCsv}},
##'   \code{\link{importKCL}}, \code{\link{importADMS}}, etc.
##' @keywords methods
##' 
import <- function (file = file.choose(), file.type = "csv", sep = ",", header.at = 1,
                    data.at = 2,  date = "date", date.format = "%d/%m/%Y %H:%M",
                    time = NULL,  time.format = NULL, tzone = "GMT", na.strings = c("", "NA"),
                    quote = "\"", ws = NULL, wd = NULL,
                    correct.time = NULL, ...)
{

    ## read header
    if (header.at > 0 ) {
        Names <- read.table(file, nrows = 1, skip = (header.at - 1), sep = sep,
                            colClasses = "character", na.strings = "")

        ## deal with header columns that are left blank
        if (any(is.na(Names))) {
            id <- which(is.na(Names))
            Names[id] <- colnames(Names)[id]

        }
    }

    ## read data
    thedata <- read.table(file, skip = (data.at - 1), sep = sep, na.strings = na.strings,
                          quote = quote, ...)

    names(thedata) <- Names


    ## rename date field
    if (!date %in% Names) stop (paste("Can't find variable", date))
    names(thedata)[which(Names == date)] <- "date"

    if (!is.null(ws)) {
        if (!ws %in% Names) stop (paste("Can't find variable", ws))
        names(thedata)[which(Names == ws)] <- "ws"
    }

    if (!is.null(wd)) {
        if (!wd %in% Names) stop (paste("Can't find variable", wd))
        names(thedata)[which(Names == wd)] <- "wd"
    }


    ## set date format - if no time column use date format directly
    if (is.null(time)) {
        ## use this to show what date looks like
        exam.date <- do.call("paste", list(head(thedata$date), collapse = ", "))

        thedata$date <- as.POSIXct(strptime(thedata$date, format = date.format, tz = tzone),
                                   tz = tzone)

        ## if all dates are NA, there is a problem...
        if (all(is.na(thedata$date))) stop (paste("Date conversion problems, check that date.format is correct.\n First few dates looks like this:", exam.date))

    } else {

        ## correct hour if 1 to 24
        if (time.format == "%H") {
            if (min(thedata[, time]) == 1 & max(thedata[, time] == 24)) {
                thedata[, time] <- thedata[, time] - 1
            }
        }
        ## time is in a separate column
        thedata$date <- as.POSIXct(strptime(paste(thedata$date, thedata[, time]),
                                            format = paste(date.format, time.format),
                                            tz = tzone), tz = tzone)

        ## if all dates are NA, there is a problem...
        if (all(is.na(thedata$date))) stop ("Date conversion problems, check that date.format and/or time.format is correct")
    }

    if (!is.null(correct.time)) thedata$date <- thedata$date + correct.time

    attr(thedata$date, "tzone") <- tzone

    ## deal with missing dates
    ids <- which(is.na(thedata$date))
    if (length(ids) > 0) {

            thedata <- thedata[-ids, ]
            warning(paste("Missing dates detected, removing",
                length(ids), "lines"), call. = FALSE)
        }

    ## print data types - helps with debugging
    print(unlist(sapply(thedata, class)))

    thedata
}

