##' Function to calculate time averages for data frames
##'
##' Function to flexibly aggregate or expand data frames by different
##' time periods, calculating vector-averaged wind direction where
##' appropriate. The averaged periods can also take account of data
##' capture rates.
##'
##' This function calculates time averages for a data frame. It also
##' treats wind direction correctly through vector-averaging. For
##' example, the average of 350 degrees and 10 degrees is either 0 or
##' 360 - not 180. The calculations therefore average the wind
##' components.
##'
##' When a data capture threshold is set through \code{data.thresh} it
##' is necessary for \code{timeAverage} to know what the original time
##' interval of the input time series is. The function will try and
##' calculate this interval based on the most common time gap (and
##' will print the assumed time gap to the screen). This works fine
##' most of the time but there are occasions where it may not
##' e.g. when very few data exist in a data frame or the data are
##' monthly (i.e. non-regular time interval between months). In this
##' case the user can explicitly specify the interval through
##' \code{interval} in the same format as \code{avg.time}
##' e.g. \code{interval = "month"}. It may also be useful to set
##' \code{start.date} and \code{end.date} if the time series do not
##' span the entire period of interest. For example, if a time series
##' ended in October and annual means are required, setting
##' \code{end.date} to the end of the year will ensure that the whole
##' period is covered and that \code{data.thresh} is correctly
##' calculated. The same also goes for a time series that starts later
##' in the year where \code{start.date} should be set to the beginning
##' of the year.
##'
##' \code{timeAverage} should be useful in many circumstances where it
##' is necessary to work with different time average data. For
##' example, hourly air pollution data and 15-minute meteorological
##' data. To merge the two data sets \code{timeAverage} can be used to
##' make the meteorological data 1-hour means first. Alternatively,
##' \code{timeAverage} can be used to expand the hourly data to 15
##' minute data - see example below.
##'
##' For the research community \code{timeAverage} should be useful for
##' dealing with outputs from instruments where there are a range of
##' time periods used.
##'
##' It is also very useful for plotting data using
##' \code{\link{timePlot}}.  Often the data are too dense to see
##' patterns and setting different averaging periods easily helps with
##' interpretation.
##'
##' @param mydata A data frame containing a \code{date} field . Can be
##' class \code{POSIXct} or \code{Date}.
##' @param avg.time This defines the time period to average to. Can be
##' \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day},
##' \dQuote{DSTday}, \dQuote{week}, \dQuote{month}, \dQuote{quarter}
##' or \dQuote{year}. For much increased flexibility a number can
##' precede these options followed by a space. For example, a
##' timeAverage of 2 months would be \code{period = "2 month"}. In
##' addition, \code{avg.time} can equal \dQuote{season}, in which case
##' 3-month seasonal values are calculated with spring defined as
##' March, April, May and so on.
##'
##' Note that \code{avg.time} can be \emph{less} than the time
##' interval of the original series, in which case the series is
##' expanded to the new time interval. This is useful, for example,
##' for calculating a 15-minute time series from an hourly one where
##' an hourly value is repeated for each new 15-minute period. Note
##' that when expanding data in this way it is necessary to ensure
##' that the time interval of the original series is an exact multiple
##' of \code{avg.time} e.g. hour to 10 minutes, day to hour. Also, the
##' input time series must have consistent time gaps between
##' successive intervals so that \code{timeAverage} can work out how
##' much \sQuote{padding} to apply. To pad-out data in this way choose
##' \code{fill = TRUE}.
##' @param data.thresh The data capture threshold to use (\%). A value
##' of zero means that all available data will be used in a particular
##' period regardless if of the number of values
##' available. Conversely, a value of 100 will mean that all data will
##' need to be present for the average to be calculated, else it is
##' recorded as \code{NA}. See also \code{interval}, \code{start.date}
##' and \code{end.date} to see whether it is advisable to set these
##' other options.
##' @param statistic The statistic to apply when aggregating the data;
##' default is the mean. Can be one of \dQuote{mean}, \dQuote{max},
##' \dQuote{min}, \dQuote{median}, \dQuote{frequency}, \dQuote{sd},
##' \dQuote{percentile}. Note that \dQuote{sd} is the standard
##' deviation and \dQuote{frequency} is the number (frequency) of
##' valid records in the period.  \dQuote{percentile} is the
##' percentile level (\%) between 0-100, which can be set using the
##' \dQuote{percentile} option --- see below. Not used if \code{avg.time
##' = "default"}.
##' @param percentile The percentile level in \% used when
##' \code{statistic = "percentile"}. The default is 95.
##' @param start.date A string giving a start date to use. This is
##' sometimes useful if a time series starts between obvious
##' intervals. For example, for a 1-minute time series that starts
##' \dQuote{2009-11-29 12:07:00} that needs to be averaged up to
##' 15-minute means, the intervals would be \dQuote{2009-11-29
##' 12:07:00}, \dQuote{2009-11-29 12:22:00} etc. Often, however, it is
##' better to round down to a more obvious start point
##' e.g. \dQuote{2009-11-29 12:00:00} such that the sequence is then
##' \dQuote{2009-11-29 12:00:00}, \dQuote{2009-11-29 12:15:00} \ldots{}
##' \code{start.date} is therefore used to force this type of
##' sequence.
##' @param end.date A string giving an end date to use. This is
##' sometimes useful to make sure a time series extends to a known end
##' point and is useful when \code{data.thresh} > 0 but the input time
##' series does not extend up to the final full interval. For example,
##' if a time series ends sometime in October but annual means are
##' required with a data capture of >75\% then it is necessary to
##' extend the time series up until the end of the year. Input in the
##' format yyyy-mm-dd HH:MM.
##' @param interval The \code{timeAverage} function tries to determine
##' the interval of the original time series (e.g. hourly) by
##' calculating the most common interval between time steps. The
##' interval is needed for calculations where the \code{data.thresh}
##' >0. For the vast majority of regular time series this works
##' fine. However, for data with very poor data capture or irregular
##' time series the automatic detection may not work. Also, for time
##' series such as monthly time series where there is a variable
##' difference in time between months users should specify the time
##' interval explicitly e.g. \code{interval = "month"}. Users can also
##' supply a time interval to \emph{force} on the time series. See
##' \code{avg.time} for the format.
##'
##' This option can sometimes be useful with \code{start.date} and
##' \code{end.date} to ensure full periods are considered e.g. a full
##' year when \code{avg.time = "year"}.
##' @param vector.ws Should vector averaging be carried out on wind
##' speed if available? The default is \code{FALSE} and scalar
##' averages are calculated. Vector averaging of the wind speed is
##' carried out on the u and v wind components. For example, consider
##' the average of two hours where the wind direction and speed of the
##' first hour is 0 degrees and 2m/s and 180 degrees and 2m/s for the
##' second hour. The scalar average of the wind speed is simply the
##' arithmetic average = 2m/s and the vector average is
##' 0m/s. Vector-averaged wind speeds will always be lower than
##' scalar-averaged values.
##' @param fill When time series are expanded i.e. when a time
##' interval is less than the original time series, data are
##' \sQuote{padded out} with \code{NA}. To \sQuote{pad-out} the
##' additional data with the first row in each original time interval,
##' choose \code{fill = TRUE}.
##' @param ... Additional arguments for other functions calling
##' \code{timeAverage}.
##' @export
##' @importFrom Rcpp evalCpp
##' @return Returns a data frame with date in class \code{POSIXct} and will
##'   remove any non-numeric columns except a column "site".
##' @author David Carslaw
##' @seealso See \code{\link{timePlot}} that plots time series data and uses
##'   \code{timeAverage} to aggregate data where necessary.
##' @keywords methods
##' @examples
##'
##' ## daily average values
##' daily <- timeAverage(mydata, avg.time = "day")
##'
##' ## daily average values ensuring at least 75 % data capture
##' ## i.e. at least 18 valid hours
##' \dontrun{daily <- timeAverage(mydata, avg.time = "day", data.thresh = 75)}
##'
##' ## 2-weekly averages
##' \dontrun{fortnight <- timeAverage(mydata, avg.time = "2 week")}
##'
##' ## make a 15-minute time series from an hourly one
##' \dontrun{
##' min15 <-  timeAverage(mydata, avg.time = "15 min", fill = TRUE)
##' }
##'
##'
timeAverage <- function(mydata, avg.time = "day", data.thresh = 0,
                        statistic = "mean", percentile = NA, start.date = NA,
                        end.date = NA, interval = NA,
                        vector.ws = FALSE, fill = FALSE, ...) {

    ## get rid of R check annoyances
    year = season = month = Uu = Vv = site = NULL

    ## extract variables of interest
    vars <- names(mydata)

    ## whether a time series has already been padded to fill time gaps
    padded <- FALSE

    mydata <- checkPrep(mydata, vars, type = "default", remove.calm = FALSE,
                        strip.white = FALSE)

    ## time zone of data
    TZ <- attr(mydata$date, "tzone")
    if (is.null(TZ)) TZ <- "GMT" ## as it is on Windows for BST

    if (!is.na(percentile)) {
        percentile <- percentile / 100
        if (percentile < 0 | percentile > 100) stop("Percentile range outside 0-100")
    }
    if (data.thresh < 0 | data.thresh > 100) stop("Data capture range outside 0-100")

    if (statistic == "mean") form <- "mean(x, na.rm = TRUE)"
    if (statistic == "max") form <- "suppressWarnings(newMax(x))"
    if (statistic == "min") form <- "suppressWarnings(min(x, na.rm = TRUE))"
    if (statistic == "median") form <- "median(x, na.rm = TRUE)"
    if (statistic == "sum") form <- "sum(x, na.rm = TRUE)"
    if (statistic == "sd") form <- "sd(x, na.rm = TRUE)"
    if (statistic == "frequency") form <- "length(na.omit(x))"
    if (statistic == "percentile") form <- "quantile(x, probs = percentile, na.rm = TRUE)"

    ## max retruns -Inf if all are missing (never understood this!) therefore choose to
    ## return NA if all are missing
    newMax <- function(x) {if (all(is.na(x))) return(NA) else max(x, na.rm = TRUE)}

    calc.mean <- function(mydata, start.date) { ## function to calculate means

        ## need to check whether avg.time is > or < actual time gap of data
        ## then data will be expanded or aggregated accordingly

         ## start from a particular time, if given
        if (!is.na(start.date)) {

            firstLine <- data.frame(date = as.POSIXct(start.date, tz = TZ))
            if ("site" %in% names (mydata)) firstLine$site <- mydata$site[1]

            mydata <- rbind.fill(firstLine, mydata)

            ## for cutting data must ensure it is in GMT because combining
            ## data frames when system is not GMT puts it in local time!...
            ## and then cut makes a string/factor levels with tz lost...

            mydata$date <- as.POSIXct(format(mydata$date), tz = TZ)

        }

         if (!is.na(end.date)) {

            lastLine <- data.frame(date = as.POSIXct(end.date, tz = TZ))
            if ("site" %in% names (mydata)) lastLine$site <- mydata$site[1]

            mydata <- rbind.fill(mydata, lastLine)

            ## for cutting data must ensure it is in GMT because combining
            ## data frames when system is not GMT puts it in local time!...
            ## and then cut makes a string/factor levels with tz lost...

            mydata$date <- as.POSIXct(format(mydata$date), tz = TZ)

        }

        ## if interval specified, then use it
        if (!is.na(interval)) {
            mydata <- date.pad2(mydata, interval)
            padded <- TRUE
        }

        ## If interval of original time series not specified, calculate it
        ## time diff in seconds of orginal data
        timeDiff <-  as.numeric(strsplit(find.time.interval(mydata$date),
                                         " ")[[1]][1])

        ## time diff of new interval
        by2 <- strsplit(avg.time, " ", fixed = TRUE)[[1]]

        seconds <- 1
        if (length(by2) > 1) seconds <- as.numeric(by2[1])
        units <- by2[length(by2)]


        if (units == "sec") int <- 1
        if (units == "min") int <- 60
        if (units == "hour") int <- 3600
        if (units == "day") int <- 3600 * 24
        if (units == "week") int <- 3600 * 24 * 7
        if (units == "month") int <- 3600 * 24 * 31 ## approx
        if (units == "quarter" || units == "season") int <- 3600 * 24 * 31 * 3 ## approx
        if (units == "year") int <- 3600 * 8784 ## approx

        seconds <- seconds * int ## interval in seconds
        if (is.na(timeDiff)) timeDiff <- seconds ## when only one row

        ## check to see if we need to expand data rather than aggregate it
        ## i.e. chosen time interval less than that of data
        if (seconds < timeDiff) {

            ## orginal dates
            theDates <- mydata$date

            ## need to add a date to the end when expanding times
            interval <- find.time.interval(mydata$date)

            ## equivalent number of days, used to refine interval for month/year
            days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
                24 / 3600

            ## find time interval of data
            if (class(mydata$date)[1] == "Date") {

                interval <- paste(days, "day")

            } else {
                ## this will be in seconds
                interval <- find.time.interval(mydata$date)

            }

            ## better interval, most common interval in a year
            if (days == 31) interval <- "month"
            if (days %in% c(365, 366)) interval <- "year"

            allDates <- seq(min(mydata$date), max(mydata$date), by = interval)
            allDates <- c(allDates, max(allDates) + timeDiff)

            ## all data with new time interval
            allData <- data.frame(date = seq(min(allDates), max(allDates), avg.time))

            ## merge with orginal data, which leaves gaps to fill
            mydata <- merge(mydata, allData, by = "date", all = TRUE)

            if (fill) {

                ## this will copy-down data to next original row of data
                ## number of additional lines to fill
                inflateFac <-  timeDiff / seconds
                if (timeDiff %% seconds != 0) stop("Non-regular time expansion selected, or non-regular input time series.")

                ## ids of orginal dates in new dates
                ids <- which(mydata$date %in% theDates)

                date <- mydata$date
                mydata <-subset(mydata, select = -date)

                for (i in 1:(inflateFac - 1)) {
                    mydata[ids + i, ] <-  mydata[ids, ]
                }

                mydata <- cbind(date, mydata)
                mydata <- mydata[1:nrow(mydata) - 1, ] ## don't need last row
            }

            ## when expanding with column 'site' make sure it is added
            if ("site" %in% names(mydata)) mydata$site <- mydata$site[1]
            return(mydata)

        }



        ## calculate wind components
        if ("wd" %in% names(mydata)) {
            if (is.numeric(mydata$wd) && "ws" %in% names(mydata)) {
                mydata <- transform(mydata,  Uu = ws * sin(2 * pi * wd / 360),
                                    Vv = ws * cos(2 * pi * wd / 360))

            }

            if (is.numeric(mydata$wd) && !"ws" %in% names(mydata)) {
               mydata <- transform(mydata,  Uu = sin(2 * pi * wd / 360),
                                    Vv = cos(2 * pi * wd / 360))
            }
        }

        if (avg.time == "season") {
            ## special case for season
            ## need to group specific months: Dec/Jan/Feb etc

            mydata <- cutData(mydata, type = "season")
            ## remove any missing seasons e.g. through type = "season"
            mydata <- mydata[!is.na(mydata$season), ]

            ## calculate year
            mydata <- transform(mydata, year = as.numeric(format(date, "%Y")),
                                month = as.numeric(format(date, "%m")))

            ## ids where month = 12, make December part of following year's season
            ids <- which(mydata$month == 12)
            mydata$year[ids] <- mydata$year[ids] + 1

            ## find mean date in year-season
            mydata <- transform(mydata, cuts = ave(date, list(year, season), FUN = mean))

            mydata <- subset(mydata, select = -c(year, month))


        } else {

            ## cut into sections dependent on period
            mydata$cuts <- cut(mydata$date, avg.time)

        }


        if (data.thresh > 0) {

            ## two methods of calculating stats, one that takes account of
            ## data capture (slow), the other not (faster)
            ## Note need to know time interval of data to work out data capture, can
            ## be a problem for non-regular time series...

            newMethod <- function(x, data.thresh, na.rm) {

                ## calculate mean only if above data capture threshold
                if (length(na.omit(x)) >= length(x) * data.thresh / 100) {
                    res <- eval(parse(text = form))
                } else {
                    res <- NA
                }
                res
            }

            ## need to make sure all data are present..
            ## print out time interval assumed for input time series
            ## useful for debugging
            if (!padded) mydata <- date.pad(mydata, print.int = TRUE)

            ## cut into sections dependent on period
            mydata$cuts <- cut(mydata$date, avg.time)

            dailymet <- aggregate(mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                         drop = FALSE], list(date = mydata$cuts), newMethod,
                                  na.rm = TRUE,  data.thresh = data.thresh)
        }

        if (data.thresh == 0 & statistic != "mean") {

            dailymet <- aggregate(mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                         drop = FALSE], list(date = mydata$cuts),
                                  function (x) eval(parse(text = form)))

        }

        if (data.thresh == 0 & statistic == "mean") {

            dailymet <- aggregate(mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                         drop = FALSE], list(date = mydata$cuts),
                                  mean, na.rm = TRUE)

        }
        ## return same date class as went in...
        if (class(mydata$date)[1] == "Date") {
            dailymet$date <- as.Date(dailymet$date)


        } else {
            ## return the same TZ that we started with
            dailymet$date <- as.POSIXct(format(dailymet$date), tz = TZ)

        }

        if ("wd" %in% names(mydata)) {
            if (is.numeric(mydata$wd)) {
                ## mean wd
                dailymet <- within(dailymet, wd <- as.vector(atan2(Uu, Vv) * 360 / 2 / pi))

                ## correct for negative wind directions
                ids <- which(dailymet$wd < 0)  ## ids where wd < 0
                dailymet$wd[ids] <- dailymet$wd[ids] + 360

                ## vector average ws
                if ("ws" %in% names(mydata)) {
                    if (vector.ws) dailymet <- within(dailymet, ws <- (Uu ^ 2 + Vv ^ 2) ^ 0.5)
                }

                dailymet <- subset(dailymet, select = c(-Uu, -Vv))
            }
        }

        ## fill missing gaps
        if (avg.time != "season") {

            dailymet <- date.pad2(dailymet, interval = avg.time)
        }

        ## when expanding with column 'site' make sure it is added
        if ("site" %in% names(mydata)) mydata$site <- mydata$site[1]
        dailymet

    }

    ## split if several sites
    if ("site" %in% names(mydata)) { ## split by site
        mydata$site <- factor(mydata$site)
        mydata <- ddply(mydata, .(site), calc.mean, start.date)

    } else {
        mydata <- calc.mean(mydata, start.date)
    }

    mydata
}
