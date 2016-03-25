## function to provide a summary of air quality statistics by year


##' Calculate summary statistics for air pollution data by year
##'
##' Calculate a range of air pollution-relevant statistics by year and by site.
##'
##' This function calculates a range of common and air pollution-specific
##' statistics from a data frame. The statistics are calculated on an annual
##' basis and the input is assumed to be hourly data. The function can cope
##' with several sites and years. The user can control the output by setting
##' \code{transpose} appropriately.
##'
##' Note that the input data is assumed to be in mass units e.g. ug/m3 for all
##' species except CO (mg/m3).
##'
##' The following statistics are calculated:
##'
##' \itemize{
##' \item \bold{data.capture} --- percentage data capture
##' over a full year.
##'
##' \item \bold{mean} --- annual mean.
##'
##' \item \bold{minimum} --- minimum hourly value.
##'
##' \item \bold{maximum} --- maximum hourly value.
##'
##' \item \bold{median} --- median value.
##'
##' \item \bold{max.daily} --- maximum daily mean.
##'
##' \item \bold{max.rolling.8} --- maximum 8-hour rolling mean.
##'
##' \item \bold{max.rolling.24} --- maximum 24-hour rolling mean.
##'
##' \item \bold{percentile.95} --- 95th percentile. Note that several
##' percentiles can be calculated.
##'
##' \item \bold{roll.8.O3.gt.100} --- number of days when the daily
##' maximum rolling 8-hour mean ozone concentration is >100
##' ug/m3. This is the target value.
##'
##'  \item \bold{roll.8.O3.gt.120} --- number of days when the daily
##' maximum rolling 8-hour mean ozone concentration is >120
##' ug/m3. This is the Limit Value not to be exceeded > 10 days a year.
##'
##' \item \bold{AOT40} --- is the accumulated amount of ozone over the
##' threshold value of 40 ppb for daylight hours in the growing season
##' (April to September). Note that \code{latitude} and
##' \code{longitude} can also be passed to this calculation.
##'
##' \item \bold{hours.gt.200} --- number of hours NO2 is more than 200 ug/m3.
##'
##' \item \bold{days.gt.50} --- number of days PM10 is more than 50 ug/m3. }
##'
##' There can be small discrepancies with the AURN due to the
##' treatment of rounding data. The \code{aqStats} function does not
##' round, whereas AURN data can be rounded at several stages during
##' the calculations.
##'
##' @param mydata A data frame containing a \code{date} field of hourly data.
##' @param pollutant The name of a pollutant e.g. \code{pollutant = c("o3",
##'   "pm10")}.
##' @param data.thresh The data capture threshold in %. No values are
##'   calculated if data capture over the period of interest is less than this
##'   value. \code{data.thresh} is used for example in the calculation of daily
##'   mean values from hourly data. If there are less than \code{data.thresh}
##'   percentage of measurements available in a period, \code{NA} is returned.
##' @param percentile Percentile values to calculate for each pollutant.
##' @param transpose The default is to return a data frame with columns
##'   representing the statistics. If \code{transpose = TRUE} then the results
##'   have columns for each pollutant-site combination.
##' @param ... Other arguments, currently unused.
##' @import lazyeval
##' @export
##' @import reshape2
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## Statistics for 2004. NOTE! these data are in ppb/ppm so the
##' ## example is for illustrative purposes only
##' aqStats(selectByDate(mydata, year = 2004), pollutant = "no2")
##'
##'
aqStats <- function(mydata, pollutant = "no2", data.thresh = 75, percentile = c(95, 99),
                    transpose = FALSE, ...) {

    ## get rid of R check annoyances
    year = site <- NULL; daylight <- NULL; . <- NULL

    ## check data and add 'ste' field if not there
    if (!"site" %in% names(mydata)) mydata$site <- "site"

    vars <- c("date", pollutant, "site")

    mydata <- checkPrep(mydata, vars, "default", remove.calm = FALSE, strip.white = FALSE)

    ## pre-defined lits of pollutants that need special treatment
    thePolls <- c("no2", "o3", "pm10", "co")

    calcStats <- function(mydata, pollutant, data.thresh, percentile, ...) {

        ## select only data needed
        mydata <- mydata[c("date", pollutant, "site")]

        ## file any missing hours
        start.date <- as.POSIXct(dateTrunc(min(mydata$date), "year"))
        end.date <- as.POSIXct(dateCeil(max(mydata$date), "year") - 3600)

        ## find time interval of data and pad any missing times
        interval <- find.time.interval(mydata$date)
        all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
        mydata <- merge(mydata, all.dates, all = TRUE)
        mydata$year <- format(mydata$date, "%Y")
        
        
        Mean <- group_by(mydata, year) %>%
          do(timeAverage(., avg.time = "year", statistic = "mean", data.thresh,
                         print.int = FALSE, type = "site")) %>%
          rename_(mean = pollutant)

        Min <- group_by(mydata, year) %>%
          do(timeAverage(., avg.time = "year", statistic = "min", data.thresh,
                         print.int = FALSE, type = "site")) %>%
          rename_(minimum = pollutant)

        Max <- group_by(mydata, year) %>%
          do(timeAverage(., avg.time = "year", statistic = "max", data.thresh,
                         print.int = FALSE, type = "site")) %>%
          rename_(maximum = pollutant)

        maxDaily <- group_by(mydata, year) %>%
          do(timeAverage(., avg.time = "day", statistic = "mean", data.thresh,
                         print.int = FALSE, type = "site")) %>%
                           do(timeAverage(., avg.time = "year", statistic = "max",
                                          data.thresh, print.int = FALSE,
                                          type = "site")) %>%
          rename_(daily.max = pollutant)

        Median <- group_by(mydata, year) %>%
          do(timeAverage(., avg.time = "year", statistic = "median", data.thresh,
                         type = "site")) %>%
          rename_(median = pollutant)

        dataCapture <- group_by(mydata, year) %>%
          do(timeAverage(., avg.time = "year", statistic = "data.cap",
                         type = "site")) %>%
          rename_(dat.cap = pollutant)

        rollMax8 <- group_by(mydata, year) %>%
          do(rollingMean(., pollutant = pollutant, data.thresh = data.thresh,
                         width = 8, new.name = pollutant)) %>%
          do(timeAverage(., avg.time = "year", statistic = "max",
                         data.thresh, type = "site")) %>%
                           rename_(max.roll.8 = pollutant)

        rollMax24 <- group_by(mydata, year) %>%
          do(rollingMean(., pollutant = pollutant, data.thresh = data.thresh,
                         width = 24, new.name = pollutant)) %>%
          do(timeAverage(., avg.time = "year", statistic = "max",
                         data.thresh, type = "site")) %>%
                           rename_(max.roll.24 = pollutant)

        Percentile <- group_by(mydata, year) %>%
          do(calcPercentile(., avg.time = "year", pollutant = pollutant,
                            data.thresh = data.thresh, percentile = percentile))


        if (length(grep("o3", pollutant, ignore.case = TRUE)) == 1) {

            rollingO3 <- group_by(mydata, year) %>%
              do(rollingMean(., pollutant, data.thresh = data.thresh)) %>%
              do(timeAverage(., avg.time = "day", statistic = "max",
                             data.thresh = data.thresh, type = "site")) %>%
              summarise_(roll.8.O3.gt.100 = interp(~ length(which(var > 100)), var = as.name("rolling8o3")))

            rollingO3$site <- Mean$site ## make sure all have same columns
            rollingO3$date <- Mean$date

            rollingO3b <- group_by(mydata, year) %>%
              do(rollingMean(., pollutant, data.thresh = data.thresh)) %>%
              do(timeAverage(., avg.time = "day", statistic = "max",
                             data.thresh = data.thresh, type = "site")) %>%
                               summarise_(roll.8.O3.gt.120 = interp(~ length(which(var > 120)), var = as.name("rolling8o3")))

            rollingO3b$site <- Mean$site ## make sure all have same columns
            rollingO3b$date <- Mean$date

            AOT40 <- group_by(mydata, year) %>%
              do(AOT40(., pollutant))

            AOT40$site <- Mean$site ## make sure all have same columns
            AOT40$date <- Mean$date

            o3.results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                               Percentile, rollingO3, rollingO3b, AOT40)
            o3.results <- Reduce(function(x, y) merge(x, y, by = c('site', 'year', 'date'),
                                                      all = TRUE), o3.results)
            o3.results$pollutant <- "O3"
            results <- o3.results
            results
        }

        if (length(grep("no2", pollutant, ignore.case = TRUE)) == 1) {

            hours <- group_by(mydata, year) %>%
              summarise_(hours = interp(~ length(which(var > 200)),
                             var = as.name(pollutant)))
            hours$site <- Mean$site ## make sure all have same columns
            hours$date <- Mean$date

            no2.results <- list(dataCapture, Mean, Min, Max, Median,  maxDaily, rollMax8,
                                rollMax24, Percentile, hours)
            
            no2.results <- Reduce(function(x, y) merge(x, y, by = c('site', 'year', 'date'),
                                                       all = TRUE), no2.results)

            no2.results$pollutant <- "NO2"
            results <- no2.results
            results
        }

        if (length(grep("pm10", pollutant, ignore.case = TRUE)) == 1) {

            days <- group_by(mydata, year) %>%
              do(timeAverage(., avg.time = "day", statistic = "mean", data.thresh,
                             type = "site")) %>%
              summarise_(days = interp(~ length(which(var > 50)),
                             var = as.name(pollutant)))
            days$site <- Mean$site ## make sure all have same columns
            days$date <- Mean$date

            pm10.results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8,
                                 rollMax24, Percentile, days)

            pm10.results <- Reduce(function(x, y) merge(x, y, by = c('site', 'year', 'date'),
                                                        all = TRUE), pm10.results)
            pm10.results$pollutant <- "PM10"
            results <- pm10.results
            results
        }

        if (length(grep("co", pollutant, ignore.case = TRUE)) == 1) {

            co.results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                               Percentile)
            co.results <- Reduce(function(x, y) merge(x, y, by = c('site', 'year', 'date'),
                                                    all = TRUE), co.results)
            co.results$pollutant <- "CO"
            results <- co.results
            results
        }
        

        ## see if pollutant string in any pre-defined ones
        ## if not calculate basic stats
        if (all(is.na(sapply(thePolls, function (x) grep(x, pollutant, ignore.case = TRUE)) >0))) {

            results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                            Percentile)
            
            results <- Reduce(function(x, y) merge(x, y, by = c('site', 'year', 'date'),
                                                   all = TRUE), results)
            
            results$pollutant <- pollutant
            results <- results

        }


        results
    }

    ## function to go through sites

    bySite <- function (mydata, pollutant, data.thresh = data.thresh,
                        percentile = percentile, ...) {
        
        ## dates should be unique; issue warning if not
        if (any(duplicated(mydata$date))) warning ("Duplicate dates detected - more than one site?",
                                                   call. = FALSE)


        results <- lapply(pollutant, function (x) calcStats(mydata = mydata, pollutant = x ,
                                                            data.thresh = data.thresh,
                                                            percentile = percentile, ...))

        ## supress warnings about binding factors
        results <- suppressWarnings(bind_rows(results))
        results$year <- as.numeric(results$year)
        results
    }

    
    results <- group_by(mydata, site) %>%
      do(bySite(., pollutant = pollutant, data.thresh = data.thresh,
                percentile = percentile,...))
    

    ## order sensible
    results <- results[c("site", "pollutant", setdiff(names(results), c("site", "pollutant")))]
                                      
    class(results$year) <- "integer"

    ## transpose if requested
    if (transpose) {
        if (length(unique(results$site)) > 1) {
            results <- melt(results, id.vars = c("site", "pollutant", "year"))
            results <- dcast(results, ... ~ site + pollutant)
        } else {
            ## only one site and don't need to add name
            results <- subset(results, select = -site)
            results <- melt(results, id.vars = c("pollutant", "year"))
            results <- dcast(results, ... ~ pollutant)
        }
        ## sort out names
        names(results) <- gsub("\\_", " ", names(results))
    }
    results

}


AOT40 <- function(mydata, pollutant, ...) {
    ## note the assumption is the O3 is in ug/m3
    daylight <- NULL

    ## need daylight hours in growing season (April to September)
    mydata <- selectByDate(mydata, month = 4:9)
    mydata <- cutData(mydata, "daylight", ...)
    mydata <- subset(mydata, daylight == "daylight")
    AOT40 <- ifelse(mydata[[pollutant]] - 80 < 0 , 0, mydata[[pollutant]] - 80)
    AOT40 <- sum(AOT40, na.rm = TRUE) * 0.50 ## for ppb
    AOT40 <- data_frame(AOT40)
    return(AOT40)
}

