## function to calculate percentiles from time series data
## For SINGLE pollutants and MULTIPLE percentile values



##' Calculate percentile values from a time series
##'
##' Calculates multiple percentile values from a time series, with flexible
##' time aggregation.
##'
##' This is a utility function to calculate percentiles and is used in, for
##' example, \code{timePlot}. Given a data frame with a \code{date} field and
##' one other numeric variable, percentiles are calculated.
##'
##' @param mydata A data frame of data with a \code{date} field in the format
##'   \code{Date} or \code{POSIXct}. Must have one variable to apply
##'   calculations to.
##' @param pollutant Name of variable to process. Mandatory.
##' @param avg.time Averaging period to use. See \code{timeAverage} for
##'   details.
##' @param percentile A vector of percentile values. For example
##'   \code{percentile = 50} for median values, \code{percentile = c(5, 50, 95}
##'   for multiple percentile values.
##' @param data.thresh Data threshold to apply when aggregating data. See
##'   \code{timeAverage} for details.
##' @param start Start date to use - see \code{timeAverage} for details.
##' @export
##' @return Returns a data frame with new columns for each percentile level.
##'   New columns are given names like percentile.95 e.g. when percentile = 95
##'   is chosen. See examples below.
##' @author David Carslaw
##' @seealso \code{\link{timePlot}}, \code{\link{timeAverage}}
##' @keywords methods
##' @examples
##'
##'
##' # 95th percentile monthly o3 concentrations
##' percentiles <- calcPercentile(mydata, pollutant ="o3",
##' avg.time = "month", percentile = 95)
##'
##' head(percentiles)
##'
##' # 5, 50, 95th percentile monthly o3 concentrations
##' \dontrun{
##' percentiles <- calcPercentile(mydata, pollutant ="o3",
##' avg.time = "month", percentile = c(5, 50, 95))
##'
##' head(percentiles)
##' }
##'
calcPercentile <- function(mydata, pollutant = "o3", avg.time = "month", percentile = 50,
                            data.thresh = 0, start = NA) {
    site <- FALSE
    if ("site" %in% names(mydata)) {
        site <- TRUE
        site.name <- mydata$site[1]
    }

    make.percentile <- function(mydata, pollutant = "o3", avg.time = "month", percentile = 50,
                                data.thresh = 0, start = NA) {

        mydata <- timeAverage(mydata, avg.time, statistic = "percentile", percentile = percentile,
                               data.thresh = 0, start.date = NA)
        ## change column name
        new.name <-  paste("percentile.", percentile,  sep = "")
        names(mydata)[names(mydata) == pollutant] <- new.name
        results <- mydata[, new.name, drop = FALSE]
        results <- data.frame(date = mydata$date, results)

        results


    }

    mydata <- lapply(percentile, function(x)
        make.percentile(mydata, pollutant = pollutant, avg.time = avg.time,
                        data.thresh = data.thresh, percentile = x))
    
    mydata <- Reduce(function(x, y, by = 'date') merge(x, y, by = 'date', all = TRUE), mydata)

    if (site) mydata$site <- site.name
    mydata
}
