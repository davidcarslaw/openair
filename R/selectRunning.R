##' Function to extract run lengths greater than a threshold
##'
##' Utility function to extract user-defined run lengths (durations) above a
##' threshold
##'
##' This is a utility function to extract runs of values above a certain
##' threshold. For example, for a data frame of hourly NOx values we would like
##' to extract all those hours where the concentration is at least 500ppb for
##' contiguous periods of 5 or more hours.
##'
##' This function is useful, for example, for selecting pollution episodes from
##' a data frame i.e. where concentrations remain elevated for a certain period
##' of time. It may also be of more general use when analysing air pollution
##' data. For example, \code{selectRunning} could be used to extract continuous
##' periods of rainfall --- which could be important for particle
##' concentrations.
##'
##' @param mydata A data frame with a \code{date} field and at least one
##'   numeric \code{pollutant} field to analyse.
##' @param pollutant Name of variable to process. Mandatory.
##' @param run.len Run length for extracting contiguous values of
##'   \code{pollutant} above the \code{threshold} value.
##' @param threshold The threshold value for \code{pollutant} above which data
##'   should be extracted.
##' @export
##' @return Returns a data frame that meets the chosen criteria. See examples
##'   below.
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## extract those hours where there are at least 5 consecutive NOx
##' ## concentrations above 500ppb
##'
##' mydata <- selectRunning(mydata, run.len = 5, threshold = 500)
##'
##' ## make a polar plot of those conditions...shows that those
##' ## conditions are dominated by low wind speeds, not
##' ## in-canyon recirculation
##' \dontrun{polarPlot(mydata, pollutant = "nox")}
##'
selectRunning <- function (mydata, pollutant = "nox", run.len = 5, threshold = 500) {

    ## function to return indices of running values above a certain threshold
    ## make sure the time series is continuous in time with NO gaps
    vars <- c("date", pollutant)

    ## pad out missing data
    thedata <- date.pad(mydata)
    mydata <- thedata ## save for later
    thedata <- checkPrep(mydata, vars, type = "default", remove.calm = FALSE)

    x <- thedata[[pollutant]]
    rle.seq = rle(x > threshold)
    cumsum.seq <- cumsum(rle.seq$lengths)
    myruns <- which(rle.seq$values == 1 & rle.seq$lengths >= run.len)

    ends <- cumsum.seq[myruns]
    newindex <- ifelse(myruns > 1, myruns - 1, 0)
    starts <- cumsum.seq[newindex] + 1
    if (0 %in% newindex) starts = c(1, starts)
    res <- data.frame(starts = starts, ends = ends)

    if (nrow(res) > 0) {
        ids <- lapply(1:nrow(res), function(x) seq(res[x, 1], res[x,2]))
        ids <- do.call(c, ids)
        mydata[ids, ]
    } else {
        print("No conditions found that match criteria")
    }

}

