##' Time series plot with categories shown as a stacked bar chart
##'
##' This function shows time series plots as stacked bar charts. The
##' different categories in the bar chart are made up from a character
##' or factor variable in a data frame. The function is primarily
##' developed to support the plotting of cluster analysis output from
##' \code{\link{polarCluster}} and \code{\link{trajCluster}} that
##' consider local and regional (back trajectory) cluster analysis
##' respectively. However, the function has more general use for
##' understanding time series data.
##'
##' In order to plot time series in this way, some sort of time
##' aggregation is needed, which is controlled by the option
##' \code{avg.time}.
##'
##' The plot shows the value of \code{pollutant} on the y-axis
##' (averaged according to \code{avg.time}). The time intervals are
##' made up of bars split according to \code{proportion}. The bars
##' therefore show how the total value of \code{pollutant} is made up
##' for any time interval.
##'
##'
##' @param mydata A data frame containing the fields \code{date},
##' \code{pollutant} and a splitting variable \code{proportion}
##' @param pollutant Name of the pollutant to plot contained in
##' \code{mydata}.
##' @param proportion The splitting variable that makes up the bars in
##' the bar chart e.g. \code{proportion = "cluster"} if the output
##' from \code{polarCluster} is being analysed. If \code{proportion}
##' is a numeric variable it is split into 4 quantiles (by default) by
##' \code{cutData}. If \code{proportion} is a factor or character
##' variable then the categories are used directly.
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
##' Note that \code{avg.time} when used in \code{timeProp}
##' should be greater than the time gap in the original data. For
##' example, \code{avg.time = "day"} for hourly data is OK, but
##' \code{avg.time = "hour"} for daily data is not.
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. "season", "year",
##' "weekday" and so on. For example, \code{type = "season"} will
##' produce four plots --- one for each season.
##'
##' It is also possible to choose \code{type} as another variable in
##' the data frame. If that variable is numeric, then the data will be
##' split into four quantiles (if possible) and labelled
##' accordingly. If type is an existing character or factor variable,
##' then those categories/levels will be used directly. This offers
##' great flexibility for understanding the variation of different
##' variables and how they depend on one another.
##'
##' \code{type} must be of length one.
##' @param statistic Determines how the bars are calculated. The
##' default (\dQuote{mean}) will provide the contribution to the
##' overall mean for a time interval. \code{statistic = "frequency"}
##' will give the proportion in terms of counts.
##' @param normalise If \code{normalise = TRUE} then each time
##' interval is scaled to 100. This is helpful to show the relative
##' (percentage) contribution of the proportions.
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
##' and \code{RColorBrewer} colours --- see the \code{openair}
##' \code{openColours} function for more details. For user defined the
##' user can supply a list of colour names recognised by R (type
##' \code{colours()} to see the full list). An example would be
##' \code{cols = c("yellow", "green", "blue")}
##' @param date.breaks Number of major x-axis intervals to use. The
##' function will try and choose a sensible number of dates/times as
##' well as formatting the date/time appropriately to the range being
##' considered.  This does not always work as desired
##' automatically. The user can therefore increase or decrease the
##' number of intervals by adjusting the value of \code{date.breaks}
##' up or down.
##' @param date.format This option controls the date format on the
##' x-axis. While \code{timePlot} generally sets the date format
##' sensibly there can be some situations where the user wishes to
##' have more control. For format types see \code{strptime}. For
##' example, to format the date like \dQuote{Jan-2012} set
##' \code{date.format = "\%b-\%Y"}.
##' @param box.width The width of the boxes for
##' \code{panel.boxplot}. A value of 1 means that there is no gap
##' between the boxes.
##' @param key.columns Number of columns to be used in the key. With
##' many pollutants a single column can make to key too wide. The user
##' can thus choose to use several columns by setting \code{columns}
##' to be less than the number of pollutants.
##' @param key.position Location where the scale key is to plotted.
##' Allowed arguments currently include \dQuote{top}, \dQuote{right},
##' \dQuote{bottom} and \dQuote{left}.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels etc. will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param ... Other graphical parameters passed onto
##' \code{timeProp} and \code{cutData}. For example,
##' \code{timeProp} passes the option \code{hemisphere =
##' "southern"} on to \code{cutData} to provide southern (rather than
##' default northern) hemisphere handling of \code{type = "season"}.
##' Similarly, common axis and title labelling options (such as
##' \code{xlab}, \code{ylab}, \code{main}) are passed to \code{xyplot}
##' via \code{quickText} to handle routine formatting.
##' @export
##' @author David Carslaw
##' @seealso See \code{\link{timePlot}} for time series plotting,
##' \code{\link{polarCluster}} for cluster analysis of bivariate polar
##' plots and \code{\link{trajCluster}} for cluster analysis of
##' HYSPLIT back trajectories.
##' @keywords methods
##' @examples
##'
##' ## See manual for more examples e.g. related to clustering
##'
##' \dontrun{
##' ## monthly plot of NOx showing the contribution by wind sector
##' timeProp(mydata, pollutant="so2", avg.time="month", proportion="wd")
##' }
##'
timeProp <- function(mydata, pollutant = "nox", proportion = "cluster", avg.time = "day",
                     type = "default", statistic = "mean", normalise = FALSE, cols = "Set1", date.breaks = 7,
                     date.format = NULL, box.width = 1, key.columns = 1,
                     key.position = "right", auto.text = TRUE, ...) {

    ## keep check happy
    sums <- NULL; freq <- NULL; Var1 <- NULL

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
    }

    if (length(type) > 1) stop ("'type' can only be of length 1.")

    ## if proportion is not categorical then make it so
    if (!class(mydata[[proportion]]) %in% c("factor")) {

        mydata <- cutData(mydata, proportion)
    }

    if (!statistic %in% c("mean", "frequency"))
        stop ("statisic should be 'mean' or 'frequency'.")

    ## extra.args setup
    extra.args <- list(...)

    ## label controls

    main <- if ("main" %in% names(extra.args))
        quickText(extra.args$main, auto.text) else quickText("", auto.text)
    xlab <- if ("xlab" %in% names(extra.args))
        quickText(extra.args$xlab, auto.text) else "date"
    ylab <- if ("ylab" %in% names(extra.args))
        quickText(extra.args$ylab, auto.text) else quickText(pollutant, auto.text)
    xlim <- if ("xlim" %in% names(extra.args))
        xlim else NULL
    ylim <- if ("ylim" %in% names(extra.args))
        ylim else NULL



    ## variables needed
    vars <- c("date", pollutant, proportion)

    if (any(type %in% dateTypes)) vars <- unique(c("date", vars))

    ## check the data
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    ## need to make sure there are full time series for each proportion
    ## necessary when avg.time is something like "3 month" and time series is non-regular

    mydata <- date.pad(mydata)

    fun.pad <- function (x) {
        the.level <- x[[proportion]][1]
        the.type <- x[[type]][1]
        mydata <- merge(subset(mydata, select = date), x, all = TRUE)
        mydata[[proportion]] <- the.level
        mydata[[type]] <- the.type
        mydata
    }

    mydata <- cutData(mydata, type)

    ## remove missing
    mydata <- na.omit(mydata)
    mydata <- ddply(mydata, c(proportion, type), fun.pad)

    procData <- function(mydata, avg.time, ...) {

        ## time frequencies
        freqs <- ddply(mydata, proportion, timeAverage, avg.time = avg.time, statistic = "frequency")

        ## the values
        values <- ddply(mydata, proportion, timeAverage, avg.time = avg.time, statistic = "mean")

        ## do not weight by concentration if statistic = frequency, just repeat overall mean
        ## by proportion
        if (statistic == "frequency") {

            tmp <- timeAverage(mydata, avg.time)
            values[, pollutant] <- rep(tmp[, pollutant], length(unique(mydata[, proportion])))
        }

        ## add frequencies
        values$freq <- freqs[[pollutant]]

        ## conc * freq
        values$sums <- freqs[[pollutant]] * values[[pollutant]]

        ## weighted conc
        res <- ddply(values, .(date), transform, Var1 = sums / sum(freq, na.rm = TRUE))

        ## normlaise to 100 if needed
        if (normalise) res <- ddply(res, .(date), transform,
                                    Var1 = Var1 * (100 / sum(Var1, na.rm = TRUE)))

        res

    }

    results <- ddply(mydata, type, procData, avg.time)

    ## proper names of labelling ###################################################
    strip.dat <- strip.fun(results, type, auto.text)
    strip <- strip.dat[[1]]
    strip.left <- strip.dat[[2]]
    pol.name <- strip.dat[[3]]


    ## ###################################################################################

    cols <-  openColours(cols, length(levels(results[[proportion]])))

    myform <- formula(paste("Var1 ~ date | ", type, sep = ""))

    dates <- dateBreaks(results$date, date.breaks)$major ## for date scale

    ## date axis formating
    if (is.null(date.format)) {
        formats <- dateBreaks(results$date, date.breaks)$format
    } else {
        formats <- date.format
    }

    scales <- list(x = list(at = dates, format = formats))

    ## work out time gap to get box.width
    tmp <- diff(unique(results$date))
    if (attr(tmp, "units") == "weeks") fac <- 7 * 24 * 3600
    if (attr(tmp, "units") == "days") fac <- 24 * 3600
    if (attr(tmp, "units") == "hours") fac <- 3600

    box.width <- box.width * fac * c(tmp, tmp[length(tmp)])

    y.max <- max(tapply(results[["Var1"]], list(results[["date"]], results[[type]]), sum, na.rm = TRUE))
    thedates <- sort(unique(results$date))
    gap <- difftime(thedates[2], thedates[1], units = "secs")

    if (is.null(xlim)) xlim <- range(results$date) + c(-1 * gap, gap)

    if (is.null(ylim)) ylim <- c(0, 1.04 * y.max)

    if (normalise) ylab <- quickText(paste("% contribution to", pollutant), auto.text)

    ## sub heading
    if (statistic == "frequency") {
        sub <- "contribution weighted by frequency"
    } else {
         sub <- "contribution weighted by mean"
    }

    plt <- xyplot(myform, data = results,
                  as.table = TRUE,
                  strip = strip,
                  strip.left = strip.left,
                  groups = get(proportion),
                  stack = TRUE,
                  sub = sub,
                  scales = scales,
                  col = cols,
                  border = NA,
                  drop.unused.levels = FALSE,
                  horizontal = FALSE,
                  key = list(rectangles = list(col = cols, border = NA),
                  text = list(levels(results[[proportion]])), space = key.position,
                  title = proportion, cex.title = 1, columns = key.columns),
                  par.strip.text = list(cex = 0.8),...,
                  panel = function (..., col) {

                      panel.grid(-1, 0)
                      panel.abline(v = dates, col = "grey95", ...)
                      panel.barchart(..., col = cols, box.width = box.width)
                  }
                  )

    ## update extra args; usual method does not seem to work...
    plt <- modifyList(plt, list(ylab = ylab, xlab = xlab, x.limits = xlim, y.limits = ylim, main = main))

    print(plt)

    invisible(trellis.last.object())

    output <- list(plot = list(plt, trellis.last.object()), data = results, call = match.call())
    class(output) <- "openair"
    invisible(output)
}

