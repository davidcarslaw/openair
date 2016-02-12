##' Kernel density plot for daily mean exceedance statistics
##'
##' This function is used to explore the conditions leading to exeedances of
##' air quality limits. Currently the focus is on understanding the conditions
##' under which daily limit values for PM10 are in excess of a specified
##' threshold. Kernel density estimates are calculated and plotted to highlight
##' those conditions.
##'
##' The \code{kernelExceed} functions is for exploring the conditions under
##' which exceedances of air pollution limits occur. Currently it is focused on
##' the daily mean (European) Limit Value for PM10 of 50~ug/m3 not to be
##' exceeded on more than 35 days. However, the function is sufficiently
##' flexible to consider other limits e.g. could be used to explore days where
##' the daily mean are greater than 100~ug/m3.
##'
##' By default the function will plot the kernel density estimate of wind speed
##' and wind directions for all days where the concentration of
##' \code{pollutant} is greater than \code{limit}. Understanding the conditions
##' where exceedances occur can help with source identification.
##'
##' The function offers different ways of selecting the data on days where the
##' \code{pollutant} are greater than \code{limit} through setting \code{by}.
##' By default it will select all data on days where \code{pollutant} is
##' greater than \code{limit}. With the default setting of \code{by} it will
##' select all data on those days where \code{pollutant} is greater than
##' \code{limit}, even if individual data (e.g. hours) are less than
##' \code{limit}. Setting \code{by = "dayhour"} will additionally ensure that
##' all data on the those dates are also greater than \code{limit}. Finally,
##' \code{by = "all"} will select all values of \code{pollutant} above limit,
##' regardless of when they occur.
##'
##' The usefulness of the function is greatly enhanced through using
##' \code{type}, which conditions the data according to the level of another
##' variable. For example, \code{type = "season"} will show the kernel density
##' estimate by spring, summer, autumn and winter and \code{type = "so2"} will
##' attempt to show the kernel density estimates by quantiles of SO2
##' concentration. By considering different values of \code{type} it is
##' possible to develop a good understanding of the conditions under which
##' exceedances occur.
##'
##' To aid interpretation the plot will also show the \emph{estimated} number
##' of days or hours where exeedances occur. For \code{type = "default"} the
##' number of days should exactly correspond to the actual number of exceedance
##' days. However, with different values of \code{type} the number of days is
##' an estimate. It is an estimate because conditioning breaks up individual
##' days and the estimate is based on the proportion of data calculated for
##' each level of \code{type}.
##'
##' @param polar A data frame minimally containing \code{date} and at least
##'   three other numeric variables, typically \code{ws}, \code{wd} and a
##'   \code{pollutant}.
##' @param x x-axis variable. Mandatory.
##' @param y y-axis variable. Mandatory
##' @param pollutant Mandatory. A pollutant name corresponding to a variable in
##'   a data frame should be supplied e.g. \code{pollutant = "nox"}
##' @param type The type of analysis to be done. The default is will produce a
##'   single plot using the entire data. Other types include "hour" (for hour
##'   of the day), "weekday" (for day of the week) and "month" (for month of
##'   the year), "year" for a polarPlot for each year. It is also possible to
##'   choose \code{type} as another variable in the data frame. For example,
##'   \code{type = "o3"} will plot four kernel exceedance plots for different
##'   levels of ozone, split into four quantiles (approximately equal numbers
##'   of counts in each of the four splits). This offers great flexibility for
##'   understanding the variation of different variables dependent on another.
##'   See function \code{cutData} for further details.
##' @param by \code{by} determines how data above the \code{limit} are
##'   selected. \code{by = "day"} will select \emph{all} data (typically hours)
##'   on days where the daily mean value is above \code{limit}. \code{by =
##'   "dayhour"} will select only those data above \code{limit} on days where
##'   the daily mean value is above \code{limit}. \code{by = "hour"} will
##'   select all data above \code{limit}.
##' @param limit The threshold above which the \code{pollutant} concentration
##'   will be considered.
##' @param data.thresh The data capture threshold to use (%) when aggregating
##'   the data using \code{timeAverage} to daily means. A value of zero means
##'   that all available data will be used in a particular period regardless if
##'   of the number of values available. Conversely, a value of 100 will mean
##'   that all data will need to be present for the average to be calculated,
##'   else it is recorded as \code{NA}.
##' @param more.than If \code{TRUE} data will be selected that are greater than
##'   \code{limit}. If \code{FALSE} data will be selected that less than
##'   \code{limit}.
##' @param cols Colours to be used for plotting. Options include "default",
##'   "increment", "heat", "spectral", "hue", "brewer1" and user defined (see
##'   manual for more details). The same line colour can be set for all
##'   pollutant e.g. \code{cols = "black"}.
##' @param nbin number of bins to be used for the kernel density estimate.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param \dots Other graphical parameters passed onto \code{lattice:levelplot}
##'   and \code{cutData}. For example, \code{kernelExceed} passes the option
##'   \code{hemisphere = "southern"} on to \code{cutData} to provide southern
##'   (rather than default northern) hemisphere handling of \code{type = "season"}.
##'   Similarly, common axis and title labelling options (such as \code{xlab},
##'   \code{ylab}, \code{main}) are passed to \code{levelplot} via \code{quickText}
##'   to handle routine formatting.
##' @export
##' @return To be completed.
##' @note This function automatically chooses the bandwidth for the kernel
##'   density estimate. We generally find that most data sets are not overly
##'   sensitive to the choice of bandwidth. One important reason for this
##'   insensitivity is likley to be the characteristics of air pollution
##'   itself. Due to atmospheric dispersion processes, pollutant plumes
##'   generally mix rapidly in the atmosphere. This means that pollutant
##'   concentrations are \sQuote{smeared-out} and extra fidelity brought about
##'   by narrower bandwidths do not recover any more detail.
##' @author David Carslaw
##' @seealso \code{\link{polarAnnulus}}, \code{\link{polarFreq}},
##'   \code{\link{polarPlot}}
##' @keywords methods
##' @examples
##'
##' # Note! the manual contains other examples that are more illuminating
##' # basic plot
##' kernelExceed(mydata, pollutant = "pm10")
##'
##' # condition by NOx concentrations
##' \dontrun{kernelExceed(mydata, pollutant = "pm10", type = "nox")}
##'
##'
##'
kernelExceed <- function(polar,
                         x = "wd",
                         y = "ws",
                         pollutant = "pm10",
                         type = "default",
                         by = c("day", "dayhour", "all"),
                         limit = 50,
                         data.thresh = 0,
                         more.than = TRUE,
                         cols = "default",
                         nbin = 256,
                         auto.text = TRUE, ...) {

    ## get rid of R check annoyances
    wd = NULL

    ## extract variables of interest
    vars <- c(y, x, "date", pollutant)
    polar <- checkPrep(polar, vars, type, remove.calm = FALSE)
    polar <- subset(polar, wd > 0)

    ##extra.args
    extra.args <- list(...)
    #label controls
    extra.args$xlab <- if ("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else quickText(x, auto.text)
    extra.args$ylab <- if ("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else quickText(y, auto.text)
    extra.args$main <- if ("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)

    if ("fontsize" %in% names(extra.args))
        trellis.par.set(fontsize = list(text = extra.args$fontsize))

    #greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "cut")))
    }

    ## white data depending on type
    polar <- cutData(polar, type, ...)

    if (by[1] == "day" | by[1] == "dayhour") {
        ## identify days where pm10 > limit
        daily <- timeAverage(polar, "day", data.thresh = data.thresh)

        ## days where this is true - more than or less than a threshold
        if (more.than)   ids <- which(daily[pollutant] > limit) else ids <- which(daily[pollutant] < limit)
        days <- daily$date[ids]

        ## ids for the hours
        ids <- which(as.Date(polar$date) %in% as.Date(days))

        subdata <- polar[ids, ]

        ## only select hour on days that exceed
        if (by[1] == "dayhour") subdata <- subdata[subdata[pollutant] > limit, ]
        subdata <- na.omit(subdata)

    } else {
        if (more.than)   ids <- which(polar[pollutant] > limit) else ids <- which(polar[pollutant] < limit)
        days <- polar$date[ids]

        subdata <- polar[ids, ]
    }

    if(nrow(subdata) == 0) stop(call. = FALSE, "No data above threshold to plot")

    prepare.grid <- function(subdata) {
        x <- subdata[[x]]
        y <- subdata[[y]]

        xy <- xy.coords(x, y, "xlab", "ylab")
        xlab <-  xy$xlab
        ylab <- xy$ylab

        x <- cbind(xy$x, xy$y)[is.finite(xy$x) & is.finite(xy$y), , drop = FALSE]

        xlim <- range(x[, 1])
        ylim <- range(x[, 2])

        map <- .smoothScatterCalcDensity(x, nbin)
        xm <- map$x1
        ym <- map$x2

        dens <- map$fhat

        grid <- expand.grid(x = xm, y = ym)

        results <- data.frame(u = grid$x, v = grid$y, z = as.vector(dens),
                              freq = nrow(subdata) / 24)

        results
    }

#############################################################################

    results.grid <- group_by_(subdata, type) %>%
      do(prepare.grid(.))

    ## adjust to get number of exceedance days
    total.sum <-  sum(unique(results.grid$freq)) ## by each condition
    results.grid$freq <- results.grid$freq * length(days) / total.sum

    strip <- TRUE
    skip <- FALSE
    if (type == "default") strip <- FALSE ## remove strip

    ## auto-scaling
    nlev <- 200  ## preferred number of intervals
    breaks <- unique(pretty(results.grid$z, n = nlev))
    nlev2 <- length(breaks)

    col <- openColours(cols, (nlev2 - 1))
    col <- c("transparent", col) ## add white at bottom
    col.scale <- breaks

    X <- x
    Y <- y ## to avoid confusion with lattice function

    scales <- list()
    if (X == "wd")  scales <- list(x = list(at = seq(0, 360, 90)))

    myform <- formula(paste("z ~ u * v |", type))

    levelplot.args <- list(x = myform, results.grid,
              as.table = TRUE,
              strip = strip,
              region = TRUE,
              scales = scales,
              colorkey = FALSE,

              panel = function(x, y, z, subscripts,...) {

                  panel.levelplot(x, y, z,
                                  subscripts,
                                  at = col.scale,
                                  pretty = TRUE,
                                  col.regions = col,
                                  labels = FALSE)
                  panel.grid(-1, 0, col = "grey90", lty = 5)
                  if (X == "wd") {

                      panel.abline(v = seq(0, 360, by = 90), col = "grey90", lty = 5)

                  } else {

                      panel.grid(0, -1, col = "grey90", lty = 5)

                  }
                  if (by[1] == "day" | by[1] == "dayhour") len <- "days" else len <- "hours"
                  panel.text(0.03 * max(results.grid$u, na.rm = TRUE), 0.95 * max(results.grid$v,
                                                       na.rm = TRUE),
                             paste(round(results.grid[subscripts[1], "freq"]), len), pos = 4,
                             cex = 0.7, ...)

              })

    #reset for extra.args
    levelplot.args<- listUpdate(levelplot.args, extra.args)

    #plot
    ans <- do.call(levelplot, levelplot.args)

    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)
    ans
}

