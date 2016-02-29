##' Function to plot wind speed/direction frequencies and other statistics
##'
##' \code{polarFreq} primarily plots wind speed-direction frequencies in
##' \sQuote{bins}. Each bin is colour-coded depending on the frequency of
##' measurements. Bins can also be used to show the concentration of pollutants
##' using a range of commonly used statistics.
##'
##' \code{polarFreq} is its default use provides details of wind speed and
##' direction frequencies. In this respect it is similar to
##' \code{\link{windRose}}, but considers wind direction intervals of 10
##' degrees and a user-specified wind speed interval. The frequency of wind
##' speeds/directions formed by these \sQuote{bins} is represented on a colour
##' scale.
##'
##' The \code{polarFreq} function is more flexible than either
##' \code{\link{windRose}} or \code{\link{polarPlot}}. It can, for example,
##' also consider pollutant concentrations (see examples below). Instead of the
##' number of data points in each bin, the concentration can be shown. Further,
##' a range of statistics can be used to describe each bin - see
##' \code{statistic} above. Plotting mean concentrations is useful for source
##' identification and is the same as \code{\link{polarPlot}} but without
##' smoothing, which may be preferable for some data. Plotting with
##' \code{statistic = "weighted.mean"} is particularly useful for understanding
##' the relative importance of different source contributions. For example,
##' high mean concentrations may be observed for high wind speed conditions,
##' but the weighted mean concentration may well show that the contribution to
##' overall concentrations is very low.
##'
##' \code{polarFreq} also offers great flexibility with the scale used and the
##' user has fine control over both the range, interval and colour.
##'
##' @param mydata A data frame minimally containing \code{ws}, \code{wd} and
##'   \code{date}.
##' @param pollutant Mandatory. A pollutant name corresponding to a variable in
##'   a data frame should be supplied e.g. \code{pollutant = "nox"}
##' @param statistic The statistic that should be applied to each wind
##' speed/direction bin. Can be \dQuote{frequency}, \dQuote{mean},
##' \dQuote{median}, \dQuote{max} (maximum), \dQuote{stdev} (standard
##' deviation) or \dQuote{weighted.mean}. The option
##' \dQuote{frequency} (the default) is the simplest and plots the
##' frequency of wind speed/direction in different bins. The scale
##' therefore shows the counts in each bin. The option \dQuote{mean}
##' will plot the mean concentration of a pollutant (see next point)
##' in wind speed/direction bins, and so on.  Finally,
##' \dQuote{weighted.mean} will plot the concentration of a pollutant
##' weighted by wind speed/direction. Each segment therefore provides
##' the percentage overall contribution to the total concentration.
##' More information is given in the examples. Note that for options
##' other than \dQuote{frequency}, it is necessary to also provide the
##' name of a pollutant. See function \code{cutData} for further
##' details.
##' @param ws.int Wind speed interval assumed. In some cases e.g. a low met
##'   mast, an interval of 0.5 may be more appropriate.
##' @param grid.line Radial spacing of grid lines.
##' @param breaks The user can provide their own scale. \code{breaks} expects a
##'   sequence of numbers that define the range of the scale. The sequence
##'   could represent one with equal spacing e.g. \code{breaks = seq(0, 100,
##'   10)} - a scale from 0-10 in intervals of 10, or a more flexible sequence
##'   e.g. \code{breaks = c(0, 1, 5, 7, 10)}, which may be useful for some
##'   situations.
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
##' and \code{RColorBrewer} colours --- see the \code{openair}
##' \code{openColours} function for more details. For user defined the
##' user can supply a list of colour names recognised by R (type
##' \code{colours()} to see the full list). An example would be
##' \code{cols = c("yellow", "green", "blue")}
##' @param trans Should a transformation be applied? Sometimes when producing
##'   plots of this kind they can be dominated by a few high points. The
##'   default therefore is \code{TRUE} and a square-root transform is applied.
##'   This results in a non-linear scale and (usually) a better representation
##'   of the distribution. If set to \code{FALSE} a linear scale is used.
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. \dQuote{season},
##' \dQuote{year}, \dQuote{weekday} and so on. For example, \code{type
##' = "season"} will produce four plots --- one for each season.
##'
##' It is also possible to choose \code{type} as another variable in
##' the data frame. If that variable is numeric, then the data will be
##' split into four quantiles (if possible) and labelled
##' accordingly. If type is an existing character or factor variable,
##' then those categories/levels will be used directly. This offers
##' great flexibility for understanding the variation of different
##' variables and how they depend on one another.
##'
##' Type can be up length two e.g. \code{type = c("season", "weekday")} will
##'   produce a 2x2 plot split by season and day of the week. Note, when two
##'   types are provided the first forms the columns and the second the rows.
##' @param min.bin The minimum number of points allowed in a wind speed/wind
##'   direction bin.  The default is 1. A value of two requires at least 2
##'   valid records in each bin an so on; bins with less than 2 valid records
##'   are set to NA. Care should be taken when using a value > 1 because of the
##'   risk of removing real data points. It is recommended to consider your
##'   data with care. Also, the \code{polarPlot} function can be of use in such
##'   circumstances.
##' @param ws.upper A user-defined upper wind speed to use. This is useful for
##'   ensuring a consistent scale between different plots. For example, to
##'   always ensure that wind speeds are displayed between 1-10, set
##'   \code{ws.int = 10}.
##' @param offset \code{offset} controls the size of the \sQuote{hole}
##' in the middle and is expressed as a percentage of the maximum wind
##' speed. Setting a higher \code{offset} e.g. 50 is useful for
##' \code{statistic = "weighted.mean"} when \code{ws.int} is greater
##' than the maximum wind speed. See example below.
##' @param border.col The colour of the boundary of each wind speed/direction
##'   bin. The default is transparent. Another useful choice sometimes is
##'   "white".
##' @param key.header,key.footer Adds additional text/labels to the scale key.
##'   For example, passing options \code{key.header = "header", key.footer =
##'   "footer"} adds addition text above and below the scale key. These
##'   arguments are passed to \code{drawOpenKey} via \code{quickText}, applying
##'   the \code{auto.text} argument, to handle formatting.
##' @param key.position Location where the scale key is to plotted.  Allowed
##'   arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
##'   and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{drawOpenKey} for further details.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the \sQuote{2}
##'   in NO2.
##' @param \dots Other graphical parameters passed onto \code{lattice:xyplot}
##'   and \code{cutData}. For example, \code{polarFreq} passes the option
##'   \code{hemisphere = "southern"} on to \code{cutData} to provide southern
##'   (rather than default northern) hemisphere handling of \code{type = "season"}.
##'   Similarly, common axis and title labelling options (such as \code{xlab},
##'   \code{ylab}, \code{main}) are passed to \code{xyplot} via \code{quickText}
##'   to handle routine formatting.
##' @export
##' @return As well as generating the plot itself, \code{polarFreq} also
##'   returns an object of class \dQuote{openair}. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- polarFreq(mydata, "nox")}, this output can be used to
##'   recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}.
##' @author David Carslaw
##' @seealso See Also as \code{\link{windRose}}, \code{\link{polarPlot}}
##' @references ~put references to the literature/web site here ~
##' @keywords methods
##' @examples
##'
##'
##' # basic wind frequency plot
##' polarFreq(mydata)
##'
##' # wind frequencies by year
##' \dontrun{polarFreq(mydata, type = "year")}
##'
##'
##' # mean SO2 by year, showing only bins with at least 2 points
##' \dontrun{polarFreq(mydata, pollutant = "so2", type = "year", statistic = "mean", min.bin = 2)}
##'
##' # weighted mean SO2 by year, showing only bins with at least 2 points
##' \dontrun{polarFreq(mydata, pollutant = "so2", type = "year", statistic = "weighted.mean",
##' min.bin = 2)}
##'
##' #windRose for just 2000 and 2003 with different colours
##' \dontrun{polarFreq(subset(mydata, format(date, "%Y") %in% c(2000, 2003)),
##' type = "year", cols = "jet")}
##'
##' # user defined breaks from 0-700 in intervals of 100 (note linear scale)
##' \dontrun{polarFreq(mydata, breaks = seq(0, 700, 100))}
##'
##' # more complicated user-defined breaks - useful for highlighting bins
##' # with a certain number of data points
##' \dontrun{polarFreq(mydata, breaks = c(0, 10, 50, 100, 250, 500, 700))}
##'
##' # source contribution plot and use of offset option
##' \dontrun{polarFreq(mydata, pollutant = "pm25", statistic
##' ="weighted.mean", offset = 50, ws.int = 25, trans = FALSE) }
##'
polarFreq <- function(mydata,
                      pollutant = "",
                      statistic = "frequency",
                      ws.int = 1,
                      grid.line = 5,
                      breaks = seq(0, 5000, 500),
                      cols = "default",
                      trans = TRUE,
                      type = "default",
                      min.bin = 1,
                      ws.upper = NA,
                      offset = 10,
                      border.col = "transparent",
                      key.header = statistic,
                      key.footer = pollutant,
                      key.position = "right",
                      key = TRUE,
                      auto.text = TRUE,...) {



    ## extract necessary data
    vars <- c("wd", "ws")
    if (any(type %in%  dateTypes)) vars <- c(vars, "date")

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip,
                            fontsize = current.font))

    ##extra.args setup
    extra.args <- list(...)

    #label controls
    extra.args$xlab <- if ("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else quickText("", auto.text)
    extra.args$ylab <- if ("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else quickText("", auto.text)
    extra.args$main <- if ("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)

    if ("fontsize" %in% names(extra.args))
        trellis.par.set(fontsize = list(text = extra.args$fontsize))
    
    if (!missing(pollutant)) vars <- c(vars, pollutant)

    ## data checks
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    ## to make first interval easier to work with, set ws = 0 + e
    ids <- which(mydata$ws == 0)
     mydata$ws[ids] <-  mydata$ws[ids] + 0.0001

    ## remove all NAs
    mydata <- na.omit(mydata)

    mydata <- cutData(mydata, type, ...)

    ## if pollutant chosen but no statistic - use mean, issue warning
    if (!missing(pollutant) & missing(statistic)) {
        statistic <- "mean"
        warning("No statistic chosen, using mean")
    }

    ## if statistic chosen but no pollutant stop
    if (!missing(statistic) & missing(pollutant)) {
        stop("No pollutant chosen, please choose one e.g. pollutant = 'nox'")
    }

    if (!missing(breaks)) trans <- FALSE  ## over-ride transform if breaks supplied

    if (missing(key.header)) key.header <- statistic
    if (key.header == "weighted.mean") key.header <- c("contribution", "(%)")

    ## apply square root transform?
    if (trans) coef <- 2 else coef <- 1

    ## set the upper wind speed
    if (is.na(ws.upper)) {
        max.ws <- max(mydata$ws, na.rm = TRUE)
    } else {
        max.ws <- ws.upper
    }

    ## offset for "hollow" middle
    offset <- (max.ws * offset) / 5 / 10

    ## make sure wd data are rounded to nearest 10
    mydata$wd <- 10 * ceiling(mydata$wd / 10 - 0.5)

    prepare.grid <- function(mydata)
    {
        wd <- factor(mydata$wd)
        ws <- factor(ws.int * ceiling(mydata$ws / ws.int))

        if (statistic == "frequency")     ## case with only ws and wd
        {
            weights <- tapply(mydata$ws, list(wd, ws), function(x) length(na.omit(x)))}

        if (statistic == "mean")
        {
            weights <- tapply(mydata[[pollutant]],
                              list(wd, ws), function(x) mean(x, na.rm = TRUE))}

        if (statistic == "median")
        {
            weights <- tapply(mydata[[pollutant]],
                              list(wd, ws), function(x) median(x, na.rm = TRUE))}

        if (statistic == "max")
        {
            weights <- tapply(mydata[[pollutant]],
                              list(wd, ws), function(x) max(x, na.rm = TRUE))}

        if (statistic == "stdev")
        {
            weights <- tapply(mydata[[pollutant]],
                              list(wd, ws), function(x) sd(x, na.rm = TRUE))}

        if (statistic == "weighted.mean")
        {
            weights <- tapply(mydata[[pollutant]], list(wd, ws),
                              function(x) (mean(x) * length(x) / nrow(mydata)))

            ## note sum for matrix
            weights <- 100 * weights / sum(sum(weights, na.rm = TRUE))

        }

        weights <- as.vector(t(weights))

        ## frequency - remove points with freq < min.bin
        bin.len <- tapply(mydata$ws, list(wd, ws), function(x) length(na.omit(x)))
        binned.len <- as.vector(t(bin.len))
        ids <- which(binned.len < min.bin)
        weights[ids] <- NA

        ws.wd <- expand.grid(ws = as.numeric(levels(ws)), wd = as.numeric(levels(wd)))

        weights <- cbind(ws.wd, weights)
        weights
    }


    poly <- function(dir, speed, colour)
    {

        ## offset by 3 * ws.int so that centre is not compressed
        angle <- seq(dir - 5, dir + 5, length = 10)
        x1 <- (speed + offset - ws.int) * sin(pi * angle / 180)
        y1 <- (speed + offset - ws.int) * cos(pi * angle / 180)
        x2 <- rev((speed + offset) * sin(pi * angle / 180))
        y2 <- rev((speed + offset) * cos(pi * angle / 180))
        lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border.col, lwd = 0.5)
    }

  
    results.grid <- group_by_(mydata, .dots = type) %>%
      do(prepare.grid(.))
    
    results.grid <- na.omit(results.grid)

    ## proper names of labelling ###################################################
    strip.dat <- strip.fun(results.grid, type, auto.text)
    strip <- strip.dat[[1]]
    strip.left <- strip.dat[[2]]
    pol.name <- strip.dat[[3]]

    results.grid$weights <- results.grid$weights ^ (1 / coef)

    nlev <- 200
    ## handle missing breaks arguments
    if(missing(breaks)) {

        breaks <- unique(c(0, pretty(results.grid$weights, nlev)))
        br <- pretty((c(0, results.grid$weights) ^ coef), n = 10)  ## breaks for scale

    } else {

        br <- breaks

    }

    nlev2 <- length(breaks)

    col <- openColours(cols, (nlev2 - 1))

    results.grid$div <- cut(results.grid$weights, breaks, include.lowest = TRUE)

    ## for pollution data
    results.grid$weights[results.grid$weights == "NaN"] <- 0
    results.grid$weights[which(is.na(results.grid$weights))] <- 0


    ##  scale key setup ################################################################################################
    legend <- list(col = col[1:(length(breaks) - 1)], at = breaks,
                   labels = list(at = br ^ (1 / coef), labels = br),
                   space = key.position,
                   auto.text = auto.text, footer = key.footer, header = key.header,
                   height = 1, width = 1.5, fit = "all")
    legend <- makeOpenKeyLegend(key, legend, "polarFreq")

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("ws ~ wd | ", temp, sep = ""))

    span <- ws.int * floor (max.ws / ws.int) + ws.int + offset

    xyplot.args <- list(x = myform,
                  xlim = 1.03 * c(-span, span),
                  ylim = 1.03 * c(-span, span),
                  data = results.grid,
                  par.strip.text = list(cex = 0.8),
                  type = "n",
                  strip = strip,
                  strip.left = strip.left,
                  as.table = TRUE,
                  aspect = 1,
                  scales = list(draw = FALSE),

                  panel = function(x, y, subscripts,...) {
                      panel.xyplot(x, y,...)

                      subdata <- results.grid[subscripts, ]

                      for (i in 1:nrow(subdata)) {
                          colour <- col[as.numeric(subdata$div[i])]
                       #   if (subdata$weights[i] == 0) colour <- "transparent"
                          poly(subdata$wd[i], subdata$ws[i], colour)
                      }

                      ## annotate
                      if (ws.int < max.ws) { ## don't annotate if only 1 interval
                          angles <- seq(0, 2 * pi, length = 360)
                          sapply(seq(0, 20 * grid.line, by = grid.line), function(x)
                                 llines((offset + x) * sin(angles),
                                        (offset + x) * cos(angles),
                                        col = "grey", lty = 5))

                          ## radial labels
                          sapply(seq(0, 20 * grid.line, by = grid.line), function(x)
                                 ltext((offset + x) * sin(pi / 4), (offset + x) * cos(pi / 4),
                                       x, cex = 0.7))
                      }

                      larrows(-span, 0,  -offset, 0, code = 1, length = 0.1)
                      larrows(span, 0,  offset, 0, code = 1, length = 0.1)
                      larrows(0, -span, 0, -offset, code = 1, length = 0.1)
                      larrows(0, span, 0, offset, code = 1, length = 0.1)

                      ltext(-span * 0.95, 0.07 * span, "W", cex = 0.7)
                      ltext(0.07 * span, -span  * 0.95, "S", cex = 0.7)
                      ltext(0.07 * span, span * 0.95, "N", cex = 0.7)
                      ltext(span * 0.95, 0.07 * span, "E", cex = 0.7)

                  },
                  legend = legend
                  )

    #reset for extra.args
    xyplot.args<- listUpdate(xyplot.args, extra.args)

    #plot
    plt <- do.call(xyplot, xyplot.args)


#################
    ## output
#################
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    invisible(output)


}
