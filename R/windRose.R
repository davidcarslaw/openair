pollutionRose <- function(mydata, pollutant = "nox", key.footer = pollutant,
                          key.position = "right", key = TRUE,
                          breaks = 6, paddle = FALSE, seg = 0.9, normalise = FALSE, 
                          ...)
{

    ## extra args setup
    extra <- list(...)

    ## check to see if two met data sets are being compared.
    ## if so, set pollutant to one of the names
    if ("ws2" %in% names(extra)) {
        pollutant <-  extra$ws
        if (missing(breaks)) breaks <- NA
    }

    if (is.null(breaks))  breaks <- 6

    if (is.numeric(breaks) & length(breaks) == 1) {

        ## breaks from the minimum to 90th percentile, which generally gives sensible
        ## spacing for skewed data. Maximum is added later.
        breaks <- unique(pretty(c(min(mydata[[pollutant]], na.rm = TRUE),
                                  quantile(mydata[[pollutant]], probs = 0.9, na.rm = TRUE),
                                  breaks)))

    }

    windRose(mydata, pollutant = pollutant, paddle = paddle, seg = seg,
             key.position = key.position, key.footer = key.footer, key = key,
             breaks = breaks, normalise = normalise, ...)
}




##' Traditional wind rose plot and pollution rose variation
##'
##' The traditional wind rose plot that plots wind speed and wind direction by
##' different intervals. The pollution rose applies the same plot structure but
##' substitutes other measurements, most commonly a pollutant time series, for
##' wind speed.
##'
##' For \code{windRose} data are summarised by direction, typically by 45 or 30
##' (or 10) degrees and by different wind speed categories. Typically, wind
##' speeds are represented by different width "paddles". The plots show the
##' proportion (here represented as a percentage) of time that the wind is from
##' a certain angle and wind speed range.
##'
##' By default \code{windRose} will plot a windRose in using "paddle" style
##' segments and placing the scale key below the plot.
##'
##' The argument \code{pollutant} uses the same plotting structure but
##' substitutes another data series, defined by \code{pollutant}, for wind
##' speed.
##'
##' The option \code{statistic = "prop.mean"} provides a measure of the
##' relative contribution of each bin to the panel mean, and is intended for
##' use with \code{pollutionRose}.
##'
##' \code{pollutionRose} is a \code{windRose} wrapper which brings
##' \code{pollutant} forward in the argument list, and attempts to sensibly
##' rescale break points based on the \code{pollutant} data range by by-passing
##' \code{ws.int}.
##'
##' By default, \code{pollutionRose} will plot a pollution rose of \code{nox}
##' using "wedge" style segments and placing the scale key to the right of the
##' plot.
##'
##' It is possible to compare two wind speed-direction data sets using
##' \code{pollutionRose}. There are many reasons for doing so e.g. to
##' see how one site compares with another or for meteorological model
##' evaluation. In this case, \code{ws} and \code{wd} are considered
##' to the the reference data sets with which a second set of wind
##' speed and wind directions are to be compared (\code{ws2} and
##' \code{wd2}). The first set of values is subtracted from the second
##' and the differences compared. If for example, \code{wd2} was
##' biased positive compared with \code{wd} then \code{pollutionRose}
##' will show the bias in polar coordinates. In its default use, wind
##' direction bias is colour-coded to show negative bias in one colour
##' and positive bias in another.
##'
##' @usage windRose(mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA,
##' ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols = "default",
##' grid.line = NULL, width = 1, seg = NULL, auto.text = TRUE, breaks
##' = 4, offset = 10, normalise = FALSE, max.freq = NULL, paddle = TRUE, key.header =
##' NULL, key.footer = "(m/s)", key.position = "bottom", key = TRUE,
##' dig.lab = 5, statistic = "prop.count", pollutant = NULL, annotate
##' = TRUE, angle.scale = 315, border = NA, ...)
##'
##'
##'     pollutionRose(mydata, pollutant = "nox", key.footer = pollutant,
##'        key.position = "right", key = TRUE, breaks = 6, paddle = FALSE,
##' seg = 0.9, normalise = FALSE, ...)
##'
##'
##' @aliases windRose pollutionRose
##' @param mydata A data frame containing fields \code{ws} and \code{wd}
##' @param ws Name of the column representing wind speed.
##' @param wd Name of the column representing wind direction.
##' @param ws2 The user can supply a second set of wind speed and wind
##' direction values with which the first can be compared. See details
##' below for full explanation.
##' @param wd2 see \code{ws2}.
##' @param ws.int The Wind speed interval. Default is 2 m/s but for low met
##'   masts with low mean wind speeds a value of 1 or 0.5 m/s may be better.
##'   Note, this argument is superseded in \code{pollutionRose}. See
##'   \code{breaks} below.
##' @param angle Default angle of \dQuote{spokes} is 30. Other potentially useful
##'   angles are 45 and 10. Note that the width of the wind speed interval may
##'   need adjusting using \code{width}.
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. \dQuote{season},
##' \dQuote{year}, \dQuote{weekday} and so on. For example, \code{type
##' = "season"} will produce four plots --- one for each season.
##'
##' It is also possible to choose \code{type} as another variable in the data
##'   frame. If that variable is numeric, then the data will be split into four
##'   quantiles (if possible) and labelled accordingly. If type is an existing
##'   character or factor variable, then those categories/levels will be used
##'   directly. This offers great flexibility for understanding the variation
##'   of different variables and how they depend on one another.
##'
##' Type can be up length two e.g. \code{type = c("season", "weekday")} will
##'   produce a 2x2 plot split by season and day of the week. Note, when two
##'   types are provided the first forms the columns and the second the rows.
##' @param bias.corr When \code{angle} does not divide exactly into
##' 360 a bias is introduced in the frequencies when the wind
##' direction is already supplied rounded to the nearest 10 degrees,
##' as is often the case. For example, if \code{angle = 22.5}, N, E,
##' S, W will include 3 wind sectors and all other angles will be
##' two. A bias correction can made to correct for this problem. A
##' simple method according to Applequist (2012) is used to adjust the
##' frequencies.
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet},
##' \dQuote{hue} and user defined. For user defined the user can
##' supply a list of colour names recognised by R (type
##' \code{colours()} to see the full list). An example would be
##' \code{cols = c("yellow", "green", "blue", "black")}.
##' @param grid.line Grid line interval to use. If \code{NULL}, as in default,
##'   this is assigned by \code{windRose} based on the available data range.
##'   However, it can also be forced to a specific value, e.g.
##'   \code{grid.line = 10}.
##' @param width For \code{paddle = TRUE}, the adjustment factor for width of
##'   wind speed intervals. For example, \code{width = 1.5} will make the
##'   paddle width 1.5 times wider.
##' @param seg For \code{pollutionRose} \code{seg} determines with
##' width of the segments. For example, \code{seg = 0.5} will produce
##' segments 0.5 * \code{angle}.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##' \code{TRUE} titles and axis labels will automatically try and
##' format pollutant names and units properly e.g.  by subscripting
##' the \sQuote{2} in NO2.
##' @param breaks Most commonly, the number of break points for wind
##' speed in \code{windRose} or pollutant in \code{pollutionRose}. For
##' \code{windRose} and the \code{ws.int} default of 2 m/s, the
##' default, 4, generates the break points 2, 4, 6, 8 m/s. For
##' \code{pollutionRose}, the default, 6, attempts to breaks the
##' supplied data at approximately 6 sensible break points. However,
##' \code{breaks} can also be used to set specific break points. For
##' example, the argument \code{breaks = c(0, 1, 10, 100)} breaks the
##' data into segments <1, 1-10, 10-100, >100.
##' @param offset The size of the 'hole' in the middle of the plot, expressed
##'   as a percentage of the polar axis scale, default 10.
##' @param normalise If \code{TRUE} each wind direction segment of a
##' pollution rose is normalised to equal one. This is useful for
##' showing how the concentrations (or other parameters) contribute to
##' each wind sector when the proprtion of time the wind is from that
##' direction is low. A line showing the probability that the wind
##' directions is from a particular wind sector is also shown.
##' @param max.freq Controls the scaling used by setting the maximum
##' value for the radial limits. This is useful to ensure several
##' plots use the same radial limits.
##' @param paddle Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
##'   plots rose using `paddle' style spokes. If \code{FALSE} plots rose using
##'   `wedge' style spokes.
##' @param key.header Adds additional text/labels above and/or below
##' the scale key, respectively. For example, passing
##' \code{windRose(mydata, key.header = "ws")} adds the addition text
##' as a scale header. Note: This argument is passed to
##' \code{drawOpenKey} via \code{quickText}, applying the auto.text
##' argument, to handle formatting.
##' @param key.footer see \code{key.footer}.
##' @param key.position Location where the scale key is to plotted.
##' Allowed arguments currently include \dQuote{top},
##' \dQuote{right}, \dQuote{bottom} and \dQuote{left}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{drawOpenKey} for further details.
##' @param dig.lab The number of signficant figures at which scientific number
##'   formatting is used in break point and key labelling. Default 5.
##' @param statistic The \code{statistic} to be applied to each data
##' bin in the plot. Options currently include \dQuote{prop.count},
##' \dQuote{prop.mean} and \dQuote{abs.count}. The default
##' \dQuote{prop.count} sizes bins according to the proportion of
##' the frequency of measurements.  Similarly, \dQuote{prop.mean} sizes
##' bins according to their relative contribution to the
##' mean. \dQuote{abs.count} provides the absolute count of
##' measurements in each bin.
##' @param pollutant Alternative data series to be sampled instead of wind
##'   speed. The \code{windRose} default NULL is equivalent to \code{pollutant
##'   = "ws"}.
##' @param annotate If \code{TRUE} then the percentage calm and mean
##' values are printed in each panel together with a description of
##' the statistic below the plot.
##' @param angle.scale The wind speed scale is by default shown at a
##' 315 degree angle. Sometimes the placement of the scale may
##' interfere with an interesting feature. The user can therefore set
##' \code{angle.scale} to another value (between 0 and 360 degrees) to
##' mitigate such problems. For example \code{angle.scale = 45} will
##' draw the scale heading in a NE direction.
##' @param border Border colour for shaded areas. Default is no border.
##' @param ... For \code{pollutionRose} other parameters that are
##' passed on to \code{windRose}. For \code{windRose} other parameters
##' that are passed on to \code{drawOpenKey}, \code{lattice:xyplot}
##' and \code{cutData}. Axis and title labelling options (\code{xlab},
##' \code{ylab}, \code{main}) are passed to \code{xyplot} via
##' \code{quickText} to handle routine formatting.
##'
##' @export windRose pollutionRose
##' @import dplyr
##' @import lazyeval
##' @importFrom plyr ddply ldply dlply llply numcolwise . 
##' @importFrom graphics abline
##' @importFrom grDevices col2rgb colorRampPalette grey rgb xy.coords
##' @importFrom methods is
##' @importFrom stats aggregate approx as.dendrogram as.dist
##' ave coef cor dist formula hclust lm median
##' na.omit optimize order.dendrogram predict qchisq qnorm
##' qt quantile reshape sd smooth.spline spline stl ts
##' update var
##' @importFrom utils compareVersion modifyList packageDescription
##' read.csv read.table
##' @return As well as generating the plot itself, \code{windRose} and 
##'   \code{pollutionRose} also return an object of class \dQuote{openair}. The
##'   object includes three main components: \code{call}, the command used to
##'   generate the plot; \code{data}, the data frame of summarised information
##'   used to make the plot; and \code{plot}, the plot itself. If retained, e.g.
##'   using \code{output <- windRose(mydata)}, this output can be used to
##' recover the data, reproduce or rework the original plot or
##' undertake further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summarise}.
##'
##' Summarised proportions can also be extracted directly using the
##'   \code{$data} operator, e.g.  \code{object$data} for \code{output <-
##'   windRose(mydata)}. This returns a data frame with three set columns:
##'   \code{cond}, conditioning based on \code{type}; \code{wd}, the wind
##'   direction; and \code{calm}, the \code{statistic} for the proportion of
##'   data unattributed to any specific wind direction because it was collected
##'   under calm conditions; and then several (one for each range binned for
##'   the plot) columns giving proportions of measurements associated with each
##'   \code{ws} or \code{pollutant} range plotted as a discrete panel.
##' @note \code{windRose} and \code{pollutionRose} both use \link{drawOpenKey}
##'   to produce scale keys.
##' @author David Carslaw (with some additional contributions by Karl Ropkins)
##' @seealso See \code{\link{drawOpenKey}} for fine control of the scale key.
##'
##' See \code{\link{polarFreq}} for a more flexible version that considers
##'   other statistics and pollutant concentrations.
##' @keywords methods
##' @references
##'
##' Applequist, S, 2012: Wind Rose Bias
##' Correction. J. Appl. Meteor. Climatol., 51, 1305-1309.
##'
##' This paper seems to be the original?
##'
##' Droppo,  J.G. and B.A. Napier (2008) Wind Direction Bias in
##' Generating Wind Roses and Conducting Sector-Based Air Dispersion
##' Modeling, Journal of the Air & Waste Management Association, 58:7, 913-918.
##'
##' @examples
##'
##' # load example data from package data(mydata)
##'
##' # basic plot
##' windRose(mydata)
##'
##' # one windRose for each year
##' windRose(mydata,type = "year")
##'
##' # windRose in 10 degree intervals with gridlines and width adjusted
##' \dontrun{
##' windRose(mydata, angle = 10, width = 0.2, grid.line = 1)
##' }
##'
##' # pollutionRose of nox
##' pollutionRose(mydata, pollutant = "nox")
##'
##' ## source apportionment plot - contribution to mean
##' \dontrun{
##' pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")
##' }
##'
##' ## example of comparing 2 met sites
##' ## first we will make some new ws/wd data with a postive bias
##' mydata$ws2 = mydata$ws + 2 * rnorm(nrow(mydata)) + 1
##' mydata$wd2 = mydata$wd + 30 * rnorm(nrow(mydata)) + 30
##'
##' ## need to correct negative wd
##' id <- which(mydata$wd2 < 0)
##' mydata$wd2[id] <- mydata$wd2[id] + 360
##'
##' ## results show postive bias in wd and ws
##' pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")
windRose <- function (mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA,
                      ws.int = 2, angle = 30, type = "default", bias.corr = TRUE,
                      cols = "default", grid.line = NULL, width = 1, seg = NULL,
                      auto.text = TRUE, breaks = 4, offset = 10, normalise = FALSE, 
                      max.freq = NULL, paddle = TRUE, key.header = NULL,
                      key.footer = "(m/s)", key.position = "bottom",
                      key = TRUE, dig.lab = 5, statistic = "prop.count",
                      pollutant = NULL, annotate = TRUE, angle.scale = 315, border = NA,
                      ...)
{

    if (is.null(seg)) seg <- 0.9

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
        ## other local colours
        calm.col <- "black"
    } else {
        calm.col <- "forestgreen"
    }
    
    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip,
                            fontsize = current.font))
    
    # make sure ws and wd and numeric
    mydata <- checkNum(mydata, vars = c(ws, wd))

    if (360 / angle != round(360 / angle)) {
        warning("In windRose(...):\n  angle will produce some spoke overlap",
                "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.", call. = FALSE)
    }
    if (angle < 3) {
        warning("In windRose(...):\n  angle too small",
                "\n  enforcing 'angle = 3'", call. = FALSE)
        angle <- 3
    }

    ## extra args setup
    extra <- list(...)

    ## label controls
    extra$xlab <- if("xlab" %in% names(extra))
                      quickText(extra$xlab, auto.text) else quickText("", auto.text)
    extra$ylab <- if("ylab" %in% names(extra))
                      quickText(extra$ylab, auto.text) else quickText("", auto.text)
    extra$main <- if("main" %in% names(extra))
                      quickText(extra$main, auto.text) else quickText("", auto.text)

    if ("fontsize" %in% names(extra))
        trellis.par.set(fontsize = list(text = extra$fontsize))
    
    rounded <- FALSE ## is the wd already rounded to 10 degrees, if so need to correct bias later
    if (all(mydata[[wd]] %% 10 == 0, na.rm = TRUE)) rounded <- TRUE
    
    ## preset statitistics

    if (is.character(statistic)) {
        ## allowed cases
        ok.stat <- c("prop.count", "prop.mean", "abs.count", "frequency")

        if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
            warning("In windRose(...):\n  statistic unrecognised",
                    "\n  enforcing statistic = 'prop.count'", call. = FALSE)
            statistic <- "prop.count"
        }

        if (statistic == "prop.count"){
            stat.fun <- length
            stat.unit <- "%"
            stat.scale <- "all"
            stat.lab <- "Frequency of counts by wind direction (%)"
            stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE), 3)
            stat.lab2 <- "mean"
            stat.labcalm <- function(x) round(x, 1)
        }

        if (statistic == "prop.mean") {
            stat.fun <- function(x) sum(x, na.rm = TRUE)
            stat.unit <- "%"
            stat.scale <- "panel"
            stat.lab <- "Proportion contribution to the mean (%)"
            stat.fun2 <- function(x) signif(mean(x, na.rm = TRUE), 3)
            stat.lab2 <- "mean"
            stat.labcalm <- function(x) round(x, 1)
        }

        if (statistic == "abs.count" | statistic == "frequency") {
            stat.fun <- length
            stat.unit <- ""
            stat.scale <- "none"
            stat.lab <- "Count by wind direction"
            stat.fun2 <- function(x) round(length(x), 0)
            stat.lab2 <- "count"
            stat.labcalm <- function(x) round(x, 0)
        }

    }

    if (is.list(statistic)) {

        ## IN DEVELOPMENT

        ## this section has no testing/protection
        ## but allows users to supply a function
        ## scale it by total data or panel
        ## convert proportions to percentage
        ## label it

        stat.fun <- statistic$fun
        stat.unit <- statistic$unit
        stat.scale <- statistic$scale
        stat.lab <- statistic$lab
        stat.fun2 <- statistic$fun2
        stat.lab2 <- statistic$lab2
        stat.labcalm <- statistic$labcalm
    }

    ## variables we need
    vars <- c(wd, ws)

    diff <- FALSE ## i.e. not two sets of ws/wd
    rm.neg <- TRUE ## will remove negative ws in check.prep

    ## case where two met data sets are to be compared
    if (!is.na(ws2) & !is.na(wd2)) {
        vars <- c(vars, ws2, wd2)
        diff <- TRUE
        rm.neg <- FALSE
        mydata$ws <- mydata[, ws2] - mydata[, ws]
        mydata$wd <- mydata[, wd2] - mydata[[wd]]

        ## fix negative wd
        id <- which(mydata$wd < 0)
        if (length(id) > 0) mydata$wd[id] <- mydata$wd[id] + 360

        pollutant <- "ws"
        key.footer <- "ws"
        wd <- "wd" ; ws <- "ws"
        vars <- c("ws", "wd")
        if (missing(angle)) angle <- 10
        if (missing(offset)) offset <- 20
        ## set the breaks to cover all the data
        if (is.na(breaks[1])) {
            max.br <- max(ceiling(abs(c(min(mydata$ws, na.rm = TRUE),
                                        max(mydata$ws, na.rm = TRUE)))))
            breaks <- c(-1 * max.br, 0, max.br)
        }

        if (missing(cols)) cols <- c("lightskyblue", "tomato")
        seg <- 1
    }

    if (any(type %in% dateTypes)) vars <- c(vars, "date")

    if (!is.null(pollutant)) vars <- c(vars, pollutant)

    mydata <- cutData(mydata, type, ...)
    

    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE, remove.neg = rm.neg)
  
    # remove lines where ws is missing
    # wd can be NA and ws 0 (calm)
    id <- which(is.na(mydata[[ws]]))
    
    if (length(id) > 0)
    mydata <- mydata[-id, ]

    if (is.null(pollutant)) pollutant <- ws

    mydata$x <- mydata[[pollutant]]

    mydata[[wd]] <- angle * ceiling(mydata[[wd]] / angle - 0.5)
    mydata[[wd]][mydata[[wd]] == 0] <- 360

    ## flag calms as negatives
    mydata[[wd]][mydata[ , ws] == 0] <- -999 ## set wd to flag where there are calms
    ## do after rounding or -999 changes

    if (length(breaks) == 1) breaks <- 0:(breaks - 1) * ws.int
    
    if (max(breaks) < max(mydata$x, na.rm = TRUE))
        breaks <- c(breaks, max(mydata$x, na.rm = TRUE))

    if (min(breaks) > min(mydata$x, na.rm = TRUE))
        warning ("Some values are below minimum break.")

    breaks <- unique(breaks)
    mydata$x <- cut(mydata$x, breaks = breaks, include.lowest = FALSE,
                    dig.lab = dig.lab)

    ## clean up cut intervals
    labs <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$x))
    labs <- gsub("[,]", " to ", labs)



    ## statistic handling

    prepare.grid <- function(mydata) {
        
        ## these are all calms...
        if (all(is.na(mydata$x))) {
            
            weights <- data.frame(Interval1 = NA, wd = NA,
                                  calm = 100, panel.fun = NA, mean.wd = NA, freqs = NA)
            
        } else {
            
            levels(mydata$x) <- c(paste("Interval", 1:length(labs), sep = ""))
            
            all <- stat.fun(mydata[[wd]])
            calm <- mydata[mydata[[wd]] == -999, ][[pollutant]]

            calm <- stat.fun(calm)

            weights <- tapply(mydata[[pollutant]], list(mydata[[wd]], mydata$x),
                              stat.fun)
            freqs <- tapply(mydata[[pollutant]], mydata[[wd]], length)

            ## scaling
            if (stat.scale == "all") {
                calm <- calm / all
                weights <- weights / all
            }

            if (stat.scale == "panel") {
                temp <- stat.fun(stat.fun(weights)) + calm
                calm <- calm / temp
                weights <- weights / temp
            }

            weights[is.na(weights)] <- 0
            weights <- t(apply(weights, 1, cumsum))

            if (stat.scale == "all" | stat.scale == "panel"){
                weights <- weights * 100
                calm <- calm * 100
            }

            panel.fun <- stat.fun2(mydata[[pollutant]])

            ## calculate mean wd - useful for cases comparing two met data sets
            u <- mean(sin(2 * pi * mydata[[wd]] / 360))
            v <- mean(cos(2 * pi * mydata[[wd]] / 360))
            mean.wd <- atan2(u, v) * 360 / 2 / pi

            if (all(is.na(mean.wd))) {
                mean.wd <- NA
            } else {
                if (mean.wd < 0) mean.wd <- mean.wd + 360
                ## show as a negative (bias)
                if (mean.wd > 180) mean.wd <- mean.wd - 360
            }


            weights <- cbind(data.frame(weights), wd = as.numeric(row.names(weights)),
                             calm = calm, panel.fun = panel.fun, mean.wd = mean.wd, freqs = freqs)

            

        }

        weights
    }

    if (paddle) {

        poly <- function(wd, len1, len2, width, colour, x.off = 0, y.off = 0) {

            theta <- wd * pi / 180
            len1 <- len1 + off.set
            len2 <- len2 + off.set
            x1 <- len1 * sin(theta) - width * cos(theta) + x.off
            x2 <- len1 * sin(theta) + width * cos(theta) + x.off
            x3 <- len2 * sin(theta) - width * cos(theta) + x.off
            x4 <- len2 * sin(theta) + width * cos(theta) + x.off
            y1 <- len1 * cos(theta) + width * sin(theta) + y.off
            y2 <- len1 * cos(theta) - width * sin(theta) + y.off
            y3 <- len2 * cos(theta) + width * sin(theta) + y.off
            y4 <- len2 * cos(theta) - width * sin(theta) + y.off
            lpolygon(c(x1, x2, x4, x3), c(y1, y2, y4, y3), col = colour,
                     border = border)
        }

    } else {

        poly <- function(wd, len1, len2, width, colour, x.off = 0,
                         y.off = 0) {
            
            len1 <- len1 + off.set
            len2 <- len2 + off.set

            theta <- seq((wd - seg * angle / 2), (wd + seg * angle / 2),
                         length.out = (angle - 2) * 10)
            theta <- ifelse(theta < 1, 360 - theta, theta)
            theta <- theta * pi / 180
            x1 <- len1 * sin(theta) + x.off
            x2 <- rev(len2 * sin(theta) + x.off)
            y1 <- len1 * cos(theta) + x.off
            y2 <- rev(len2 * cos(theta) + x.off)
            lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
        }
    }

    
    results <- group_by_(mydata, .dots = type) %>%
      do(prepare.grid(.))
    
    ## format
    results$calm <- stat.labcalm(results$calm)
    results$mean.wd <- stat.labcalm(results$mean.wd)

    ## correction for bias when angle does not divide exactly into 360
    if (bias.corr & rounded) {
        wd <- seq(10, 360, 10)
        tmp <- angle * ceiling(wd / angle - 0.5)
        id <- which(tmp == 0)
        if (length(id > 0)) tmp[id] <- 360
        tmp <- table(tmp) ## number of sectors spanned
        vars <- grep("Interval[1-9]", names(results)) ## the frequencies
        results[-1, vars] <- results[-1, vars] * mean(tmp) /tmp
    }

    ## proper names of labelling###########################################
    strip.dat <- strip.fun(results, type, auto.text)
    strip <- strip.dat[[1]]
    strip.left <- strip.dat[[2]]
    pol.name <- strip.dat[[3]]

    if (length(labs) < length(cols)) {
        col <- cols[1:length(labs)]
    } else {
        col <- openColours(cols, length(labs))
    }

    ## normalise by sector
    
    if (normalise) {
        
        vars <- grep("Interval[1-9]", names(results))
        
        ## original frequencies, so we can plot the wind frequency line
        results$freq <- results[[max(vars)]]

        results$freq <- ave(results$freq, results[type], FUN = function(x) x / sum(x))

        ## scale by maximum frequency
        results$norm <- results$freq / max(results$freq)

        ## normalise
        results[, vars] <- results[, vars] / results[[max(vars)]]

        stat.lab <- "Normalised by wind sector"
        stat.unit <- ""

    }
    
    if (is.null(max.freq)) {
        max.freq <- max(results[, (length(type) + 1):(length(labs) +
                                                             length(type))],
                        na.rm = TRUE)
    } else {
        max.freq <- max.freq
    }

    off.set <- max.freq * (offset / 100)
    box.widths <- seq(0.002 ^ 0.25, 0.016 ^ 0.25,
                      length.out = length(labs)) ^ 4
    box.widths <- box.widths * max.freq * angle / 5

    ## key, colorkey, legend
    legend <- list(col = col, space = key.position, auto.text = auto.text,
                   labels = labs, footer = key.footer, header = key.header,
                   height = 0.60, width = 1.5, fit = "scale",
                   plot.style = if (paddle) "paddle" else "other")

    legend <- makeOpenKeyLegend(key, legend, "windRose")


    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Interval1 ~ wd | ", temp, sep = ""))

    mymax <- 2 * max.freq
    myby <- if (is.null(grid.line)) pretty(c(0, mymax), 10)[2] else grid.line

    if (myby / mymax > 0.9) myby <- mymax * 0.9

    if (annotate) sub <- stat.lab else sub <- NULL

    

    xy.args <- list(x = myform,
                    xlim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
                    ylim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
                    data = results,
                    type = "n",
                    sub = sub,
                    strip = strip,
                    strip.left = strip.left,
                    as.table = TRUE,
                    aspect = 1,
                    par.strip.text = list(cex = 0.8),
                    scales = list(draw = FALSE),

                    panel = function(x, y, subscripts, ...) {
                        panel.xyplot(x, y, ...)
                        angles <- seq(0, 2 * pi, length = 360)
                        sapply(seq(off.set, mymax, by = myby),
                               function(x) llines(x * sin(angles), x * cos(angles),
                                                  col = "grey85", lwd = 1))

                        dat <- results[subscripts, ] ## subset of data
                        upper <- max.freq + off.set

                        ## add axis lines
                        larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
                        larrows(0, -upper, 0, upper, code = 3, length = 0.1)

                        ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7)
                        ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7)
                        ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7)
                        ltext(upper * 0.95, 0.07 *upper, "E", cex = 0.7)

                        if (nrow(dat) > 0) {

                            dat$Interval0 <- 0 ## make a lower bound to refer to

                            for (i in 1:nrow(dat)) { ## go through wind angles 30, 60, ...

                                for (j in seq_along(labs)) { ## go through paddles x1, x2, ...

                                    tmp <- paste("poly(dat$wd[i], dat$Interval", j - 1,
                                                 "[i], dat$Interval", j, "[i], width * box.widths[",
                                                 j, "], col[", j, "])", sep = "")


                                    eval(parse(text = tmp))
                                }
                            }
                        }

                        if (normalise)
                            panel.wdprob(dat, seg, angle, off.set)

                        ltext(seq((myby + off.set), mymax, myby) * sin(pi * angle.scale / 180),
                              seq((myby + off.set), mymax, myby) * cos(pi * angle.scale / 180),
                              paste(seq(myby, mymax, by = myby), stat.unit,  sep = ""), cex = 0.7)

                        ## annotations e.g. calms, means etc
                        if (annotate) ## don't add calms for prop.mean for now...

                            if (!diff) {
                                ltext(max.freq + off.set, -max.freq - off.set,
                                      label = paste(stat.lab2, " = ",
                                          dat$panel.fun[1], "\ncalm = ",
                                          dat$calm[1], stat.unit, sep = ""),
                                      adj = c(1, 0), cex = 0.7, col = calm.col)
                            }
                        if (diff) { ## when two data sets are present
                            ltext(max.freq + off.set, -max.freq - off.set,
                                  label = paste("mean ws = ",
                                      round(dat$panel.fun[1], 1),
                                      "\nmean wd = ", round(dat$mean.wd[1], 1),
                                      sep = ""), adj = c(1, 0), cex = 0.7, col = calm.col)
                        }

                    }, legend = legend)

    ## reset for extra
    xy.args <- listUpdate(xy.args, extra)

    ## plot
    plt <- do.call(xyplot, xy.args)


    ## output ################################################################################

    if (length(type) == 1) {
        plot(plt)
    } else {
        plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)
        plot(plt)
    }


    newdata <- results

    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)

}

## adds a line showing probability wind direction is from a particular sector
## used when normalise = TRUE

panel.wdprob <- function(dat, seg, angle, off.set) {
    len1 <- off.set
    
    x.off <- 0; y.off <- 0

    makeline <- function(i, dat) {
        theta <- seq((dat$wd[i] - seg * angle / 2), (dat$wd[i] + seg * angle / 2),
                     length.out = (angle - 2) * 10)
        theta <- ifelse(theta < 1, 360 - theta, theta)
        theta <- theta * pi / 180
        x1 <- len1 * sin(theta) + x.off
        x2 <- rev((dat$norm[i] + off.set) * sin(theta) + x.off)
        y1 <- len1 * cos(theta) + x.off
        y2 <- rev((dat$norm[i] + off.set) * cos(theta) + x.off)
        lpolygon(c(x1, x2), c(y1, y2), col = "transparent", border = "black", lwd = 2)
    }

    lapply(1:nrow(dat), makeline, dat)
    
    
}

