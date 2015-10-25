##' Function to plot percentiles by wind direction
##'
##' \code{percentileRose} plots percentiles by wind direction with flexible
##' conditioning. The plot can display mutiple percentile lines or filled
##' areas.
##'
##' \code{percentileRose} calculates percentile levels of a pollutant and plots
##' them by wind direction. One or more percentile levels can be calculated and
##' these are displayed as either filled areas or as lines.
##'
##' The wind directions are rounded to the nearest 10 degrees,
##' consistent with surface data from the UK Met Office before a
##' smooth is fitted. The levels by wind direction are optionally
##' calculated using a cyclic smooth cubic spline using the option
##' \code{smooth}. If \code{smooth = FALSE} then the data are shown in
##' 10 degree sectors.
##'
##' The \code{percentileRose} function compliments other similar functions
##' including \code{\link{windRose}}, \code{\link{pollutionRose}},
##' \code{\link{polarFreq}} or \code{\link{polarPlot}}. It is most useful for
##' showing the distribution of concentrations by wind direction and often can
##' reveal different sources e.g. those that only affect high percentile
##' concentrations such as a chimney stack.
##'
##' Similar to other functions, flexible conditioning is available through the
##' \code{type} option. It is easy for example to consider multiple percentile
##' values for a pollutant by season, year and so on. See examples below.
##'
##' \code{percentileRose} also offers great flexibility with the scale used and
##' the user has fine control over both the range, interval and colour.
##'
##' @param mydata A data frame minimally containing \code{wd} and a numeric
##'   field to plot --- \code{pollutant}.
##' @param pollutant Mandatory. A pollutant name corresponding to a variable in
##'   a data frame should be supplied e.g. \code{pollutant = "nox"}. More than
##'   one pollutant can be supplied e.g. \code{pollutant = c("no2", "o3")}
##'   provided there is only one \code{type}.
##' @param wd Name of the wind direction field.
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
##' @param percentile The percentile value(s) to plot. Must be between
##' 0--100. If \code{percentile = NA} then only a mean line will be
##' shown.
##' @param smooth Should the wind direction data be smoothed using a
##' cyclic spline?
##' @param method When \code{method = "default"} the supplied
##' percentiles by wind direction are calculated. When \code{method =
##' "cpf"} the conditional probability function (CPF) is plotted and a
##' single (usually high) percentile level is supplied. The CPF is
##' defined as CPF = my/ny, where my is the number of samples in the
##' wind sector y with mixing ratios greater than the \emph{overall}
##' percentile concentration, and ny is the total number of samples in
##' the same wind sector (see Ashbaugh et al., 1985).
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
##' and \code{RColorBrewer} colours --- see the \code{openair}
##' \code{openColours} function for more details. For user defined the
##' user can supply a list of colour names recognised by R (type
##' \code{colours()} to see the full list). An example would be
##' \code{cols = c("yellow", "green", "blue")}
##' @param mean Show the mean by wind direction as a line?
##' @param mean.lty Line type for mean line.
##' @param mean.lwd Line width for mean line.
##' @param mean.col Line colour for mean line.
##' @param fill Should the percentile intervals be filled (default) or should
##'   lines be drawn (\code{fill = FALSE}).
##' @param intervals User-supplied intervals for the scale
##' e.g. \code{intervals = c(0, 10, 30, 50)}
##' @param angle.scale The pollutant scale is by default shown at a 45 degree
##'   angle. Sometimes the placement of the scale may interfere with an
##'   interesting feature. The user can therefore set \code{angle.scale} to
##'   another value (between 0 and 360 degrees) to mitigate such problems. For
##'   example \code{angle.scale = 315} will draw the scale heading in a NW
##'   direction.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the \sQuote{2}
##'   in NO2.
##' @param key.header Adds additional text/labels to the scale key.
##'   For example, passing options \code{key.header = "header", key.footer =
##'   "footer"} adds addition text above and below the scale key. These
##'   arguments are passed to \code{drawOpenKey} via \code{quickText}, applying
##'   the \code{auto.text} argument, to handle formatting.
##' @param key.footer \code{key.header}.
##' @param key.position Location where the scale key is to plotted.  Allowed
##'   arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
##'   and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{drawOpenKey} for further details.
##' @param ... Other graphical parameters are passed onto \code{cutData} and
##'   \code{lattice:xyplot}. For example, \code{percentileRose} passes the option
##'   \code{hemisphere = "southern"} on to \code{cutData} to provide southern
##'   (rather than default northern) hemisphere handling of \code{type = "season"}.
##'   Similarly, common graphical arguments, such as \code{xlim} and \code{ylim}
##'   for plotting ranges and \code{lwd} for line thickness when using
##'   \code{fill = FALSE}, are passed on \code{xyplot}, although some local
##'   modifications may be applied by openair. For example, axis and title
##'   labelling options (such as \code{xlab}, \code{ylab} and \code{main})
##'   are passed to \code{xyplot} via \code{quickText} to handle routine formatting.
##' @export
##' @return As well as generating the plot itself, \code{percentileRose} also
##'   returns an object of class \dQuote{openair}. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- percentileRose(mydata, "nox")}, this output can be used
##'   to recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}.
##' @author David Carslaw
##' @seealso See Also as \code{\link{windRose}}, \code{\link{pollutionRose}},
##'   \code{\link{polarFreq}}, \code{\link{polarPlot}}
##' @references
##' Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A
##' residence time probability analysis of sulfur concentrations at
##' ground canyon national park. Atmospheric Environment 19 (8),
##' 1263-1270.
##' @keywords methods
##' @examples
##'
##' # basic percentile plot
##' percentileRose(mydata, pollutant = "o3")
##'
##' # 50/95th percentiles of ozone, with different colours
##' percentileRose(mydata, pollutant = "o3", percentile = c(50, 95), col = "brewer1")
##'
##' \dontrun{
##' # percentiles of ozone by year, with different colours
##' percentileRose(mydata, type = "year", pollutant = "o3", col = "brewer1")
##'
##' # percentile concentrations by season and day/nighttime..
##' percentileRose(mydata, type = c("season", "daylight"), pollutant = "o3", col = "brewer1")
##' }
##'
##'
percentileRose <- function (mydata, pollutant = "nox", wd = "wd", type = "default",
                            percentile = c(25, 50, 75, 90, 95), smooth = FALSE,
                            method = "default", cols = "default",
                            mean = TRUE, mean.lty = 1, mean.lwd = 3, mean.col = "grey",
                            fill = TRUE, intervals = NULL, angle.scale = 45,
                            auto.text = TRUE,  key.header = NULL,
                            key.footer = "percentile", key.position = "bottom",
                            key = TRUE,  ...)

{

  ## get rid of R check annoyances
  sub <- NULL

  ## calculate percetiles or just show mean?
  if (is.na(percentile[1])) {
    mean.only <- TRUE
    percentile <- 0
  } else {
    mean.only <- FALSE
  }

  if (tolower(method) == "cpf") {
    mean <- FALSE
    if (length(percentile) > 1) stop ("Only one percentile should be supplied when method = 'CPF'.")
  }

  vars <- c(wd, pollutant)
  if (any(type %in%  dateTypes)) vars <- c(vars, "date")

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE, wd = wd)
  ## round wd
  mydata[, wd] <- 10 * ceiling(mydata[, wd] / 10 - 0.5)

  ## make sure all wds are present
  ids <- which(!seq(10, 360, by = 10) %in% unique(mydata[, wd]))
  if (length(ids) > 0 & smooth != TRUE) {

    extra <- mydata[rep(1, length(ids)), ]
    extra[, wd] <- seq(10, 360, by = 10)[ids]
    extra[, pollutant] <- NA
    mydata <- rbind(mydata, extra)
  }

  ## need lowest value if shading
  if (fill) percentile <- unique(c(0, percentile))

  ## if more than one pollutant, need to stack the data and set type = "variable"
  ## this case is most relevent for model-measurement compasrions where data are in columns
  ## Can also do more than one pollutant and a single type that is not "default", in which
  ## case pollutant becomes a conditioning variable
  if (length(pollutant) > 1) {

    if (length(type) > 1) {
      warning(paste("Only type = '", type[1], "' will be used", sep = ""))
      type <- type[1]
    }
    ## use pollutants as conditioning variables
    mydata <- melt(mydata, measure.vars = pollutant)
    ## now set pollutant to "value"
    pollutant <- "value"
    if (type == "default") {
      type <- "variable"
    } else {
      type <- c(type, "variable")
    }

  }


  ##extra.args setup
  extra.args <- list(...)

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  ## reset graphic parameters
  on.exit(trellis.par.set(strip.background = current.strip,
                          fontsize = current.font))

  #label controls
  extra.args$xlab <- if ("xlab" %in% names(extra.args))
    quickText(extra.args$xlab, auto.text) else quickText("", auto.text)

  extra.args$ylab <- if ("ylab" %in% names(extra.args))
    quickText(extra.args$ylab, auto.text) else quickText("", auto.text)

  extra.args$main <- if ("main" %in% names(extra.args))
    quickText(extra.args$main, auto.text) else quickText("", auto.text)

  if ("fontsize" %in% names(extra.args))
      trellis.par.set(fontsize = list(text = extra.args$fontsize))
  
  ## layout default
  if (!"layout" %in% names(extra.args))
    extra.args$layout <- NULL

  ## lwd handling
  if (!"lwd" %in% names(extra.args))
    extra.args$lwd <- 2

  ## mydata <- na.omit(mydata)
  id <- which(is.na(mydata[, wd]))
  if (length(id) > 0)
      mydata <- mydata[-id, ]

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {

    trellis.par.set(list(strip.background = list(col = "white")))
  }

 

  if (!fill) { ## labels depend on whether line or area are used
    theLabels <- percentile
  } else {
    values <- cbind(percentile[-length(percentile)], percentile[-1])
    theLabels <- paste(values[ , 1], "-", values[ , 2], sep = "")
  }

  prepare.grid <- function(mydata, stat, overall.lower, overall.upper) {
    #wd = NULL
    ## add zero wind angle = same as 360 for cyclic spline
    ids <- which(mydata[ , wd] == 360)


     if (length(ids) > 0) {
       zero.wd <- mydata[ids, ]
       zero.wd[, wd] <- 0
       mydata <- bind_rows(mydata, zero.wd)
     }

    mod.percentiles <- function(i, mydata, overall.lower, overall.upper) {
      ## need to work out how many knots to use in smooth
      thedata <- subset(percentiles, percentile == i)


      if (smooth) {
          min.dat <- min(thedata)

        ## fit a spline through the data; making sure it goes through each wd value
        spline.res <- spline(x = thedata[ , wd], y = thedata[, pollutant], n = 361,
                             method = "natural")

        pred <- data.frame(percentile = i, wd = 0:360, pollutant = spline.res$y)

        ## don't let interpolated percentile be lower than data
        pred$pollutant[pred$pollutant < min.dat] <- min.dat

        ## only plot where there are valid wd
        wds <- unique(percentiles[, wd])
        ids <- lapply(wds, function(x) seq(from = x - 5, to = x + 5))
        ids <- unique(do.call(c, ids))
        ids[ids < 0] <- ids[ids < 0] + 360
        pred$pollutant[-ids] <- min(c(0, min(percentiles[ , pollutant], na.rm = TRUE)))

      } else {

        ## do not smooth
          dat1 <- thedata
          dat2 <- thedata
        dat1[, wd] <- thedata[, wd] - 5
        dat2[, wd] <- thedata[, wd] + 5
        dat1$id <- 2 * 1:nrow(dat1) - 1
        dat2$id <- 2 * 1:nrow(dat2)
        thedata <- rbind(dat1, dat2)
        id <- which(thedata[, wd] == -5)
        thedata[, wd][id] <- 0
        id <- which(thedata[, wd] == 365)
        thedata[, wd][id] <- 0

        thedata <- thedata[order(thedata$id), ]
        thedata$pollutant <- thedata[, eval(pollutant)]
          pred <- thedata

      }
      pred
    }

    if (method == "default") {
        
      ## calculate percentiles
      percentiles <- plyr::ddply(mydata, wd, numcolwise(function (x)
        quantile(x, probs = percentile / 100, na.rm = TRUE)))
      percentiles$percentile <- percentile

    }

    if (tolower(method) == "cpf") {

      percentiles1 <- plyr::ddply(mydata, wd, numcolwise(function (x)
                                                   length(which(x < overall.lower)) /
                                                   length(x)))
      percentiles1$percentile <- min(percentile)

      percentiles2 <- plyr::ddply(mydata, wd, numcolwise(function (x)
                                                   length(which(x > overall.upper)) /
                                                   length(x)))
      percentiles2$percentile <- max(percentile)

      if (fill) {
        percentiles <- rbind(percentiles1, percentiles2)
      } else {
        percentiles <- percentiles2
      }

    }


    results <- plyr::ldply(percentile, mod.percentiles, overall.lower, overall.upper)

    ## calculate mean; assume a percentile of 999 to flag it later
    percentiles <- plyr::ddply(mydata, wd, numcolwise(function (x) mean(x, na.rm = TRUE)))
    percentiles$percentile <- 999
    Mean <- plyr::ldply(999, mod.percentiles)

    if (stat == "percentile") results <- results else results <- Mean
    results
  }


  mydata <- cutData(mydata, type, ...)

  ## overall.lower and overall.upper are the OVERALL upper/lower percentiles
  overall.lower <-  quantile(mydata[, pollutant], probs = min(percentile) / 100, na.rm = TRUE)
  overall.upper =  quantile(mydata[, pollutant], probs = max(percentile) / 100, na.rm = TRUE)

  results.grid <- plyr::ddply(mydata, type, prepare.grid, stat = "percentile",
                        overall.lower, overall.upper)

  if (method == "cpf") {
    ## useful labelling
    sub <- paste("CPF at the ", max(percentile),
                 "th percentile (=",
                 round(max(quantile(mydata[, pollutant], probs = percentile / 100,
                                    na.rm = TRUE)), 1), ")", sep = "")
  }


  if (mean) {
    Mean <- plyr::ddply(mydata, type, prepare.grid, stat = "mean")

    results.grid <- rbind(results.grid, Mean)
  }

  ## proper names of labelling ###################################################
  strip.dat <- strip.fun(results.grid, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  col <- openColours(cols, length(theLabels))

  legend <- list(col = col, space = key.position, auto.text = auto.text,
                 labels = theLabels, footer = key.footer, header = key.header,
                 height = 0.60, width = 1.5, fit = "scale",
                 plot.style =  "other")
  legend <- makeOpenKeyLegend(key, legend, "percentileRose")

  if (mean.only || tolower(method) == "cpf") legend <- NULL

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("y ~ x | ", temp, sep = ""))

  ## keep unstransformed copy in case data are negative
  results <- results.grid

  results.grid$x <- results.grid$pollutant * sin(results.grid[, wd] * pi / 180)
  results.grid$y <- results.grid$pollutant * cos(results.grid[, wd] * pi / 180)


  min.res <- min(results.grid$pollutant, na.rm = TRUE)

  newdata <- results.grid ## data to return

  ## nice intervals for pollutant concentrations
  tmp <- (results.grid$x ^ 2 + results.grid$y ^ 2) ^ 0.5
  if (missing(intervals)) intervals <- pretty(c(min(tmp, na.rm = TRUE), max(tmp, na.rm = TRUE)))



  labs <- intervals ## the labels

  ## if negative data, add to make all postive to plot properly
  min.int <- min(intervals, na.rm = TRUE)
  zero <- NA

  if (min.int < 0 ) {
    zero <- which(intervals == 0) ## the zero line
    intervals <- intervals + -1 * min.int
    results$pollutant <- results$pollutant + -1 * min.int
    results.grid <- transform(results, x = pollutant * sin(eval(wd) * pi / 180),
                              y = pollutant * cos(eval(wd) * pi / 180))
  }

  ## re-label if CPF plot
  if (tolower(method) == "cpf") pollutant <- "probability"

  xyplot.args <- list(x = myform,
                      xlim = c(max(intervals) * -1, max(intervals) * 1),
                      ylim = c(max(intervals) * -1, max(intervals) * 1),
                      data = results.grid,
                      type = "n",
                      sub = sub,
                      strip = strip,
                      strip.left = strip.left,
                      as.table = TRUE,
                      aspect = 1,
                      par.strip.text = list(cex = 0.8),
                      scales = list(draw = FALSE),...,

                      panel = function(x, y, subscripts, ...) {

                        if (fill) { ## filled polygons

                          for (i in rev(seq_along(percentile))) {
                            value <- percentile[i]

                            if (i == 1) {
                              subdata <- subset(results.grid[subscripts, ], percentile == value)
                             lpolygon(subdata$x, subdata$y, col = "white", border = NA)

                            } else {

                              subdata1 <- subset(results.grid[subscripts, ], percentile == value)
                              value2 <- percentile[i - 1]
                              subdata2 <- subset(results.grid[subscripts, ],
                                                 percentile == value2)

                              poly.na(x1 = subdata1$x, x2 = subdata2$x, y1 = subdata1$y, y2 = subdata2$y,
                                       myColors = col[i - 1], alpha = 1, border = col[i - 1])

                            }
                          }
                        }

                        angles <- seq(0, 2 * pi, length = 360)
                        sapply(intervals, function(x) llines(x * sin(angles), x * cos(angles),
                                                             col = "grey85", lty = 5))

                        ## zero line if needed
                        if (!is.na(zero)) llines(intervals[zero] * sin(angles),
                                                 intervals[zero] * cos(angles), col = "grey85")


                        ## add axis lines
                        larrows(max(intervals) * -1, 0, max(intervals), 0, code = 3, length = 0.1)
                        larrows(0, max(intervals) * -1, 0, max(intervals), code = 3, length = 0.1)


                        ltext(0.7 * sin(pi * (angle.scale + 5) / 180) * max(intervals),
                              0.7 * cos(pi * (angle.scale + 5) / 180) * max(intervals),
                              quickText(pollutant, auto.text), srt = 0, cex = 0.8, pos = 4)


                        ltext(max(intervals) * -1 * 0.95, 0.07 * max(intervals), "W", cex = 0.7)
                        ltext(0.07 * max(intervals), max(intervals) * -1 * 0.95, "S", cex = 0.7)
                        ltext(0.07 * max(intervals), max(intervals) * 0.95, "N", cex = 0.7)
                        ltext(max(intervals) * 0.95, 0.07 * max(intervals), "E", cex = 0.7)

                        ## draw lines if fill = FALSE
                        if (!fill) {
                          for (i in seq_along(percentile)) {
                            value <- percentile[i]
                            subdata <- subset(results.grid[subscripts, ], percentile == value)
                            llines(subdata$x, subdata$y, col = col[i], lwd = extra.args$lwd)
                          }

                        }

                        ## add mean line
                        if (mean) {
                          subdata <- subset(results.grid[subscripts, ], percentile == 999)
                          llines(subdata$x, subdata$y, col = mean.col, lwd = mean.lwd, lty = mean.lty)
                        }

                        ltext(intervals * sin(pi * angle.scale / 180),
                              intervals * cos(pi * angle.scale / 180),
                              paste(labs, c("", "", rep("", 7))), cex = 0.7)


                      }, legend = legend)

  #reset for extra.args
  xyplot.args<- listUpdate(xyplot.args, extra.args)

  #plot
  plt <- do.call(xyplot, xyplot.args)


  ## output ####################################################################################

  if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))

  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"


  invisible(output)

}
