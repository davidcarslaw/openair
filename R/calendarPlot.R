## development of a calendar function
## David Carslaw November 2009
## modifications for internation Karl Ropkins 2010
## drawOpenKey add-in Karl Ropkins 2010

## calendarPlot shows air quality or other data in a conventional
## calendar format.  This makes it easy to gain a feel about
## variations in concentrations and other parameters on a daily basis.
## By default it shows the day of the month for each day, but can also
## show the wind vector, which may also be normalised to wind speed
## and categorical scales.



##' Plot time series values in convential calendar format
##'
##' This function will plot one year of data by month laid out in a 
##' conventional calendar format. The main purpose is to help rapidly
##' visualise potentially complex data in a familiar way. Users can
##' also choose to show daily mean wind vectors if wind speed and
##' direction are available.
##' 
##' \code{calendarPlot} will plot one year of data in a conventional 
##' calendar format i.e. by month and day of the week. The main 
##' purpose of this function is to make it easy to visualise data in a
##' familiar way. Daily statistics are calculated using 
##' \code{\link{timeAverage}}, which by default will calculate the 
##' daily mean concentration.
##' 
##' If wind direction is available it is then possible to plot the
##' wind direction vector on each day. This is very useful for getting
##' a feel for the meteorological conditions that affect pollutant
##' concentrations. Note that if hourly or higher time resolution are
##' supplied, then \code{calendarPlot} will calculate daily averages
##' using \code{\link{timeAverage}}, which ensures that wind
##' directions are vector-averaged.
##' 
##' If wind speed is also available, then setting the option
##' \code{annotate = "ws"} will plot the wind vectors whose length is
##' scaled to the wind speed. Thus information on the daily mean wind
##' speed and direction are available.
##' 
##' It is also possible to plot categorical scales. This is useful 
##' where, for example, an air quality index defines concentrations as
##' bands e.g. \dQuote{good}, \dQuote{poor}. In these cases users must
##' supply \code{labels} and corresponding \code{breaks}.
##' 
##' Note that is is possible to pre-calculate concentrations in some 
##' way before passing the data to \code{calendarPlot}. For example 
##' \code{\link{rollingMean}} could be used to calculate rolling 
##' 8-hour mean concentrations. The data can then be passed to 
##' \code{calendarPlot} and \code{statistic = "max"} chosen, which 
##' will plot maximum daily 8-hour mean concentrations.
##' 
##' @param mydata A data frame minimally containing \code{date} and at
##'   least one other numeric variable. The date should be in either 
##'   \code{Date} format or class \code{POSIXct}.
##' @param pollutant Mandatory. A pollutant name corresponding to a
##'   variable in a data frame should be supplied e.g. \code{pollutant
##'   = "nox". }
##' @param year Year to plot e.g. \code{year = 2003}.
##' @param month If only certain month are required. By default the 
##'   function will plot an entire year even if months are missing. To
##'   only plot certain months use the \code{month} option where month
##'   is a numeric 1:12 e.g. \code{month = c(1, 12)} to only plot 
##'   January and December.
##' @param type Not yet implemented.
##' @param annotate This option controls what appears on each day of 
##'   the calendar. Can be: \dQuote{date} --- shows day of the month; 
##'   \dQuote{wd} --- shows vector-averaged wind direction, or 
##'   \dQuote{ws} --- shows vector-averaged wind direction scaled by 
##'   wind speed. Finally it can be \dQuote{value} which shows the
##'   daily mean value.
##' @param statistic Statistic passed to \code{timeAverage}.
##' @param cols Colours to be used for plotting. Options include 
##'   \dQuote{default}, \dQuote{increment}, \dQuote{heat},
##'   \dQuote{jet} and \code{RColorBrewer} colours --- see the
##'   \code{openair} \code{openColours} function for more details. For
##'   user defined the user can supply a list of colour names
##'   recognised by R (type \code{colours()} to see the full list). An
##'   example would be \code{cols = c("yellow", "green", "blue")}
##' @param limits Use this option to manually set the colour scale 
##'   limits. This is useful in the case when there is a need for two
##'   or more plots and a consistent scale is needed on each. Set the 
##'   limits to cover the maximimum range of the data for all plots of
##'   interest. For example, if one plot had data covering 0--60 and 
##'   another 0--100, then set \code{limits = c(0, 100)}. Note that
##'   data will be ignored if outside the limits range.
##' @param lim A threshold value to help differentiate values above 
##'   and below \code{lim}. It is used when \code{annotate = "value"}.
##'   See next few options for control over the labels used.
##' @param col.lim For the annotation of concentration labels on each 
##'   day. The first sets the colour of the text below \code{lim} and 
##'   the second sets the colour of the text above \code{lim}.
##' @param font.lim For the annotation of concentration labels on each
##'   day. The first sets the font of the text below \code{lim} and
##'   the second sets the font of the text above \code{lim}. Note that
##'   font = 1 is normal text and font = 2 is bold text.
##' @param cex.lim For the annotation of concentration labels on each 
##'   day. The first sets the size of the text below \code{lim} and 
##'   the second sets the size of the text above \code{lim}.
##' @param digits The number of digits used to display concentration 
##'   values when \code{annotate = "value"}.
##' @param data.thresh Data capture threshold passed to 
##'   \code{timeAverage}. For example, \code{data.thresh = 75} means 
##'   that at least 75\% of the data must be available in a day for
##'   the value to be calculate, else the data is removed.
##' @param labels If a categorical scale is required then these labels
##'   will be used. Note there is one less label than break. For 
##'   example, \code{labels = c("good", "bad", "very bad")}.
##'   \code{breaks} must also be supplied if labels are given.
##' @param breaks If a categorical scale is required then these breaks
##'   will be used. For example, \code{breaks = c(0, 50, 100, 1000)}.
##'   In this case \dQuote{good} corresponds to values berween 0 and
##'   50 and so on. Users should set the maximum value of
##'   \code{breaks} to exceed the maximum data value to ensure it is
##'   within the maximum final range e.g. 100--1000 in this case.
##' @param main The plot title; default is pollutant and year.
##' @param w.shift Controls the order of the days of the week. By
##'   default the plot shows Saturday first (\code{w.shift = 0}). To
##'   change this so that it starts on a Monday for example, set
##'   \code{w.shift = 2}, and so on.
##' @param key.header Adds additional text/labels to the scale key.
##'   For example, passing \code{calendarPlot(mydata, key.header =
##'   "header", key.footer = "footer")} adds addition text above and
##'   below the scale key. These arguments are passed to
##'   \code{drawOpenKey} via \code{quickText}, applying the
##'   \code{auto.text} argument, to handle formatting.
##' @param key.footer see \code{key.header}.
##' @param key.position Location where the scale key is to plotted. 
##'   Allowed arguments currently include \code{"top"},
##'   \code{"right"}, \code{"bottom"} and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}.
##'   See \code{drawOpenKey} for further details.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If 
##'   \code{TRUE} titles and axis labels will automatically try and
##'   format pollutant names and units properly e.g.  by subscripting
##'   the `2' in NO2.
##' @param ... Other graphical parameters are passed onto the
##'   \code{lattice} function \code{lattice:levelplot}, with common
##'   axis and title labelling options (such as \code{xlab},
##'   \code{ylab}, \code{main}) being passed to via \code{quickText}
##'   to handle routine formatting.
##' @export
##' @import grid
##' @return As well as generating the plot itself, \code{calendarPlot}
##'   also returns an object of class ``openair''. The object includes
##'   three main components: \code{call}, the command used to generate
##'   the plot; \code{data}, the data frame of summarised information
##'   used to make the plot; and \code{plot}, the plot itself. If
##'   retained, e.g. using \code{output <- calendarPlot(mydata,
##'   "nox")}, this output can be used to recover the data, reproduce
##'   or rework the original plot or undertake further analysis.
##'   
##'   An openair output can be manipulated using a number of generic
##'   operations, including \code{print}, \code{plot} and
##'   \code{summary}.
##' @author David Carslaw
##' @seealso \code{\link{timePlot}}, \code{\link{timeVariation}}
##' @keywords methods
##' @examples
##'
##'
##' # load example data from package
##' data(mydata)
##'
##' # basic plot
##' calendarPlot(mydata, pollutant = "o3", year = 2003)
##'
##' # show wind vectors
##' calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "wd")
##' \dontrun{
##' # show wind vectors scaled by wind speed and different colours
##' calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "ws",
##' cols = "heat")
##'
##' # show only specific months with selectByDate
##' calendarPlot(selectByDate(mydata, month = c(3,6,10), year = 2003),
##' pollutant = "o3", year = 2003, annotate = "ws", cols = "heat")
##'
##' # categorical scale example
##' calendarPlot(mydata, pollutant = "no2", breaks = c(0, 50, 100, 150, 1000),
##' labels = c("Very low", "Low", "High", "Very High"),
##' cols = c("lightblue", "green", "yellow",  "red"), statistic = "max")
##'
##' }
##'
calendarPlot <- function(mydata, pollutant = "nox", year = 2003, month = 1:12, 
                         type = "default",
                         annotate = "date", statistic = "mean", cols = "heat", 
                         limits = c(0, 100),
                         lim = NULL, col.lim = c("grey30", "black"),
                         font.lim = c(1, 2), cex.lim = c(0.6, 1),
                         digits = 0, data.thresh = 0, labels = NA,
                         breaks = NA, w.shift = 0,
                         main = paste(pollutant, "in", year),
                         key.header = "", key.footer = "",
                         key.position = "right", key = TRUE, auto.text = TRUE,
                         ...) {

    conc.mat <- NULL ## keep R check quiet

    ## international keyboard
    ## first letter and ordered Sun to Sat
    
    if (w.shift < 0 || w.shift > 6) 
      warning("w.shift should be between 0 and 6")

    weekday.abb <- substr(format(ISOdate(2000, 1, 2:8), "%A"), 1, 1)[((6:12) + 
                                                                        w.shift)%%7 + 1]

    ## extra args
    extra.args <- list(...)

    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip,
                            fontsize = current.font))

    ## label controls
    ## (main currently handled in formals)
    extra.args$xlab <- if ("xlab" %in% names(extra.args))
        quickText(extra.args$xlab, auto.text) else quickText("", auto.text)

    extra.args$ylab <- if ("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else quickText("", auto.text)

    if ("fontsize" %in% names(extra.args))
        trellis.par.set(fontsize = list(text = extra.args$fontsize))

    ## extract variables of interest
    if (annotate %in% c("date", "value")) vars <- c("date", pollutant)
    if (annotate == "wd") vars <- c("wd", "ws", "date", pollutant)
    if (annotate == "ws") vars <- c("wd", "ws", "date", pollutant)

    ## select year first, then check variables
    if (missing(year)) {
        year <- as.numeric(format(mydata$date[1], "%Y"))
        warning (paste("'year not supplied, using'", year))
    }

    mydata <- selectByDate(mydata, year = year)
    if (nrow(mydata) == 0 ) stop("No data to plot - check year chosen")
    mydata <- checkPrep(mydata, vars, "default", remove.calm = FALSE)

    main <- quickText(main, auto.text)


    ## themes for calendarPlot
    def.theme  <- list(strip.background = list(col = "#ffe5cc"),
                       strip.border = list(col = "black"),
                       axis.line = list(col = "black"),
                       par.strip.text = list(cex =1))

    cal.theme <- list(strip.background = list(col = "grey90"),
                      strip.border = list(col = "transparent"),
                      axis.line = list(col = "transparent"),
                      par.strip.text = list(cex = 0.8))

    lattice.options(default.theme = cal.theme)

    ## all the days in the year
    all.dates <- seq(as.Date(paste(year, "-01-01", sep = "")),
                     as.Date(paste(year, "-12-31", sep = "")), by = "day")

    prepare.grid <- function(mydata, pollutant) {

        ## if month is partial make sure name is available for all days
        month <- mydata$month[1]
        
        ## make sure all days in month are present
        all.days <-  all.dates[format(all.dates, "%B") %in% format(mydata$date[1], "%B")]
        all.days <- data.frame(date = all.days)
        mydata <- merge(mydata, all.days, all = TRUE)
        mydata$month <- month

        firstDay <- format(mydata$date[1], "%A")
        lastDay <- as.numeric(format(mydata$date[length(mydata$date)], "%d"))

        ## number of blank cells at beginning to get calendar format
        pad.start <- (as.numeric(format(mydata$date[1], "%w")) - w.shift) %% 7 + 1

        ## need to do in reverse to plot easily
        conc <- rev(mydata[, pollutant])

        ## day of the month
        theDates <- as.numeric(format(mydata$date, "%d"))
        theDates <- rev(theDates)

        daysAtEnd <- 42 - pad.start - nrow(mydata) ## 7x6 regular grid
        conc <- c(rep(NA, daysAtEnd), conc)

        ## get relevant days in previous and next month, like a real calendar
        endDates <- mydata$date[nrow(mydata)] + (1 :daysAtEnd)
        endDates <- rev(as.numeric(format(endDates, "%d")))

        theDates <- c(endDates, theDates)

        beginDates <-  -1 * (1 : pad.start) + mydata$date[1]
        beginDates <- as.numeric(format(beginDates, "%d"))

        conc <- c(conc, rep(NA, pad.start))

        if (pad.start != 0) theDates <- c(theDates, beginDates)

        ## colurs for dates
        dateColour <- c(rep("grey70", daysAtEnd), rep("black", nrow(mydata)),
                        rep("grey70", pad.start))

        ## convert to matrix
        conc.mat <- matrix(conc, ncol = 7, byrow = TRUE)
        date.mat <- matrix(theDates, ncol = 7, byrow = TRUE)
        colour.mat <- matrix(dateColour, ncol = 7, byrow = TRUE)

        ## need to reverse each row for calendar format
        conc.mat <-  as.vector(apply(conc.mat, 1, rev))
        date.mat <-  as.vector(apply(date.mat, 1, rev))
        colour.mat <- as.vector(apply(colour.mat, 1, rev))
        
        grid <- data.frame(expand.grid(x = 1:7, y = 1:6))
        results <- suppressWarnings(data.frame(x = grid$x, y = grid$y, conc.mat,
                                               month = mydata$month[1],
                                               date.mat = date.mat, dateColour = colour.mat))

        results
    }
    
    ## calculate daily means
    if ("POSIXt" %in% class(mydata$date) && !is.factor(mydata[, pollutant])) {
        mydata <- timeAverage(mydata, "day", statistic = statistic, data.thresh = data.thresh)
        mydata$date <- as.Date(mydata$date)
    }

    ## type not yet used, set to month
    type <- "month"
    mydata <- cutData(mydata, type = type, ...)
    baseData <- mydata
    
    ## subset months if necessary
    mydata <- mydata[mydata$month %in% unique(format(all.dates, "%B")), ]
    mydata$month <- factor(mydata$month)

    strip.dat <- strip.fun(mydata, type, auto.text)
    strip <- strip.dat[[1]]

    mydata <- group_by_(mydata, .dots = type) %>%
        do(prepare.grid(., pollutant))
    
    mydata$value <- mydata$conc.mat ## actual numerical value (retain for categorical scales)

    category <- FALSE ## assume pollutant is not a categorical value

    if (!is.na(labels) && !is.na(breaks)) {
        category <- TRUE
        mydata <- transform(mydata, conc.mat = cut(conc.mat, breaks = breaks, labels = labels))
    }

    if (annotate == "wd") {
        baseData$wd <- baseData$wd * 2 * pi / 360
    
        wd  <- group_by_(baseData, type) %>%
          do(prepare.grid(., "wd"))
        
        wd$value <- wd$conc.mat ## actual numerical value (retain for categorical scales)
    }

    if (annotate == "ws") {
        baseData$wd <- baseData$wd * 2 * pi / 360
  
        ws  <- group_by_(baseData, type) %>%
          do(prepare.grid(., "ws"))
        
        wd  <- group_by_(baseData, type) %>%
          do(prepare.grid(., "wd"))
        
        ## normalise wind speeds to highest daily mean
        ws$conc.mat <- ws$conc.mat / max(ws$conc.mat, na.rm = TRUE)
        ws$value <- ws$conc.mat ## actual numerical value (retain for categorical scales)
        wd$value <- wd$conc.mat ## actual numerical value (retain for categorical scales)
    }

    ## set up scales

    ## categorical scales required
    if (category) {
        ## check the breaks and labels are consistent
        if (length(labels) + 1 != length(breaks)) stop("Need one more break than labels")
        n <- length(levels(mydata$conc.mat))

        col <- openColours(cols, n)
        legend <- list(col = col, space = key.position, auto.text = auto.text,
                       labels = levels(mydata$conc.mat), footer = key.footer,
                       header = key.header, height = 0.8, width = 1.5, fit = "scale",
                       plot.style = "other")

        col.scale <- breaks
        legend <- makeOpenKeyLegend(key, legend, "windRose")

    } else { ## continuous colour scale
        nlev <- 200

        ## handle missing breaks arguments

        if (missing(limits)) {
            breaks <- pretty(mydata$value, n = nlev)
            labs <- pretty(breaks, 7)
            labs <- labs[labs >= min(breaks) & labs <= max(breaks)]

        } else {

            breaks <- pretty(limits, n = nlev)
            labs <- pretty(breaks, 7)
            labs <- labs[labs >= min(breaks) & labs <= max(breaks)]

            if (max(limits) < max(mydata$value, na.rm = TRUE)) {
                ## if clipping highest, then annotate differently
                id <- which(mydata$value > max(limits))
                mydata$value[id] <- max(limits)
                labs <- pretty(breaks, 7)
                labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
                labs[length(labs)] <- paste(">", labs[length(labs)])
            }
        }


        nlev2 <- length(breaks)
        col <- openColours(cols, (nlev2 - 1))
        col.scale <- breaks

        legend <- list(col = col, at = col.scale, labels = list(labels = labs),
                   space = key.position, auto.text = auto.text,
                   footer = key.footer, header = key.header,
                   height = 1, width = 1.5, fit = "all")

        legend <- makeOpenKeyLegend(key, legend, "calendarPlot")
    }


    
    
    lv.args <- list(x = value ~ x * y | month, data = mydata,
                    
                    par.settings = cal.theme,
                    main = main,
                    strip = strip,
                    at = col.scale,
                    col.regions = col,
                    as.table = TRUE,
                    scales = list(y = list(draw = FALSE),
                    x = list(at = 1:7, labels = weekday.abb, tck = 0),
                    par.strip.text = list(cex = 0.8),
                    alternating = 1, relation = "free"),
                    aspect = 6/7,
                    between = list(x = 1),
                    colorkey = FALSE, legend = legend,
                    panel = function(x, y, subscripts,...) {
                        
                        panel.levelplot(x, y, subscripts,...)
                        panel.abline(v=c(0.5: 7.5), col = "grey90")
                        panel.abline(h=c(0.5: 7.5), col = "grey90")
                        
                        if (annotate == "date") {
                            ltext(x, y, labels = mydata$date.mat[subscripts], cex = 0.6,
                                  col = as.character(mydata$dateColour[subscripts]))
                        }
                        
                        if (annotate == "value") {
                            ## add some dates for navigation
                            date.col <- as.character(mydata$dateColour[subscripts])
                            ids <- which(date.col == "black")
                            date.col[ids] <- "transparent"
                            ltext(x, y, labels = mydata$date.mat[subscripts], cex = 0.6,
                                  col = date.col)

                            concs <- mydata$value[subscripts]

                            ## deal with values above/below threshold
                            ids <- seq_along(concs)
                            the.cols <- rep(col.lim[1], length(ids))
                            the.font <- rep(font.lim[1], length(ids))
                            the.cex <- rep(cex.lim[1], length(ids))
                            if (!is.null(lim)) {
                                ## ids where conc is >= lim
                                ids <- which(concs >= lim)
                                the.cols[ids] <- col.lim[2]
                                the.font[ids] <- font.lim[2]
                                the.cex[ids] <- cex.lim[2]
                            }

                            the.labs <- round(concs, digits = digits)
                            id <- which(is.na(the.labs))
                            if (length(id) > 0) {
                                the.labs <- as.character(the.labs)
                                the.labs[id] <- ""
                            }
                            ltext(x, y, labels = the.labs, cex = the.cex,
                                  font = the.font, col = the.cols)
                        }

                        if (annotate == "wd") {
                            larrows(x + 0.5 * sin(wd$value[subscripts]),
                                    y +  0.5 * cos(wd$value[subscripts]),
                                    x +  -0.5 * sin(wd$value[subscripts]),
                                    y +  -0.5 * cos(wd$value[subscripts]),
                                    angle = 20, length = 0.07, lwd = 0.5)
                        }

                        if (annotate == "ws") {
                            larrows(x + (0.5 * sin(wd$value[subscripts]) *
                                         ws$value[subscripts]),
                                    y +  (0.5 * cos(wd$value[subscripts]) *
                                          ws$value[subscripts]) ,
                                    x +  (-0.5 * sin(wd$value[subscripts]) *
                                          ws$value[subscripts]) ,
                                    y +  (-0.5 * cos(wd$value[subscripts]) *
                                          ws$value[subscripts]),
                                    angle = 20, length = 0.07, lwd = 0.5)
                        }

                    })

    ## reset for extra.args
    lv.args <- listUpdate(lv.args, extra.args)

    ## plot
    print(do.call(levelplot, lv.args))

    ## reset theme
    lattice.options(default.theme = def.theme)

    ## ###############
    ## output
    ## ###############
    plt <- trellis.last.object()
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)

}

