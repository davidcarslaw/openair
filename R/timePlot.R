##' Plot time series
##'
##' Plot time series quickly, perhaps for multiple pollutants, grouped or in
##' separate panels.
##'
##' The \code{timePlot} is the basic time series plotting function in
##' \code{openair}. Its purpose is to make it quick and easy to plot time
##' series for pollutants and other variables. The other purpose is to plot
##' potentially many variables together in as compact a way as possible.
##'
##' The function is flexible enough to plot more than one variable at once. If
##' more than one variable is chosen plots it can either show all variables on
##' the same plot (with different line types) \emph{on the same scale}, or (if
##' \code{group = FALSE}) each variable in its own panels with its own scale.
##'
##' The general preference is not to plot two variables on the same graph with
##' two different y-scales. It can be misleading to do so and difficult with
##' more than two variables. If there is in interest in plotting several
##' variables together that have very different scales, then it can be useful
##' to normalise the data first, which can be down be setting the
##' \code{normalise} option.
##'
##' The user has fine control over the choice of colours, line width and line
##' types used. This is useful for example, to emphasise a particular variable
##' with a specific line type/colour/width.
##'
##' \code{timePlot} works very well with \code{\link{selectByDate}}, which is
##' used for selecting particular date ranges quickly and easily. See examples
##' below.
##'
##' By default plots are shown with a colour key at the bottom and in teh case
##' of multiple pollutants or sites, strips on the left of each plot. Sometimes
##' this may be overkill and the user can opt to remove the key and/or the
##' strip by setting \code{key} and/or \code{strip} to \code{FALSE}. One reason
##' to do this is to maximise the plotting area and therefore the information
##' shown.
##'
##' @param mydata A data frame of time series. Must include a \code{date} field
##'   and at least one variable to plot.
##' @param pollutant Name of variable to plot. Two or more pollutants can be
##'   plotted, in which case a form like \code{pollutant = c("nox", "co")}
##'   should be used.
##' @param group If more than one pollutant is chosen, should they all be
##'   plotted on the same graph together? The default is \code{FALSE}, which
##'   means they are plotted in separate panels with their own scaled. If
##'   \code{TRUE} then they are plotted on the same plot with the same scale.
##' @param stack If \code{TRUE} the time series will be stacked by year. This
##'   option can be useful if there are several years worth of data making it
##'   difficult to see much detail when plotted on a single plot.
##' @param normalise Should variables be normalised? The default is is not to
##'   normalise the data. \code{normalise} can take two values, either
##'   \dQuote{mean} or a string representing a date in UK format e.g.
##'   "1/1/1998" (in the format dd/mm/YYYY). If \code{normalise = "mean"} then
##'   each time series is divided by its mean value.  If a date is chosen, then
##'   values at that date are set to 100 and the rest of the data scaled
##'   accordingly. Choosing a date (say at the beginning of a time series) is
##'   very useful for showing how trends diverge over time. Setting \code{group
##'   = TRUE} is often useful too to show all time series together in one
##'   panel.
##' @param avg.time This defines the time period to average to. Can be
##' \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day},
##' \dQuote{DSTday}, \dQuote{week}, \dQuote{month}, \dQuote{quarter}
##' or \dQuote{year}. For much increased flexibility a number can
##' precede these options followed by a space. For example, a
##' timeAverage of 2 months would be \code{period = "2 month"}. See
##' function \code{timeAverage} for further details on this.
##' @param data.thresh The data capture threshold to use (\%) when aggregating
##'   the data using \code{avg.time}. A value of zero means that all available
##'   data will be used in a particular period regardless if of the number of
##'   values available. Conversely, a value of 100 will mean that all data will
##'   need to be present for the average to be calculated, else it is recorded
##'   as \code{NA}. Not used if \code{avg.time = "default"}.
##' @param statistic The statistic to apply when aggregating the data;
##' default is the mean. Can be one of \dQuote{mean}, \dQuote{max},
##' \dQuote{min}, \dQuote{median}, \dQuote{frequency}, \dQuote{sd},
##' \dQuote{percentile}. Note that \dQuote{sd} is the standard
##' deviation and \dQuote{frequency} is the number (frequency) of
##' valid records in the period.  \dQuote{percentile} is the
##' percentile level (\%) between 0-100, which can be set using the
##' \dQuote{percentile} option - see below. Not used if \code{avg.time
##' = "default"}.
##' @param percentile The percentile level in \% used when \code{statistic =
##'   "percentile"} and when aggregating the data with \code{avg.time}. More
##'   than one percentile level is allowed for \code{type = "default"} e.g.
##'   \code{percentile = c(50, 95)}. Not used if \code{avg.time = "default"}.
##' @param date.pad Should missing data be padded-out? This is useful where a
##'   data frame consists of two or more "chunks" of data with time gaps
##'   between them. By setting \code{date.pad = TRUE} the time gaps between the
##'   chunks are shown properly, rather than with a line connecting each chunk.
##'   For irregular data, set to \code{FALSE}. Note, this should not be set for
##'   \code{type} other than \code{default}.
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
##' Only one \code{type} is currently allowed in \code{timePlot}.
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
##' and \code{RColorBrewer} colours --- see the \code{openair}
##' \code{openColours} function for more details. For user defined the
##' user can supply a list of colour names recognised by R (type
##' \code{colours()} to see the full list). An example would be
##' \code{cols = c("yellow", "green", "blue")}
##' @param plot.type The \code{lattice} plot type, which is a line
##'   (\code{plot.type = "l"}) by default. Another useful option is
##'   \code{plot.type = "h"}, which draws vertical lines.
##' @param key Should a key be drawn? The default is \code{TRUE}.
##' @param log Should the y-axis appear on a log scale? The default is
##'   \code{FALSE}. If \code{TRUE} a well-formatted log10 scale is used. This
##'   can be useful for plotting data for several different pollutants that
##'   exist on very different scales. It is therefore useful to use \code{log =
##'   TRUE} together with \code{group = TRUE}.
##' @param smooth Should a smooth line be applied to the data? The default is
##'   \code{FALSE}.
##' @param ci If a smooth fit line is applied, then \code{ci} determines
##'   whether the 95\% confidence intervals aer shown.
##' @param y.relation This determines how the y-axis scale is plotted. "same"
##'   ensures all panels use the same scale and "free" will use panel-specfic
##'   scales. The latter is a useful setting when plotting data with very
##'   different values.
##' @param ref.x See \code{ref.y} for details. In this case the
##' correct date format should be used for a vertical line e.g.  \code{ref.x
##' = list(v = as.POSIXct("2000-06-15"), lty = 5)}.
##' @param ref.y A list with details of the horizontal lines to be
##' added representing reference line(s). For example, \code{ref.y =
##' list(h = 50, lty = 5)} will add a dashed horizontal line at
##' 50. Several lines can be plotted e.g. \code{ref.y = list(h = c(50,
##' 100), lty = c(1, 5), col = c("green", "blue"))}. See
##' \code{panel.abline} in the \code{lattice} package for more details
##' on adding/controlling lines.
##' @param key.columns Number of columns to be used in the key. With many
##'   pollutants a single column can make to key too wide. The user can thus
##'   choose to use several columns by setting \code{columns} to be less than
##'   the number of pollutants.
##' @param name.pol This option can be used to give alternative names
##' for the variables plotted. Instead of taking the column headings
##' as names, the user can supply replacements. For example, if a
##' column had the name \dQuote{nox} and the user wanted a different
##' description, then setting \code{name.pol = "nox before change"}
##' can be used. If more than one pollutant is plotted then use
##' \code{c} e.g. \code{name.pol = c("nox here", "o3 there")}.
##' @param date.breaks Number of major x-axis intervals to use. The function
##'   will try and choose a sensible number of dates/times as well as
##'   formatting the date/time appropriately to the range being considered.
##'   This does not always work as desired automatically. The user can
##'   therefore increase or decrease the number of intervals by adjusting the
##'   value of \code{date.breaks} up or down.
##' @param date.format This option controls the date format on the
##' x-axis. While \code{timePlot} generally sets the date format
##' sensibly there can be some situations where the user wishes to
##' have more control. For format types see \code{strptime}. For
##' example, to format the date like \dQuote{Jan-2012} set
##' \code{date.format = "\%b-\%Y"}.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##' \code{TRUE} titles and axis labels will automatically try and
##' format pollutant names and units properly e.g.  by subscripting
##' the \sQuote{2} in NO2.
##' @param ... Other graphical parameters are passed onto \code{cutData} and
##'   \code{lattice:xyplot}. For example, \code{timePlot} passes the option
##'   \code{hemisphere = "southern"} on to \code{cutData} to provide southern
##'   (rather than default northern) hemisphere handling of \code{type = "season"}.
##'   Similarly, most common plotting parameters, such as \code{layout} for
##'   panel arrangement and \code{pch} and \code{cex} for plot symbol type and
##'   size and \code{lty} and \code{lwd} for line type and width, as passed
##'   to \code{xyplot}, although some maybe locally managed by \code{openair}
##'   on route, e.g. axis and title labelling options (such as \code{xlab},
##'   \code{ylab}, \code{main}) are passed via \code{quickText}
##'   to handle routine formatting. See examples below.
##' @export
##' @return As well as generating the plot itself, \code{timePlot} also returns
##'   an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- timePlot(mydata, "nox")}, this output can be used to
##'   recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}.
##' @author David Carslaw
##' @seealso \code{\link{TheilSen}}, \code{\link{smoothTrend}},
##'   \code{\link{linearRelation}}, \code{\link{selectByDate}} and
##'   \code{\link{timeAverage}} for details on selecting averaging times and
##'   other statistics in a flexible way
##' @keywords methods
##' @examples
##'
##'
##' # basic use, single pollutant
##' timePlot(mydata, pollutant = "nox")
##'
##' # two pollutants in separate panels
##' \dontrun{timePlot(mydata, pollutant = c("nox", "no2"))}
##'
##' # two pollutants in the same panel with the same scale
##' \dontrun{timePlot(mydata, pollutant = c("nox", "no2"), group = TRUE)}
##'
##' # alternative by normalising concentrations and plotting on the same
##'   scale
##' \dontrun{
##' timePlot(mydata, pollutant = c("nox", "co", "pm10", "so2"), group = TRUE, avg.time =
##'   "year", normalise = "1/1/1998", lwd = 3, lty = 1)
##' }
##'
##' # examples of selecting by date
##'
##' # plot for nox in 1999
##' \dontrun{timePlot(selectByDate(mydata, year = 1999), pollutant = "nox")}
##'
##' # select specific date range for two pollutants
##' \dontrun{
##' timePlot(selectByDate(mydata, start = "6/8/2003", end = "13/8/2003"),
##' pollutant = c("no2", "o3"))
##' }
##'
##' # choose different line styles etc
##' \dontrun{timePlot(mydata, pollutant = c("nox", "no2"), lty = 1)}
##'
##' # choose different line styles etc
##' \dontrun{
##' timePlot(selectByDate(mydata, year = 2004, month = 6), pollutant =
##' c("nox", "no2"), lwd = c(1, 2), col = "black")
##' }
##'
##' # different averaging times
##'
##' #daily mean O3
##' \dontrun{timePlot(mydata, pollutant = "o3", avg.time = "day")}
##'
##' # daily mean O3 ensuring each day has data capture of at least 75%
##' \dontrun{timePlot(mydata, pollutant = "o3", avg.time = "day", data.thresh = 75)}
##'
##' # 2-week average of O3 concentrations
##' \dontrun{timePlot(mydata, pollutant = "o3", avg.time = "2 week")}
##'
timePlot <- function(mydata, pollutant = "nox", group = FALSE, stack = FALSE,
                     normalise = NULL, avg.time = "default", data.thresh = 0,
                     statistic = "mean", percentile = NA, date.pad = FALSE,
                     type = "default", cols = "brewer1", plot.type = "l",
                     key = TRUE, log = FALSE, smooth = FALSE, ci = TRUE,
                     y.relation = "same", ref.x = NULL, ref.y = NULL,
                     key.columns = 1, name.pol = pollutant, date.breaks = 7,
                     date.format = NULL, auto.text = TRUE, ...)   {


    ## basic function to plot single/multiple time series in flexible ways
    ## optionally includes several pre-deifined averaging periods
    ## can deal with wide range of date/time formats e.g. minute, 15-min, hourly, daily

    ## note that in teh case of type "site", each site is thought of as a "pollutant"

    ## Author: David Carslaw 11 Sep. 09
    ## CHANGES:

    ## get rid of R check annoyances
    variable = year = NULL

    ## # EXPERIMENTAL LOG SCALING###############################################
    if(log) nlog <- 10 else nlog <- FALSE

    ## #################################################################################

    vars <- c("date", pollutant)

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## reset strip color on exit
    current.strip <- trellis.par.get("strip.background")
    on.exit(trellis.par.set("strip.background", current.strip))

    ## ################################################################################

    ## extra.args setup
    extra.args <- list(...)

    #label controls
    #(further xlab handling in code body)
    extra.args$xlab <- if("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else quickText("", auto.text)
    extra.args$ylab <- if("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else NULL
    extra.args$main <- if("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)

    xlim <- if ("xlim" %in% names(extra.args))
        extra.args$xlim else  NULL

    if(!"pch" %in% names(extra.args))
        extra.args$pch <- NA
    if(!"lwd" %in% names(extra.args))
        extra.args$lwd <- 1
    if(!"lty" %in% names(extra.args))
        extra.args$lty <- NULL

    ## layout
    ## (type and group handling in code body)
    if(!"layout" %in% names(extra.args))
        extra.args$layout <- NULL

    ## strip
    ## extensive handling in main code body)
    strip <- if("strip" %in% names(extra.args))
        extra.args$strip else TRUE

    ## ### warning messages and other checks ################################################

    ## also ckeck that column "site" is present when type set to "default"
    ## but also check to see if dates are duplicated, if not, OK to proceed
    len.all <- length(mydata$date)
    len.unique <- length(unique(mydata$date))

    if (type == "default" & "site" %in% names(mydata) & len.all != len.unique) {
        if (length(unique(factor(mydata$site))) > 1) stop("More than one site has been detected: choose type = 'site' and pollutant(s)")
    }

    if (length(percentile) > 1 & length(pollutant) > 1) {stop("Only one pollutant allowed when considering more than one percentile")}

    if (!missing(statistic) & missing(avg.time)) {
        message("No averaging time applied, using avg.time ='month'")
        avg.time <- "month"}

    ## #######################################################################################

    ## data checks
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    ## pad out any missing date/times so that line don't extend between areas of missing data

    theStrip <- strip

    if (date.pad) mydata <- date.pad(mydata)

    mydata <- cutData(mydata, type, ...)

    ## average the data if necessary (default does nothing)
    if (avg.time != "default") {
        ## deal with mutiple percentile values

        if (length(percentile) > 1) {

            mydata <- ddply(mydata, type, calcPercentile, pollutant = pollutant,
                            avg.time = avg.time, data.thresh = data.thresh,
                            percentile = percentile)

            pollutant <-  paste("percentile.", percentile,  sep = "")

            if (missing(group)) group <- TRUE

        } else {

            mydata <- ddply(mydata, type, timeAverage, avg.time = avg.time,
                            statistic = statistic, percentile = percentile,
                            data.thresh = data.thresh, ...)
        }
    }

    mydata <- melt(mydata, id.var = c("date", type))

    if (type != "default")  group <- TRUE ## need to group pollutants if conditioning

    ## number of pollutants (or sites for type = "site")
    npol <- length(unique(mydata$variable)) ## number of pollutants

    ## layout - stack vertically
    if (is.null(extra.args$layout) & !group & !stack) extra.args$layout <- c(1, npol)

    ## function to normalise data ##################################
    divide.by.mean <- function(x) {
        Mean <- mean(x$value, na.rm = TRUE)
        x$value <- x$value / Mean
        x
    }

    ## function to normalise data by a specfic date ##################################
    norm.by.date <- function(x, thedate){
        ## nearest date in time series
        ## need to find first non-missing value
        temp <- na.omit(x)
        id <-  which(abs(temp$date - thedate) == min(abs(temp$date - thedate)))
        id <- temp$date[id] ## the nearest date for non-missing data
        x$value <- 100 * x$value / x$value[x$date == id]
        x
    }

    ## need to check the ylab handling below
    ## not sure what was meant

    if (!missing(normalise)) {

        if(is.null(extra.args$ylab))
            extra.args$ylab <- "normalised level"

        if (normalise == "mean") {

            mydata <-  ddply(mydata, .(variable), divide.by.mean)

        } else {

            ## scale value to 100 at specific date

            thedate <- as.POSIXct(strptime(normalise, format = "%d/%m/%Y", tz = "GMT"))
            mydata <- ddply(mydata, .(variable), norm.by.date, thedate = thedate)
        }
    }

    #set ylab as pollutant(s) if not already set
    if(is.null(extra.args$ylab))
        extra.args$ylab <- quickText(paste(pollutant, collapse = ", "), auto.text)


    mylab <- sapply(seq_along(pollutant), function(x) quickText(pollutant[x], auto.text))

    ## user-supplied names
    if (!missing(name.pol)) {mylab <- sapply(seq_along(name.pol), function(x)
                                             quickText(name.pol[x], auto.text))
                         }

    ## set up colours
    myColors <- if (length(cols) == 1 && cols == "greyscale")
        openColours(cols, npol + 1)[-1] else openColours(cols, npol)

    ## basic function for lattice call + defaults
    myform <- formula(paste("value ~ date |", type))

    if(is.null(extra.args$strip))
        strip <- TRUE

    strip.left <- FALSE

    dates <- dateBreaks(mydata$date, date.breaks)$major ## for date scale

    ## date axis formating
    if (is.null(date.format)) {
        formats <- dateBreaks(mydata$date, date.breaks)$format
    } else {
        formats <- date.format
    }

    scales <- list(x = list(at = dates, format = formats),
                   y = list(log = nlog, relation = y.relation, rot = 0))

    ## layout changes depening on plot type

    if (!group) { ## sepate panels per pollutant
        if(is.null(extra.args$strip))
            strip <- FALSE

        myform <- formula("value ~ date | variable")

        if (npol == 1) {
            strip.left <- FALSE
        } else {
            strip.left <- strip.custom(par.strip.text = list(cex = 0.9), horizontal = FALSE,
                                       factor.levels = mylab)
        }

        scales <- list(x = list(at = dates, format = formats), y = list(relation = "free",
                                                               rot = 0, log = nlog))

        if (is.null(extra.args$lty)) extra.args$lty <- 1 ## don't need different line types here
    }

    ## set lty if not set by this point
    if(is.null(extra.args$lty))
        extra.args$lty <- 1:length(pollutant)

    if (type == "default") strip <- FALSE

    ## if stacking of plots by year is needed
    if (stack) {
        mydata$year <- format(mydata$date, "%Y")
        if(is.null(extra.args$layout))
            extra.args$layout <- c(1, length(unique(mydata$year)))
        strip <- FALSE
        myform <- formula("value ~ date | year")
        strip.left <- strip.custom(par.strip.text = list(cex = 0.9), horizontal = FALSE)
        ##  dates <- unique(dateTrunc(mydata$date, "months")) - this does not work?
        dates <- as.POSIXct(unique(paste(format(mydata$date, "%Y-%m"), "-01", sep ="")), "GMT")

        scales <- list(x = list(at = dates, format = "%d-%b", relation = "sliced"), y = list(log = nlog))

        xlim <- dlply(mydata, .(year), function (x) range(x$date))

    }

    if (missing(key.columns)) key.columns <- npol

    ## keys and strips - to show or not

    if (key) {
        ## type of key depends on whether points are plotted or not
        if (any(!is.na(extra.args$pch))) {
            key <- list(lines = list(col = myColors[1:npol], lty = extra.args$lty,
                        lwd = extra.args$lwd), points = list(pch = extra.args$pch,
                                               col = myColors[1:npol]),
                        text = list(lab = mylab),  space = "bottom", columns = key.columns)
        } else {
            key <- list(lines = list(col = myColors[1:npol], lty = extra.args$lty,
                        lwd = extra.args$lwd),
                        text = list(lab = mylab),  space = "bottom", columns = key.columns)
        }
    } else {
        key <- NULL ## either there is a key or there is not
    }

    if (theStrip) {
        strip <- strip
        strip.left <- strip.left
    } else {
        strip <- FALSE
        strip.left <- FALSE
    }

    ## special layout if type = "wd"
    if (length(type) == 1 & type[1] == "wd" & is.null(extra.args$layout)) {
        ## re-order to make sensible layout
        wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
        mydata$wd <- ordered(mydata$wd, levels = wds)

        ## see if wd is actually there or not
        wd.ok <- sapply(wds, function (x) {if (x %in% unique(mydata$wd)) FALSE else TRUE })
        skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])

        mydata$wd <- factor(mydata$wd)  ## remove empty factor levels

        extra.args$layout <- c(3, 3)
        if(!"skip" %in% names(extra.args))
            extra.args$skip <- skip
    }
    if(!"skip" %in% names(extra.args))
         extra.args$skip <- FALSE

    ## allow reasonable gaps at ends, default has too much padding
    gap <- difftime(max(mydata$date), min(mydata$date), units = "secs") / 80
    if (is.null(xlim)) xlim <- range(mydata$date) + c(-1 * gap, gap)

    #the plot
    xyplot.args <- list(x = myform,  data = mydata, groups = mydata$variable,
                        as.table = TRUE,
                        par.strip.text = list(cex = 0.8),
                        scales = scales,
                        key = key,
                        xlim = xlim,
                        strip = strip,
                        strip.left = strip.left,
                        yscale.components = yscale.components.log10ticks,
                        panel =  panel.superpose,
                        panel.groups = function(x, y, col.line, col.symbol, col, col.se, type,
                        group.number, lty, lwd, pch, subscripts,...) {

                            if (group.number == 1) {
                                panel.grid(-1, 0)
                                panel.abline(v = dates, col = "grey90")

                            }
                            if (!group & !stack) {
                                panel.abline(v = dates, col = "grey90")
                                panel.grid(-1, 0)
                            }

                            panel.xyplot(x, y, type = plot.type, lty = lty, lwd = lwd, pch = pch,
                                         col.line = myColors[group.number],...)
                            ## deal with points separately - useful if missing data where line
                            ## does not join consequtive points
                            if (any(!is.na(extra.args$pch))) {

                                lpoints(x, y, type = "p", pch = extra.args$pch[group.number],
                                        col.symbol = myColors[group.number],...)
                            }
                            if (smooth) panel.gam(x, y, col = myColors[group.number] ,
                                                  col.se =  myColors[group.number],
                                                  lty = 1, lwd = 1, se = ci, k = NULL, ...)

                            ## add reference lines

                            if (!is.null(ref.x)) do.call(panel.abline, ref.x)
                            if (!is.null(ref.y)) do.call(panel.abline, ref.y)

                        })

    ## reset for extra.args
    xyplot.args<- listUpdate(xyplot.args, extra.args)

    #plot
    plt <- do.call(xyplot, xyplot.args)

    ## output

    plot(plt)
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    invisible(output)

}



