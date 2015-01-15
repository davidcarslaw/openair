##' Linear relations between pollutants
##'
##' This function considers linear relationships between two pollutants. The
##' relationships are calculated on different times bases using a linear model.
##' The slope and 95% confidence interval in slope relationships by time unit
##' are plotted in many ways. The function is particularly useful when
##' considering whether relationships are consistent with emissions
##' inventories.
##'
##'
##' The relationships between pollutants can yield some very useful information
##' about source emissions and how they change. A scatterPlot between two
##' pollutants is the usual way to investigate the relationship. A linear
##' regression is useful to test the strength of the relationship. However,
##' considerably more information can be gleaned by considering different time
##' periods, such as how the relationship between two pollutants vary over
##' time, by day of the week, diurnally and so on. The \code{linearRelation}
##' function does just that - it fits a linear relationship between two
##' pollutants over a wide range of time periods determined by \code{period}.
##'
##' \code{linearRelation} function is particularly useful if background
##' concentrations are first removed from roadside concentrations, as the
##' increment will relate more directly with changes in emissions. In this
##' respect, using \code{linearRelation} can provide valuable information on
##' how emissions may have changed over time, by hour of the day etc. Using the
##' function in this way will require users to do some basic manipulation with
##' their data first.
##'
##' If a data frame is supplied that contains \code{nox}, \code{no2} and
##' \code{o3}, the \code{y} can be chosen as \code{y = "ox"}. In function will
##' therefore consider total oxidant slope (sum of NO2 + O3), which can provide
##' valuable information on likely vehicle primary NO emissions. Note, however,
##' that most roadside sites do not have ozone measurements and
##' \code{\link{calcFno2}} is the alternative.
##'
##' A smooth line is added to all plots except \code{period =
##' "weekday"} using \code{smooth.spline}. \code{smooth.spline} uses
##' Generalized Cross Validation (GCV) to find a line that is neither
##' too wiggly nor too smooth. The line tends to highlight both the
##' overall tendency of a relationship as well as some of the more
##' interesting features.
##'
##' @param mydata A data frame minimally containing \code{date} and two
##'   pollutants.
##' @param x First pollutant that when plotted would appear on the x-axis of a
##'   relationship e.g. \code{x = "nox"}.
##' @param y Second pollutant that when plotted would appear on the y-axis of a
##'   relationship e.g. \code{y = "pm10"}.
##' @param period A range of different time periods can be
##' analysed. \dQuote{monthly} will plot a monthly time series and
##' \dQuote{weekly} a weekly time series of the relationship between
##' \code{x} and \code{y}. "hour" will show the diurnal relationship
##' between \code{x} and \code{y} and \dQuote{weekday} the day of the
##' week relationship between \code{x} and \code{y}. \dQuote{day.hour}
##' will plot the relationship by weekday and hour of the day.
##' @param condition For \code{period = "hour"}, \code{period = "day"} and
##'   \code{period = "day.hour"}, setting \code{condition = TRUE} will plot the
##'   relationships split by year. This is useful for seeing how the
##'   relationships may be changing over time.
##' @param n The minimum number of points to be sent to the linear model.
##'   Because there may only be a few points e.g. hours where two pollutants
##'   are available over one week, \code{n} can be set to ensure that at least
##'   \code{n} points are sent to the linear model. If a period has hours <
##'   \code{n} that period will be ignored.
##' @param rsq.thresh The minimum correlation coefficient (R2) allowed. If the
##'   relationship between \code{x} and \code{y} is not very good for a
##'   particular period, setting \code{rsq.thresh} can help to remove those
##'   periods where the relationship is not strong. Any R2 values below
##'   \code{rsq.thresh} will not be plotted. If set too high it may not be
##'   possible to fit a smooth line and warnings will be issues - but the plot
##'   still produced.
##' @param ylim y-axis limits, specified by the user.
##' @param ylab y-axis title, specified by the user.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the \sQuote{2}
##'   in NO2.
##' @param cols Predefined colour scheme, currently only enabled for
##'   \dQuote{greyscale}.
##' @param span span for \code{loess} fit. Controls the fit line: lower values
##'   produce a more \dQuote{wiggly} fit.
##' @param \dots Other graphical parameters. A useful one to remove the strip
##'   with the date range on at the top of the plot is to set \code{strip =
##'   FALSE}.
##' @export
##' @return As well as generating the plot itself, \code{linearRelation} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- linearRelation(mydata, "nox", "no2")}, this output can be
##'   used to recover the data, reproduce or rework the original plot or
##'   undertake further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}.
##' @author David Carslaw
##' @seealso \code{\link{calcFno2}}
##' @keywords methods
##' @examples
##'
##'
##' # monthly relationship between NOx and SO2 - note rapid fall in
##' # ratio at the beginning of the series
##' linearRelation(mydata, x = "nox", y = "so2")
##' # monthly relationship between NOx and SO2 - note rapid fall in
##' # ratio at the beginning of the series
##' \dontrun{linearRelation(mydata, x = "nox", y = "ox")}
##'
##' # diurnal oxidant slope by year # clear change in magnitude
##' # starting 2003, but the diurnal profile has also changed: the
##' # morning and evening peak hours are more important, presumably due
##' # to change in certain vehicle types
##' \dontrun{linearRelation(mydata, x = "nox", y = "ox", period = "hour", condition = TRUE)}
##'
##' # PM2.5/PM10 ratio, but only plot where monthly R2 >= 0.8
##' \dontrun{linearRelation(mydata, x = "pm10", y = "pm25", rsq.thresh = 0.8)}
##'
##'
linearRelation <- function(mydata,
                            x = "nox",
                            y = "no2",
                            period = "monthly",
                            condition = FALSE,
                            n = 20,
                            rsq.thresh = 0,
                            ylim = c(0, 20),
                            ylab = paste("slope from ", y, " = m.", x, " + c", sep = ""),
                            auto.text = TRUE,
                            cols = NULL,
                            span = 0.3,...) {

    ## get rid of R check annoyances
    nox = ox = cond = rsquare = N = r.thresh = NULL

    adj <- 1 ## factors for ratios (oxidant is a percentage)

     ## reset strip color on exit
    current.strip <- trellis.par.get("strip.background")
    on.exit(trellis.par.set("strip.background", current.strip))

    #greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
        #other local colours
        data.col <- "darkgrey"
        line.col <- "black"
    } else {
        data.col <- "#3366FF"
        line.col <- "red"
    }


    ##extra.args setup
    extra.args <- list(...)

    #label controls
    ##ylab handled in args because unique
    ##further xlab handled in code because mulitple outputs by period
    extra.args$xlab <- if("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else NULL
    extra.args$main <- if("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)


    ## prepare data
    if ("ox" %in% tolower(c(x, y))) {
        vars <- c("date", "nox", "no2", "ox")
        mydata$ox <- mydata$no2 + mydata$o3
        mydata <- subset(mydata, nox > 0 & ox > 0)
        if (missing(ylab)) ylab <- "f-no2 (%) by vol."
        adj <- 100
    } else {
        vars <- c("date", x, y)
    }

    mydata <- checkPrep(mydata, vars, "default", remove.calm = FALSE)
    mydata <- na.omit(mydata)

    if (!condition) {
        mydata$cond <- paste(format(min(mydata$date), "%d/%/%m/%Y"),
                             " to ", format(max(mydata$date), "%d/%/%m/%Y"))
    } else {   ## condition by year
        mydata$cond <- format(mydata$date, "%Y")
    }

    model <- function(df) { lm(eval(paste(y, "~", x)), data = df) }
    rsq <- function(x) summary(x)$r.squared
    seslope <-  function(x) { if (nrow(summary(x)$coefficients) == 2) {
        2 * summary(x)$coefficients[2, 2]  ## 95 % CI; need two rows in coefs
    } else {
        NA
    }}
    len <-  function(x) nrow(x$model)

    ## y range taking account of expanded uncertainties
    rng <- function(x) {
        lims <- range(c(x$slope - x$seslope, x$slope + x$seslope), na.rm = TRUE)
        inc <- 0.04 * abs(lims[2] - lims[1])
        lims <- c(lims[1] - inc, lims[2] + inc)
        lims
    }
################################################################################################
    if (period == "hour") {

        #xlab default
        if(is.null(extra.args$xlab))
            extra.args$xlab <- "hour"

        models <- dlply(mydata, .(cond, hour = as.numeric(format(date, "%H"))), model)
        results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
        names(results) <- c("cond", "hour", "intercept", "slope", "rsquare", "seslope", "N")
        results$slope <- results$slope * adj
        results$seslope <- results$seslope * adj
        results <- subset(results, rsquare >= rsq.thresh & N >= n)

        eq <- formula(slope ~ hour)
        if (condition) eq <- formula(slope ~ hour | cond)
        if (missing(ylim)) ylim <- rng(results)

        xyplot.args <- list(x = eq, data = results,
                      as.table = TRUE,
                      ylim = ylim,
                      ylab = quickText(ylab, auto.text),
                      scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                          panel.xyplot(x, y, col = data.col, pch = 16,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = data.col,
                                         lwd = 2)
                      })

        #reset for extra.args
        xyplot.args<- listUpdate(xyplot.args, extra.args)

        #plot
        plt <- do.call(xyplot, xyplot.args)
    }
################################################################################################

    if (period == "monthly" | period == "weekly") {

        #xlab default
        if(is.null(extra.args$xlab))
            extra.args$xlab <- "year"

        if (period == "monthly") {
            models <- dlply(mydata, .(cond, year = as.numeric(format(date, "%Y")),
                                      month = as.numeric(format(date, "%m"))), model)
            results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
            names(results) <- c("cond", "year", "month", "intercept", "slope",
                                "rsquare", "seslope", "N")
            results$slope <- results$slope * adj
            results$seslope <- results$seslope * adj
            results$date <- ISOdate(results$year, results$month, 15)

        } else {
            models <- dlply(mydata, .(cond, year = as.numeric(format(date, "%Y")),
                                      month = as.numeric(format(date, "%U"))), model)
            results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
            names(results) <- c("cond", "year", "week", "intercept", "slope",
                                "rsquare", "seslope", "N")
            results$slope <- results$slope * adj
            results$seslope <- results$seslope * adj

            day <- round(results$week * 7) + 1
            day[day > 366] <- 366
            dates <- paste(results$year, day, sep = "-")
            results$date <- as.POSIXct(strptime(dates, "%Y-%j"), "GMT")
        }

        results <- subset(results, rsquare >= rsq.thresh & N >= n)
        start.year <- as.numeric(format(min(results$date), "%Y"))
        end.year <- as.numeric(format(max(results$date), "%Y"))

        yrs <- seq(ISOdate(start.year, 1, 1), ISOdate(end.year + 1, 1, 1), by = "1 years")
        if (missing(ylim)) ylim <- rng(results)

        xyplot.args <- list(x = slope ~ date, data = results,
                      ylim = ylim,
                      ylab = quickText(ylab, auto.text),
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = yrs, col = "grey85")
                          panel.xyplot(x, y, col = data.col,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = data.col)
                          panel.loess(x, y, col = line.col, lwd = 2, span = span)
                      })

        #reset for extra.args
        xyplot.args<- listUpdate(xyplot.args, extra.args)

        #plot
        plt <- do.call(xyplot, xyplot.args)
    }
################################################################################################

    if (period == "weekday") {

        #xlab default
        if(is.null(extra.args$xlab))
            extra.args$xlab <- "weekday"

        models <- dlply(mydata, .(cond, weekday = format(date, "%a")), model)
        results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
        names(results) <- c("cond", "weekday", "intercept", "slope", "rsquare", "seslope", "N")
        results <- subset(results, rsquare >= rsq.thresh & N >= n)
        results$slope <- results$slope * adj
        results$seslope <- results$seslope * adj

        results$weekday <- ordered(results$weekday, levels = format(ISOdate(2000, 1, 3:9), "%a"))
        if (missing(ylim)) ylim <- rng(results)


        if (condition) {
            myform <- formula("x = slope ~ weekday | cond")
        } else {
            myform <- formula("x = slope ~ weekday")
        }

        xyplot.args <- list(myform, data = results,
                      as.table = TRUE,
                      ylim = ylim,
                      ylab = quickText(ylab, auto.text),
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = 1:7, col = "grey85")
                          panel.xyplot(x, y, col = data.col, pch = 16,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = data.col,
                                         lwd = 2)
                      })

        #reset for extra.args
        xyplot.args<- listUpdate(xyplot.args, extra.args)

        #plot
        plt <- do.call(xyplot, xyplot.args)
    }
################################################################################################

    if (period == "day.hour") {

        #xlab default
        if(is.null(extra.args$xlab))
            extra.args$xlab <- "hour"

        models <- dlply(mydata, .(cond, weekday = format(date, "%A"),
                                  hour = as.numeric(format(date, "%H"))), model)
        results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
        names(results) <- c("cond", "weekday", "hour", "intercept", "slope",
                            "rsquare", "seslope", "N")
        results$slope <- results$slope * adj
        results$seslope <- results$seslope * adj
        results <- subset(results, rsquare >= rsq.thresh & N >= n)
        results$weekday <- ordered(results$weekday, levels = format(ISOdate(2000, 1, 3:9), "%A"))
        if (nrow(results) == 0) stop("Note enough data to plot. Try reducing 'n'.")

        eq <- formula(slope ~ hour | weekday)
        if (condition) eq <- formula(slope ~ hour | weekday * cond)
        if (missing(ylim)) ylim <- rng(results)

        xyplot.args <- list(x = eq, data = results,
                      as.table = TRUE,
                      layout = c(7, length(unique(results$cond))),
                      ylim = ylim,
                      ylab = quickText(ylab, auto.text),
                      scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                          panel.xyplot(x, y, col = data.col, pch = 16,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = data.col,
                                         lwd = 2)
                      })

        #reset for extra.args
        xyplot.args<- listUpdate(xyplot.args, extra.args)

        #plot
        plt <- do.call(xyplot, xyplot.args)
    }

    if (condition & period == "day.hour") print(useOuterStrips(plt)) else print(plt)

    #################
    #output
    #################
    plt <- trellis.last.object()
    newdata <- results
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    invisible(output)

}
