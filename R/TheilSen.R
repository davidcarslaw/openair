## functions to calculate Mann-Kendall and Sen-Theil slopes
## Uncertainty in slopes are calculated using bootstrap methods
## The block bootstrap used should be regarded as an ongoing development
## see http://www-rcf.usc.edu/~rwilcox/
##
## Author: David Carslaw with Sen-Theil functions from Rand Wilcox
###############################################################################
##' Tests for trends using Theil-Sen estimates
##'
##' Theil-Sen slope estimates and tests for trend.
##'
##' The \code{TheilSen} function provides a collection of functions to
##' analyse trends in air pollution data. The \code{TheilSen} function
##' is flexible in the sense that it can be applied to data in many
##' ways e.g. by day of the week, hour of day and wind direction. This
##' flexibility makes it much easier to draw inferences from data
##' e.g. why is there a strong downward trend in concentration from
##' one wind sector and not another, or why trends on one day of the
##' week or a certain time of day are unexpected.
##'
##' For data that are strongly seasonal, perhaps from a background
##' site, or a pollutant such as ozone, it will be important to
##' deseasonalise the data (using the option \code{deseason =
##' TRUE}.Similarly, for data that increase, then decrease, or show
##' sharp changes it may be better to use \code{\link{smoothTrend}}.
##'
##' A minimum of 6 points are required for trend estimates to be made.
##'
##' Note! that since version 0.5-11 openair uses Theil-Sen to derive
##' the p values also for the slope. This is to ensure there is
##' consistency between the calculated p value and other trend
##' parameters i.e. slope estimates and uncertainties. The p value and
##' all uncertainties are calculated through bootstrap simulations.
##'
##' Note that the symbols shown next to each trend estimate relate to
##' how statistically significant the trend estimate is: p $<$ 0.001 =
##' ***, p $<$ 0.01 = **, p $<$ 0.05 = * and p $<$ 0.1 = $+$.
##'
##' Some of the code used in \code{TheilSen} is based on that from
##' Rand Wilcox \url{http://www-rcf.usc.edu/~rwilcox/}. This mostly
##' relates to the Theil-Sen slope estimates and uncertainties.
##' Further modifications have been made to take account of correlated
##' data based on Kunsch (1989). The basic function has been adapted
##' to take account of auto-correlated data using block bootstrap
##' simulations if \code{autocor = TRUE} (Kunsch, 1989). We follow the
##' suggestion of Kunsch (1989) of setting the block length to n(1/3)
##' where n is the length of the time series.
##'
##' The slope estimate and confidence intervals in the slope are plotted and
##' numerical information presented.
##' 
##' @aliases TheilSen
##' @param mydata A data frame containing the field \code{date} and at
##'   least one other parameter for which a trend test is required;
##'   typically (but not necessarily) a pollutant.
##' @param pollutant The parameter for which a trend test is required.
##'   Mandatory.
##' @param deseason Should the data be de-deasonalized first? If 
##'   \code{TRUE} the function \code{stl} is used (seasonal trend 
##'   decomposition using loess). Note that if \code{TRUE} missing
##'   data are first linearly interpolated because \code{stl} cannot
##'   handle missing data.
##' @param type \code{type} determines how the data are split i.e.
##'   conditioned, and then plotted. The default is will produce a 
##'   single plot using the entire data. Type can be one of the
##'   built-in types as detailed in \code{cutData} e.g.
##'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For
##'   example, \code{type = "season"} will produce four plots --- one
##'   for each season.
##'   
##'   It is also possible to choose \code{type} as another variable in
##'   the data frame. If that variable is numeric, then the data will
##'   be split into four quantiles (if possible) and labelled 
##'   accordingly. If type is an existing character or factor
##'   variable, then those categories/levels will be used directly.
##'   This offers great flexibility for understanding the variation of
##'   different variables and how they depend on one another.
##'   
##'   Type can be up length two e.g. \code{type = c("season", 
##'   "weekday")} will produce a 2x2 plot split by season and day of
##'   the week. Note, when two types are provided the first forms the 
##'   columns and the second the rows.
##' @param avg.time Can be \dQuote{month} (the default), 
##'   \dQuote{season} or \dQuote{year}. Determines the time over which
##'   data should be averaged. Note that for \dQuote{year}, six or
##'   more years are required. For \dQuote{season} the data are split
##'   up into spring: March, April, May etc. Note that December is
##'   considered as belonging to winter of the following year.
##' @param statistic Statistic used for calculating monthly values.
##'   Default is \dQuote{mean}, but can also be \dQuote{percentile}.
##'   See \code{timeAverage} for more details.
##' @param percentile Single percentile value to use if 
##'   \code{statistic = "percentile"} is chosen.
##' @param data.thresh The data capture threshold to use (%) when 
##'   aggregating the data using \code{avg.time}. A value of zero
##'   means that all available data will be used in a particular
##'   period regardless if of the number of values available.
##'   Conversely, a value of 100 will mean that all data will need to
##'   be present for the average to be calculated, else it is recorded
##'   as \code{NA}.
##' @param alpha For the confidence interval calculations of the
##'   slope. The default is 0.05. To show 99\% confidence intervals
##'   for the value of the trend, choose alpha = 0.01 etc.
##' @param dec.place The number of decimal places to display the trend
##'   estimate at. The default is 2.
##' @param xlab x-axis label, by default \code{"year"}.
##' @param lab.frac Fraction along the y-axis that the trend
##'   information should be printed at, default 0.99.
##' @param lab.cex Size of text for trend information.
##' @param x.relation This determines how the x-axis scale is plotted.
##'   \dQuote{same} ensures all panels use the same scale and 
##'   \dQuote{free} will use panel-specfic scales. The latter is a 
##'   useful setting when plotting data with very different values.
##' @param y.relation This determines how the y-axis scale is plotted.
##'   \dQuote{same} ensures all panels use the same scale and 
##'   \dQuote{free} will use panel-specfic scales. The latter is a 
##'   useful setting when plotting data with very different values.
##' @param data.col Colour name for the data
##' @param trend list containing information on the line width, line 
##'   type and line colour for the main trend line and confidence 
##'   intervals respectively.
##' @param text.col Colour name for the slope/uncertainty numeric
##'   estimates
##' @param slope.text The text shown for the slope (default is
##'   \sQuote{units/year}).
##' @param cols Predefined colour scheme, currently only enabled for 
##'   \code{"greyscale"}.
##' @param shade The colour used for marking alternate years. Use 
##'   \dQuote{white} or \dQuote{transparent} to remove shading.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If 
##'   \code{TRUE} titles and axis labels will automatically try and
##'   format pollutant names and units properly e.g.  by subscripting
##'   the \sQuote{2} in NO2.
##' @param autocor Should autocorrelation be considered in the trend 
##'   uncertainty estimates? The default is \code{FALSE}. Generally,
##'   accounting for autocorrelation increases the uncertainty of the
##'   trend estimate --- sometimes by a large amount.
##' @param slope.percent Should the slope and the slope uncertainties 
##'   be expressed as a percentage change per year? The default is 
##'   \code{FALSE} and the slope is expressed as an average units/year
##'   change e.g. ppb.  Percentage changes can often be confusing and 
##'   should be clearly defined.  Here the percentage change is 
##'   expressed as 100 * (C.end/C.start - 1) / (end.year - 
##'   start.year). Where C.start is the concentration at the start 
##'   date and C.end is the concentration at the end date.
##'   
##'   For \code{avg.time = "year"} (end.year - start.year) will be the
##'   total number of years - 1. For example, given a concentration in
##'   year 1 of 100 units and a percentage reduction of 5%/yr, after 5
##'   years there will be 75 units but the actual time span will be 6 
##'   years i.e. year 1 is used as a reference year. Things are
##'   slightly different for monthly values e.g.  \code{avg.time =
##'   "month"}, which will use the total number of months as a basis
##'   of the time span and is therefore able to deal with partial
##'   years.  There can be slight differences in the %/yr trend
##'   estimate therefore, depending on whether monthly or annual
##'   values are considered.
##' @param date.breaks Number of major x-axis intervals to use. The 
##'   function will try and choose a sensible number of dates/times as
##'   well as formatting the date/time appropriately to the range 
##'   being considered. This does not always work as desired
##'   automatically. The user can therefore increase or decrease the
##'   number of intervals by adjusting the value of \code{date.breaks}
##'   up or down.
##' @param ... Other graphical parameters passed onto \code{cutData}
##'   and \code{lattice:xyplot}. For example, \code{TheilSen} passes
##'   the option \code{hemisphere = "southern"} on to \code{cutData}
##'   to provide southern (rather than default northern) hemisphere
##'   handling of \code{type = "season"}. Similarly, common axis and
##'   title labelling options (such as \code{xlab}, \code{ylab},
##'   \code{main}) are passed to \code{xyplot} via \code{quickText} to
##'   handle routine formatting.
##' @export TheilSen
##' @return As well as generating the plot itself, \code{TheilSen}
##'   also returns an object of class ``openair''. The object includes
##'   three main components: \code{call}, the command used to generate
##'   the plot; \code{data}, the data frame of summarised information
##'   used to make the plot; and \code{plot}, the plot itself. If
##'   retained, e.g. using \code{output <- TheilSen(mydata, "nox")},
##'   this output can be used to recover the data, reproduce or rework
##'   the original plot or undertake further analysis.
##'   
##'   An openair output can be manipulated using a number of generic
##'   operations, including \code{print}, \code{plot} and
##'   \code{summary}.
##'   
##'   The \code{data} component of the \code{TheilSen} output includes
##'   two subsets: \code{main.data}, the monthly data \code{res2} the
##'   trend statistics. For \code{output <- TheilSen(mydata, "nox")},
##'   these can be extracted as \code{object$data$main.data} and
##'   \code{object$data$res2}, respectively.
##'
##' Note: In the case of the intercept, it is assumed the y-axis crosses the
##'   x-axis on 1/1/1970.
##' @author David Carslaw with some trend code from Rand Wilcox
##' @seealso See \code{\link{smoothTrend}} for a flexible approach to
##'   estimating trends using nonparametric regression. The \code{smoothTrend}
##'   function is suitable for cases where trends are not monotonic and is
##'   probably better for exploring the shape of trends.
##' @references
##'
##' Helsel, D., Hirsch, R., 2002. Statistical methods in water resources. US
##'   Geological Survey.  \url{http://pubs.usgs.gov/twri/twri4a3/}. Note that
##'   this is a very good resource for statistics as applied to environmental
##'   data.
##'
##' Hirsch, R. M., Slack, J. R., Smith, R. A., 1982. Techniques of trend
##'   analysis for monthly water-quality data. Water Resources Research 18 (1),
##'   107-121.
##'
##' Kunsch, H. R., 1989. The jackknife and the bootstrap for general stationary
##'   observations. Annals of Statistics 17 (3), 1217-1241.
##'
##' Sen, P. K., 1968. Estimates of regression coefficient based on
##' Kendall's tau. Journal of the American Statistical Association
##' 63(324).
##'
##' Theil, H., 1950. A rank invariant method of linear and polynomial
##' regression analysis, i, ii, iii. Proceedings of the Koninklijke
##' Nederlandse Akademie Wetenschappen, Series A - Mathematical
##' Sciences 53, 386-392, 521-525, 1397-1412.
##'
##' \dots{} see also several of the Air Quality Expert Group (AQEG) reports for
##'   the use of similar tests applied to UK/European air quality data, see
##'   \url{http://uk-air.defra.gov.uk/library/aqeg/}.
##' @keywords methods
##' @examples
##'
##'
##' # load example data from package
##' data(mydata)
##'
##' # trend plot for nox
##' TheilSen(mydata, pollutant = "nox")
##'
##' # trend plot for ozone with p=0.01 i.e. uncertainty in slope shown at
##' # 99 % confidence interval
##'
##' \dontrun{TheilSen(mydata, pollutant = "o3", ylab = "o3 (ppb)", alpha = 0.01)}
##'
##' # trend plot by each of 8 wind sectors
##' \dontrun{TheilSen(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")}
##'
##' # and for a subset of data (from year 2000 onwards)
##' \dontrun{TheilSen(selectByDate(mydata, year = 2000:2005), pollutant = "o3", ylab = "o3 (ppb)")}
##'
##'
TheilSen <- function(mydata, pollutant = "nox", deseason = FALSE, 
                     type = "default", avg.time = "month",
                     statistic = "mean", percentile = NA, data.thresh = 0, alpha = 0.05,
                     dec.place = 2, xlab = "year", lab.frac = 0.99, lab.cex = 0.8,
                     x.relation = "same", y.relation = "same", data.col = "cornflowerblue",
                     trend = list(lty = c(1, 5), lwd = c(2, 1), col = c("red", "red")),
                     text.col = "darkgreen", slope.text = NULL, cols = NULL, 
                     shade = "grey95", auto.text = TRUE,
                     autocor = FALSE, slope.percent = FALSE, date.breaks = 7,...)  {
  
  ## get rid of R check annoyances
  a = b = lower.a = lower.b = upper.a = upper.b = slope.start = date.end = intercept.start = date.start = lower.start = intercept.lower.start = upper.start = intercept.upper.start = NULL
  
  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  ## reset graphic parameters
  on.exit(trellis.par.set(strip.background = current.strip,
                          fontsize = current.font))
  
  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    
    trellis.par.set(list(strip.background = list(col = "white")))
    ## other local colours
    trend$col <- c("black", "black")
    data.col <- "darkgrey"
    text.col <- "black"
  } else {
    
    data.col <- data.col
    text.col <- text.col
  }
  
  ## extra.args setup
  extra.args <- list(...)
  
  ## label controls
  ## (xlab currently handled in plot because unqiue action)
  extra.args$ylab <- if ("ylab" %in% names(extra.args))
    quickText(extra.args$ylab, auto.text) else quickText(pollutant, auto.text)
  
  extra.args$main <- if ("main" %in% names(extra.args))
    quickText(extra.args$main, auto.text) else quickText("", auto.text)
  
  if ("fontsize" %in% names(extra.args))
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  
  xlim <- if ("xlim" %in% names(extra.args))
    extra.args$xlim else  NULL
  
  ## layout default
  if(!"layout" %in% names(extra.args))
    extra.args$layout <- NULL
  
  vars <- c("date", pollutant)
  
  
  if (!avg.time %in% c("year", "month", "season")) 
    stop ("avg.time can only be 'month', 'season' or 'year'.")
  
  ## if data clearly annual, then assume annual
  interval <- find.time.interval(mydata$date)
  interval <- as.numeric(strsplit(interval, split = " ")[[1]][1])   
  if (round(interval / 8760) == 3600) avg.time <- "year"
  
  ## data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  
  ## date formatting for plot
  date.at <- as.Date(dateBreaks(mydata$date, date.breaks)$major)
  date.format <- dateBreaks(mydata$date)$format
  
  
  ## cutData depending on type
  mydata <- cutData(mydata, type, ...)
  
  ## for overall data and graph plotting
  start.year <- startYear(mydata$date)
  end.year <-   endYear(mydata$date)
  start.month <-  startMonth(mydata$date)
  end.month <-   endMonth(mydata$date)
  
  mydata <- timeAverage(mydata, type = type, 
                        avg.time = avg.time, 
                        statistic = statistic, 
                        percentile = percentile,
                        data.thresh = data.thresh)
  
  # timeAverage drops type if default
  if (type == "default") mydata$default <- "default"
  
  
  process.cond <- function(mydata) {
    
    if (all(is.na(mydata[[pollutant]]))) return()       
    
    ## sometimes data have long trailing NAs, so start and end at
    ## first and last data
    min.idx <- min(which(!is.na(mydata[, pollutant])))
    max.idx <- max(which(!is.na(mydata[, pollutant])))
    mydata <- mydata[min.idx:max.idx, ]
    
    ## these subsets may have different dates to overall
    start.year <-  startYear(mydata$date)
    end.year <-   endYear(mydata$date)
    start.month <-  startMonth(mydata$date)
    end.month <-   endMonth(mydata$date)
    
    if (avg.time == "month") {
      
      mydata$date <- as.Date(mydata$date)
      
      deseas <- mydata[[pollutant]]
      
      ## can't deseason less than 2 years of data
      if (nrow(mydata) <= 24) deseason <- FALSE
      
      if (deseason) {
        ## interpolate missing data
        mydata[[pollutant]] <- approx(mydata[[pollutant]], 
                                      n = length(mydata[[pollutant]]))$y
        
        myts <- ts(mydata[[pollutant]], start = c(start.year, start.month),
                   end = c(end.year, end.month), frequency = 12)
        ## key thing is to allow the seanonal cycle to vary, hence
        ## s.window should not be "periodic"; set quite high to avoid
        ## overly fitted seasonal cycle
        ## robustness also makes sense for sometimes noisy data
        ssd <- stl(myts, s.window = 35, robust = TRUE, s.degree = 0)
        
        deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]
        
        deseas <- as.vector(deseas)
      }
      
      all.results <- data.frame(date = mydata$date, conc = deseas,
                                stringsAsFactors = FALSE)
      results <- na.omit(all.results)
      
      
    } else {
      
      ## assume annual
      all.results <- data.frame(date = as.Date(mydata$date), 
                                conc = mydata[[pollutant]],
                                stringsAsFactors = FALSE)
      results <- na.omit(all.results)
    }
    
    ## now calculate trend, uncertainties etc ###########################
    if (nrow(results) < 6) return() ## need enough data to calculate trend
    MKresults <- MKstats(results$date, results$conc, alpha, autocor)
   
    ## make sure missing data are put back in for plotting
    results <- merge(all.results, MKresults, by = "date", all = TRUE)
   
    results
  }
  
  
  split.data <- group_by_(mydata, .dots = type) %>%
    do(process.cond(.))
  

  if (nrow(split.data) < 2) return()
  
  
  ## special wd layout
  #(type field in results.grid called type not wd)
  if (length(type) == 1 &
      type[1] == "wd" & is.null(extra.args$layout)) {
    ## re-order to make sensible layout
    ## starting point code as of ManKendall
    wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    split.data$wd <- ordered(split.data$wd, levels = wds)
    wd.ok <-
      sapply(wds, function (x) {
        if (x %in% unique(split.data$wd))
          FALSE
        else
          TRUE
      })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    split.data$wd <- factor(split.data$wd)
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args))
      extra.args$skip <- skip
  }
  if(!"skip" %in% names(extra.args))
    extra.args$skip <- FALSE
  
  ## proper names of labelling #######################################
  strip.dat <- strip.fun(split.data, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]
  
  #### calculate slopes etc ###########################################
  
  split.data <- transform(split.data, slope = 365 * b, intercept = a,
                          intercept.lower = lower.a, intercept.upper = upper.a,
                          lower = 365 * lower.b, upper = 365 * upper.b)
  
  ## aggregated results
  
  res2 <- group_by_(split.data, type, "p.stars") %>% 
    summarise_each(funs(mean(., na.rm = TRUE)))
  
  ## calculate percentage changes in slope and uncertainties need
  ## start and end dates (in days) to work out concentrations at those
  ## points percentage change defind as 100.(C.end/C.start -1) /
  ## duration
  
  start <- group_by_(split.data, type) %>% 
    do(head(., 1))
  
  end <- group_by_(split.data, type) %>% 
    do(tail(., 1))
  
  percent.change <- merge(start, end, by = type, suffixes = c(".start", ".end"))
  
  percent.change <-
    transform(percent.change,
              slope.percent = 100 * 365 *
                ((slope.start * as.numeric(date.end) / 365 + intercept.start) /
                   (slope.start * as.numeric(date.start) / 365 + intercept.start) - 1
                ) /
                (as.numeric(date.end) - as.numeric(date.start)))
  ## got upper/lower intercepts mixed up To FIX?
  percent.change <-
    transform(percent.change,
              lower.percent = 100 * 365 *
                ((lower.start * as.numeric(date.end) / 365 + intercept.lower.start) /
                   (
                     lower.start * as.numeric(date.start) / 365 + intercept.lower.start
                   ) - 1
                ) /
                (as.numeric(date.end) - as.numeric(date.start)))
  
  percent.change <-
    transform(percent.change,
              upper.percent = 100 * 365 *
                ((upper.start * as.numeric(date.end) / 365 + intercept.upper.start) /
                   (
                     upper.start * as.numeric(date.start) / 365 + intercept.upper.start
                   ) - 1
                ) /
                (as.numeric(date.end) - as.numeric(date.start)))
  
  percent.change <- percent.change[ ,  c(type, "slope.percent", 
                                         "lower.percent", "upper.percent")]
  
  split.data <- merge(split.data, percent.change, by = type)
  
  res2 <- merge(res2, percent.change, by = type)
  ## #################################################################
  
  
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("conc ~ date| ", temp, sep = ""))
  
  gap <- (max(split.data$date) - min(split.data$date)) / 80
  if (is.null(xlim)) xlim <- range(split.data$date) + c(-1 * gap, gap)
  
  xyplot.args <- list(x = myform, data = split.data,
                      xlab = quickText(xlab, auto.text),
                      par.strip.text = list(cex = 0.8),
                      as.table = TRUE,
                      xlim = xlim,
                      strip = strip,
                      strip.left = strip.left,
                      scales = list(x = list(at = date.at, format = date.format,
                                             relation = x.relation),
                                    y = list(relation = y.relation, rot = 0)),
                      
                      panel = function(x, y, subscripts,...){
                        ## year shading
                        panel.shade(split.data, start.year, end.year,
                                    ylim = current.panel.limits()$ylim, shade)
                        panel.grid(-1, 0)
                        
                        panel.xyplot(x, y, type = "b", col = data.col, ...)
                        
                        # sub.dat <- na.omit(split.data[subscripts, ])
                        sub.dat <- split.data[subscripts, ]
                        
                        if (nrow(sub.dat) > 0) {
                          panel.abline(a = sub.dat[1, "intercept"],
                                       b = sub.dat[1, "slope"] / 365,
                                       col = trend$col[1], lwd = trend$lwd[1],
                                       lty = trend$lty[1])
                          panel.abline(a = sub.dat[1, "intercept.lower"],
                                       b = sub.dat[1, "lower"] / 365,
                                       col = trend$col[2], lwd = trend$lwd[2],
                                       lty = trend$lty[2])
                          panel.abline(a = sub.dat[1, "intercept.upper"],
                                       b = sub.dat[1, "upper"] / 365,
                                       col = trend$col[2], lwd = trend$lwd[2],
                                       lty = trend$lty[2])
                          
                          ## for text on plot - % trend or not?
                          slope <- "slope"
                          lower <- "lower"
                          upper <- "upper"
                          units <- "units"
                          
                          if (slope.percent) {
                            slope <- "slope.percent"
                            lower <- "lower.percent"
                            upper <- "upper.percent"
                            units <- "%"
                          }
                          
                          ## allow user defined slope text
                          if (!is.null(slope.text)) {
                            
                            slope.text <- slope.text
                            
                          } else {
                            
                            slope.text <- paste0(units, "/year")
                            
                          }
                          
                          ## plot top, middle
                          panel.text(mean(c(current.panel.limits()$xlim[2], 
                                            current.panel.limits()$xlim[1])),
                                     current.panel.limits()$ylim[1] + lab.frac *
                                       (current.panel.limits()$ylim[2] - 
                                          current.panel.limits()$ylim[1]),
                                     paste(round(sub.dat[1, slope], dec.place), " ", "[",
                                           round(sub.dat[1, lower], dec.place), ", ",
                                           round(sub.dat[1, upper], dec.place), "] ",
                                           slope.text, " ", sub.dat[1, "p.stars"], sep = ""),
                                     cex = lab.cex, adj = c(0.5, 1), col = text.col, font = 2)
                        }
                      })
  
  #reset for extra.args
  xyplot.args<- listUpdate(xyplot.args, extra.args)
  
  #plot
  plt <- do.call(xyplot, xyplot.args)
  
  
  ## output ##########################################################
  
  if (length(type) == 1) plot(plt) else 
    plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
  
  newdata <- list(main.data = split.data, res2 = res2, 
                  subsets = c("main.data", "res2"))
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  
  invisible(output)
  
}



panel.shade <- function(split.data, start.year, end.year, ylim, 
                        shade = "grey95") {
  
  x1 <- as.POSIXct(seq(ISOdate(start.year - 6, 1, 1),
                       ISOdate(end.year + 5, 1, 1), by = "2 years"), "GMT")
  x2 <- as.POSIXct(seq(ISOdate(start.year + 1 - 6, 1, 1),
                       ISOdate(end.year + 5, 1, 1), by = "2 years"), "GMT")
  
  
  if (class(split.data$date)[1]  == "Date") {x1 <- as.Date(x1)
  x2 <- as.Date(x2)
  }
  
  rng <- range(split.data$conc, na.rm = TRUE) ## range of data
  y1 <- min(split.data$conc, na.rm = TRUE) - 0.1 * abs(rng[2] - rng[1])
  y2 <- max(split.data$conc, na.rm = TRUE) + 0.1 * abs(rng[2] - rng[1])
  
  ## if user selects specific limits
  
  if (!missing(ylim)) {
    y1 <- ylim[1] - 0.1 * abs(ylim[2] - ylim[1])
    y2 <- ylim[2] + 0.1 * abs(ylim[2] - ylim[1])
  }
  
  sapply(seq_along(x1), function(x) lpolygon(c(x1[x], x1[x], x2[x], x2[x]),
                                             c(y1, y2, y2, y1),
                                             col = shade, border = "grey95"))
}

MKstats <- function(x, y, alpha, autocor) {
  
  estimates <- regci(as.numeric(x), y, alpha = alpha, autocor = autocor)$regci
  
  p <- estimates[2, 5]
  
  if (p >= 0.1) stars <- ""
  if (p < 0.1 & p >= 0.05) stars <- "+"
  if (p < 0.05 & p >= 0.01) stars <- "*"
  if (p < 0.01 & p >= 0.001) stars <- "**"
  if (p < 0.001) stars <- "***"
  
  results <-
    data.frame(
      date = x,
      a = estimates[1, 3],
      b = estimates[2, 3],
      upper.a = estimates[1, 1],
      upper.b = estimates[2, 2],
      lower.a = estimates[1, 2],
      lower.b = estimates[2, 1],
      p = p,
      p.stars = stars,
      stringsAsFactors = FALSE
    )
  results
}
