##' Bivariate polar plot with smoothing
##' 
##' Function for plotting pollutant concentration in polar coordinates
##' showing concentration by wind speed (or another numeric variable) 
##' and direction. Mean concentrations are calculated for wind 
##' speed-direction \sQuote{bins} (e.g. 0-1, 1-2 m/s,...  and 0-10, 
##' 10-20 degrees etc.).  To aid interpretation, \code{gam} smoothing
##' is carried out using \code{mgcv}.
##' 
##' The bivariate polar plot is a useful diagnostic tool for quickly 
##' gaining an idea of potential sources. Wind speed is one of the 
##' most useful variables to use to separate source types (see 
##' references). For example, ground-level concentrations resulting 
##' from buoyant plumes from chimney stacks tend to peak under higher 
##' wind speed conditions. Conversely, ground-level, non-buoyant 
##' plumes such as from road traffic, tend to have highest 
##' concentrations under low wind speed conditions. Other sources such
##' as from aircraft engines also show differing characteristics by 
##' wind speed.
##' 
##' The function has been developed to allow variables other than wind
##' speed to be plotted with wind direction in polar coordinates. The 
##' key issue is that the other variable plotted against wind 
##' direction should be discriminating in some way. For example, 
##' temperature can help reveal high-level sources brought down to 
##' ground level in unstable atmospheric conditions, or show the 
##' effect a source emission dependent on temperature e.g. biogenic 
##' isoprene.
##' 
##' The plots can vary considerably depending on how much smoothing is
##' done.  The approach adopted here is based on the very flexible and
##' capable \code{mgcv} package that uses \emph{Generalized Additive 
##' Models}. While methods do exist to find an optimum level of 
##' smoothness, they are not necessarily useful. The principal aim of 
##' \code{polarPlot} is as a graphical analysis rather than for 
##' quantitative purposes. In this respect the smoothing aims to 
##' strike a balance between revealing interesting (real) features and
##' overly noisy data. The defaults used in \code{polarPlot} are based
##' on the analysis of data from many different sources. More advanced
##' users may wish to modify the code and adopt other smoothing 
##' approaches.
##' 
##' Various statistics are possible to consider e.g. mean, maximum, 
##' median. \code{statistic = "max"} is often useful for revealing 
##' sources.
##' 
##' Wind direction is split up into 10 degree intervals and the other 
##' variable (e.g. wind speed) 30 intervals. These 2D bins are then 
##' used to calculate the statistics.
##' 
##' These plots often show interesting features at higher wind speeds 
##' (see references below). For these conditions there can be very few
##' measurements and therefore greater uncertainty in the calculation 
##' of the surface. There are several ways in which this issue can be 
##' tackled. First, it is possible to avoid smoothing altogether and 
##' use \code{\link{polarFreq}}. Second, the effect of setting a 
##' minimum number of measurements in each wind speed-direction bin 
##' can be examined through \code{min.bin}. It is possible that a 
##' single point at high wind speed conditions can strongly affect the
##' surface prediction. Therefore, setting \code{min.bin = 3}, for 
##' example, will remove all wind speed-direction bins with fewer than
##' 3 measurements \emph{before} fitting the surface. Third, consider 
##' setting \code{uncertainty = TRUE}. This option will show the 
##' predicted surface together with upper and lower 95% confidence 
##' intervals, which take account of the frequency of measurements.
##' 
##' Variants on \code{polarPlot} include \code{polarAnnulus} and 
##' \code{polarFreq}.
##' 
##' @param mydata A data frame minimally containing \code{wd}, another
##'   variable to plot in polar coordinates (the default is a column 
##'   \dQuote{ws} --- wind speed) and a pollutant. Should also contain
##'   \code{date} if plots by time period are required.
##' @param pollutant Mandatory. A pollutant name corresponding to a 
##'   variable in a data frame should be supplied e.g. \code{pollutant
##'   = "nox"}. There can also be more than one pollutant specified 
##'   e.g. \code{pollutant = c("nox", "no2")}. The main use of using
##'   two or more pollutants is for model evaluation where two species
##'   would be expected to have similar concentrations. This saves the
##'   user stacking the data and it is possible to work with columns
##'   of data directly. A typical use would be \code{pollutant =
##'   c("obs", "mod")} to compare two columns \dQuote{obs} (the
##'   observations) and \dQuote{mod} (modelled values).
##' @param x Name of variable to plot against wind direction in polar 
##'   coordinates, the default is wind speed, \dQuote{ws}.
##' @param wd Name of wind direction field.
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
##' @param statistic The statistic that should be applied to each wind
##'   speed/direction bin. Can be \dQuote{mean} (default), 
##'   \dQuote{median}, \dQuote{max} (maximum), \dQuote{frequency}. 
##'   \dQuote{stdev} (standard deviation), \dQuote{weighted.mean}, 
##'   \dQuote{cpf} (Conditional Probability Function) or \dQuote{r}
##'   (correlation coefficient). Because of the smoothing involved,
##'   the colour scale for some of these statistics is only to provide
##'   an indication of overall pattern and should not be interpreted
##'   in concentration units e.g. for \code{statistic =
##'   "weighted.mean"} where the bin mean is multiplied by the bin
##'   frequency and divided by the total frequency. In many cases
##'   using \code{polarFreq} will be better. Setting \code{statistic =
##'   "weighted.mean"} can be useful because it provides an indication
##'   of the concentration * frequency of occurrence and will
##'   highlight the wind speed/direction conditions that dominate the
##'   overall mean.
##'   
##'   When \code{statistic = "cpf"} the conditional probability 
##'   function (CPF) is plotted and a single (usually high) percentile
##'   level is supplied. The CPF is defined as CPF = my/ny, where my 
##'   is the number of samples in the y bin (by default a wind 
##'   direction, wind speed interval) with mixing ratios greater than 
##'   the \emph{overall} percentile concentration, and ny is the total
##'   number of samples in the same wind sector (see Ashbaugh et al., 
##'   1985). Note that percentile intervals can also be considered; 
##'   see \code{percentile} for details.
##'   
##'   When \code{statistic = "r"}, the Pearson correlation coefficient
##'   is calculated for \emph{two} pollutants. The calculation
##'   involves a weighted Pearson correlation coefficient, which is
##'   weighted by Gaussian kernels for wind direction an the radial
##'   variable (by default wind speed). More weight is assigned to
##'   values close to a wind speed-direction interval. Kernel
##'   weighting is used to ensure that all data are used rather than
##'   relying on the potentially small number of values in a wind
##'   speed-direction interval.
##' @param resolution Two plot resolutions can be set: \dQuote{normal}
##'   (the default) and \dQuote{fine}, for a smoother plot. It should
##'   be noted that plots with a \dQuote{fine} resolution can take
##'   longer to render and the default option should be sufficient or
##'   most circumstances.
##' @param limits The function does its best to choose sensible limits
##'   automatically. However, there are circumstances when the user
##'   will wish to set different ones. An example would be a series of
##'   plots showing each year of data separately. The limits are set
##'   in the form \code{c(lower, upper)}, so \code{limits = c(0, 100)}
##'   would force the plot limits to span 0-100.
##' @param exclude.missing Setting this option to \code{TRUE} (the 
##'   default) removes points from the plot that are too far from the 
##'   original data. The smoothing routines will produce predictions
##'   at points where no data exist i.e. they predict. By removing the
##'   points too far from the original data produces a plot where it
##'   is clear where the original data lie. If set to \code{FALSE}
##'   missing data will be interpolated.
##' @param uncertainty Should the uncertainty in the calculated 
##'   surface be shown? If \code{TRUE} three plots are produced on the
##'   same scale showing the predicted surface together with the 
##'   estimated lower and upper uncertainties at the 95% confidence 
##'   interval. Calculating the uncertainties is useful to understand 
##'   whether features are real or not.  For example, at high wind 
##'   speeds where there are few data there is greater uncertainty
##'   over the predicted values. The uncertainties are calculated
##'   using the GAM and weighting is done by the frequency of
##'   measurements in each wind speed-direction bin. Note that if
##'   uncertainties are calculated then the type is set to "default".
##' @param percentile If \code{statistic = "percentile"} then 
##'   \code{percentile} is used, expressed from 0 to 100. Note that
##'   the percentile value is calculated in the wind speed, wind
##'   direction \sQuote{bins}. For this reason it can also be useful
##'   to set \code{min.bin} to ensure there are a sufficient number of
##'   points available to estimate a percentile. See \code{quantile}
##'   for more details of how percentiles are calculated.
##'   
##'   \code{percentile} is also used for the Conditional Probability 
##'   Function (CPF) plots. \code{percentile} can be of length two, in
##'   which case the percentile \emph{interval} is considered for use 
##'   with CPF. For example, \code{percentile = c(90, 100)} will plot 
##'   the CPF for concentrations between the 90 and 100th percentiles.
##'   Percentile intervals can be useful for identifying specific
##'   sources. In addition, \code{percentile} can also be of length 3.
##'   The third value is the \sQuote{trim} value to be applied. When
##'   calculating percentile intervals many can cover very low values
##'   where there is no useful information. The trim value ensures
##'   that values greater than or equal to the trim * mean value are
##'   considered \emph{before} the percentile intervals are 
##'   calculated. The effect is to extract more detail from many
##'   source signatures. See the manual for examples. Finally, if the
##'   trim value is less than zero the percentile range is interpreted
##'   as absolute concentration values and subsetting is carried out
##'   directly.
##' @param cols Colours to be used for plotting. Options include 
##'   \dQuote{default}, \dQuote{increment}, \dQuote{heat},
##'   \dQuote{jet} and \code{RColorBrewer} colours --- see the
##'   \code{openair} \code{openColours} function for more details. For
##'   user defined the user can supply a list of colour names
##'   recognised by R (type \code{colours()} to see the full list). An
##'   example would be \code{cols = c("yellow", "green", "blue")}
##' @param weights At the edges of the plot there may only be a few 
##'   data points in each wind speed-direction interval, which could
##'   in some situations distort the plot if the concentrations are 
##'   high. \code{weights} applies a weighting to reduce their 
##'   influence. For example and by default if only a single data
##'   point exists then the weighting factor is 0.25 and for two
##'   points 0.5. To not apply any weighting and use the data as is,
##'   use \code{weights = c(1, 1, 1)}.
##'   
##'   An alternative to down-weighting these points they can be
##'   removed altogether using \code{min.bin}.
##'   
##' @param min.bin The minimum number of points allowed in a wind 
##'   speed/wind direction bin.  The default is 1. A value of two 
##'   requires at least 2 valid records in each bin an so on; bins
##'   with less than 2 valid records are set to NA. Care should be
##'   taken when using a value > 1 because of the risk of removing
##'   real data points. It is recommended to consider your data with
##'   care. Also, the \code{polarFreq} function can be of use in such
##'   circumstances.
##' @param mis.col When \code{min.bin} is > 1 it can be useful to show
##'   where data are removed on the plots. This is done by shading the
##'   missing data in \code{mis.col}. To not highlight missing data
##'   when \code{min.bin} > 1 choose \code{mis.col = "transparent"}.
##' @param upper This sets the upper limit wind speed to be used.
##'   Often there are only a relatively few data points at very high
##'   wind speeds and plotting all of them can reduce the useful 
##'   information in the plot.
##' @param angle.scale The wind speed scale is by default shown at a 
##'   315 degree angle. Sometimes the placement of the scale may 
##'   interfere with an interesting feature. The user can therefore
##'   set \code{angle.scale} to another value (between 0 and 360
##'   degrees) to mitigate such problems. For example
##'   \code{angle.scale = 45} will draw the scale heading in a NE
##'   direction.
##' @param units The units shown on the polar axis scale.
##' @param force.positive The default is \code{TRUE}. Sometimes if 
##'   smoothing data with steep gradients it is possible for predicted
##'   values to be negative. \code{force.positive = TRUE} ensures that
##'   predictions remain positive. This is useful for several reasons.
##'   First, with lots of missing data more interpolation is needed
##'   and this can result in artifacts because the predictions are too
##'   far from the original data. Second, if it is known beforehand
##'   that the data are all positive, then this option carries that
##'   assumption through to the prediction. The only likely time where
##'   setting \code{force.positive = FALSE} would be if background 
##'   concentrations were first subtracted resulting in data that is 
##'   legitimately negative. For the vast majority of situations it is
##'   expected that the user will not need to alter the default
##'   option.
##' @param k This is the smoothing parameter used by the \code{gam} 
##'   function in package \code{mgcv}. Typically, value of around 100 
##'   (the default) seems to be suitable and will resolve important 
##'   features in the plot. The most appropriate choice of \code{k} is
##'   problem-dependent; but extensive testing of polar plots for many
##'   different problems suggests a value of \code{k} of about 100 is 
##'   suitable. Setting \code{k} to higher values will not tend to 
##'   affect the surface predictions by much but will add to the 
##'   computation time. Lower values of \code{k} will increase 
##'   smoothing. Sometimes with few data to plot \code{polarPlot} will
##'   fail. Under these circumstances it can be worth lowering the
##'   value of \code{k}.
##' @param normalise If \code{TRUE} concentrations are normalised by 
##'   dividing by their mean value. This is done \emph{after} fitting 
##'   the smooth surface. This option is particularly useful if one is
##'   interested in the patterns of concentrations for several 
##'   pollutants on different scales e.g. NOx and CO. Often useful if 
##'   more than one \code{pollutant} is chosen.
##' @param key.header Adds additional text/labels to the scale key. 
##'   For example, passing the options \code{key.header = "header", 
##'   key.footer = "footer1"} adds addition text above and below the 
##'   scale key. These arguments are passed to \code{drawOpenKey} via 
##'   \code{quickText}, applying the \code{auto.text} argument, to 
##'   handle formatting.
##' @param key.footer see \code{key.footer}.
##' @param key.position Location where the scale key is to plotted. 
##'   Allowed arguments currently include \code{"top"},
##'   \code{"right"}, \code{"bottom"} and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}.
##'   See \code{drawOpenKey} for further details.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If 
##'   \code{TRUE} titles and axis labels will automatically try and 
##'   format pollutant names and units properly e.g.  by subscripting 
##'   the `2' in NO2.
##' @param ... Other graphical parameters passed onto 
##'   \code{lattice:levelplot} and \code{cutData}. For example, 
##'   \code{polarPlot} passes the option \code{hemisphere =
##'   "southern"} on to \code{cutData} to provide southern (rather
##'   than default northern) hemisphere handling of \code{type =
##'   "season"}. Similarly, common axis and title labelling options
##'   (such as \code{xlab}, \code{ylab}, \code{main}) are passed to 
##'   \code{levelplot} via \code{quickText} to handle routine 
##'   formatting.
##' @export
##' @import lattice
##' @import mgcv
##' @return As well as generating the plot itself, \code{polarPlot} 
##'   also returns an object of class ``openair''. The object includes
##'   three main components: \code{call}, the command used to generate
##'   the plot; \code{data}, the data frame of summarised information 
##'   used to make the plot; and \code{plot}, the plot itself. If 
##'   retained, e.g. using \code{output <- polarPlot(mydata, "nox")}, 
##'   this output can be used to recover the data, reproduce or rework
##'   the original plot or undertake further analysis.
##'   
##'   An openair output can be manipulated using a number of generic 
##'   operations, including \code{print}, \code{plot} and 
##'   \code{summary}.
##'   
##'   \code{polarPlot} surface data can also be extracted directly
##'   using the \code{results}, e.g.  \code{results(object)} for
##'   \code{output <- polarPlot(mydata, "nox")}. This returns a data
##'   frame with four set columns: \code{cond}, conditioning based on
##'   \code{type}; \code{u} and \code{v}, the translational vectors
##'   based on \code{ws} and \code{wd}; and the local \code{pollutant}
##'   estimate.
##' @author David Carslaw
##' @seealso \code{\link{polarCluster}} for identifying features in 
##'   bivairate polar plots and for post processing and 
##'   \code{\link{polarAnnulus}}, \code{\link{polarFreq}}, 
##'   \code{\link{percentileRose}} for other ways of plotting
##'   directional data.
##' @references
##' 
##' Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time
##' probability analysis of sulfur concentrations at ground canyon
##' national park. Atmospheric Environment 19 (8), 1263-1270.
##' 
##' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). 
##' Detecting and quantifying aircraft and other on-airport 
##' contributions to ambient nitrogen oxides in the vicinity of a 
##' large international airport.  Atmospheric Environment. 40/28 pp 
##' 5424-5434.
##' 
##' Carslaw, D.C., & Beevers, S.D. (2013). Characterising and 
##' understanding emission sources using bivariate polar plots and 
##' k-means clustering. Environmental Modelling & Software, 40, 
##' 325-329. doi:10.1016/j.envsoft.2012.09.005
##' 
##' Henry, R.C., Chang, Y.S., Spiegelman, C.H., 2002. Locating nearby 
##' sources of air pollution by nonparametric regression of 
##' atmospheric concentrations on wind direction. Atmospheric 
##' Environment 36 (13), 2237-2244.
##' 
##' Westmoreland, E.J., N. Carslaw, D.C. Carslaw, A. Gillah and E.
##' Bates (2007).  Analysis of air quality within a street canyon 
##' using statistical and dispersion modelling techniques. Atmospheric
##' Environment. Vol.  41(39), pp. 9195-9205.
##' 
##' Yu, K.N., Cheung, Y.P., Cheung, T., Henry, R.C., 2004. Identifying
##' the impact of large urban airports on local air quality by
##' nonparametric regression. Atmospheric Environment 38 (27),
##' 4501-4507.
##' @keywords methods
##' @examples
##' 
##' 
##' # load example data from package
##' data(mydata)
##' 
##' # basic plot
##' polarPlot(mydata, pollutant = "nox")
##' 
##' # polarPlots by year on same scale
##' \dontrun{polarPlot(mydata, pollutant = "so2", type = "year", main = "polarPlot of so2")}
##' 
##' # set minimum number of bins to be used to see if pattern remains similar
##' \dontrun{polarPlot(mydata, pollutant = "nox", min.bin = 3)}
##' 
##' # plot by day of the week
##'
##' \dontrun{polarPlot(mydata, pollutant = "pm10", type = "weekday")}
##'
##' # show the 95% confidence intervals in the surface fitting
##' \dontrun{polarPlot(mydata, pollutant = "so2", uncertainty = TRUE)}
##'
##'
polarPlot <- 
  function(mydata, pollutant = "nox", x = "ws", wd = "wd", 
           type = "default", statistic = "mean", resolution = "normal", 
           limits = NA, exclude.missing = TRUE, uncertainty = FALSE, 
           percentile = NA, cols = "default", weights = c(0.25, 0.5, 0.75), 
           min.bin = 1, mis.col = "grey", upper = NA, angle.scale = 315,
           units = x, force.positive = TRUE, k = 100, normalise = FALSE,
           key.header = "", key.footer = pollutant, key.position = "right", 
           key = TRUE, auto.text = TRUE, ...)
    {
  
  ## get rid of R check annoyances
  z = NULL
  
  if (statistic == "percentile" & is.na(percentile[1] & statistic != "cpf")) {
    warning("percentile value missing,  using 50")
    percentile <- 50
  }
  
  if (statistic == "r" && length(pollutant) != 2)
    stop("Correlation statistic requires two pollutants.")
  
  # names of variables for use later
  nam.x <- x
  nam.wd <- wd

  if (length(type) > 2) {stop("Maximum number of types is 2.")}
  
  if (uncertainty) type <- "default" ## can't have conditioning here
  
  if (uncertainty & length(pollutant) > 1)
    stop("Can only have one pollutant when uncertainty = TRUE")
  
  if (!statistic %in% c("mean", "median", "frequency", "max", "stdev",
                        "weighted.mean", "percentile", "cpf", "r")) 
    stop (paste("statistic '", statistic, "' not recognised", sep = ""))
  
  if (length(weights) != 3) stop ("weights should be of length 3.")
  
  if (missing(key.header)) key.header <- statistic
  if (key.header == "weighted.mean") key.header <- "weighted\nmean"
  if (key.header == "percentile")
    key.header <- c(paste(percentile, "th", sep = ""), "percentile")
  
  if ("cpf" %in% key.header) key.header <- c("CPF", "probability")
  
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
  
  
  ## extra.args setup
  extra.args <- list(...)
  
  ## label controls
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
  
  ## extract variables of interest
  
  vars <- c(wd, x, pollutant)
  if (any(type %in%  dateTypes)) vars <- c(vars, "date")
  
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  
  mydata <- na.omit(mydata)
  
  ## this is used later for the 'x' scale
  min.scale <- min(mydata[[x]], na.rm = TRUE)
  
  
  # scale data by subtracting the min value this helps with dealing 
  # with negative data on radial axis (starts from zero, always 
  # postive)
  mydata[[x]] <- mydata[[x]] - min(mydata[[x]], na.rm = TRUE)
  
  # if more than one pollutant, need to stack the data and set type =
  # "variable" this case is most relevent for model-measurement
  # compasrions where data are in columns Can also do more than one
  # pollutant and a single type that is not "default", in which case
  # pollutant becomes a conditioning variable
  
  # if statistic is 'r' we need the data for two pollutants in columns
  if (length(pollutant) > 1 && statistic != "r") {
    
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
  
 
  ## cutData depending on type
  mydata <- cutData(mydata, type, ...)
  
  ## if upper ws not set, set it to the max to display all information
  max.ws <- max(mydata[[x]], na.rm = TRUE)
  min.ws <- min(mydata[[x]], na.rm = TRUE)
  clip <- TRUE ## used for removing data where ws > upper
  
  if (missing(upper)) {
    upper <- max.ws
    clip <- FALSE
  }
  
  ## for resolution of grid plotting (default = 101; fine =201)
  if (resolution == "normal") int <- 101
  if (resolution == "fine") int <- 201
  if (resolution == "ultra.fine") int <- 401  ## very large files!
  
  ## binning wd data properly
  ## use 10 degree binning of wd if already binned, else 5
  if (all(mydata[[wd]] %% 10 == 0, na.rm = TRUE)) {
    wd.int <- 10
  } else {
    wd.int <- 5 ## how to split wd
  }
  
  ws.seq <- seq(min.ws, max.ws, length = 30)
  wd.seq <- seq(from = wd.int, to = 360, by = wd.int) ## wind directions from wd.int to 360
  ws.wd <- expand.grid(x = ws.seq, wd = wd.seq)
  
  u <- with(ws.wd, x * sin(pi * wd / 180))  ## convert to polar coords
  v <- with(ws.wd, x * cos(pi * wd / 180))
  
  ## data to predict over
  input.data <- expand.grid(u = seq(-upper, upper, length = int),
                            v = seq(-upper, upper, length = int))
  
  if (statistic == "cpf") {
    ## can be interval of percentiles or a single (threshold)
    if (length(percentile) > 1) {
      statistic <- "cpfi" # CPF interval
      
      if (length(percentile) == 3) {
        ## in this case there is a trim value as a proprtion of the mean
        ## if this value <0 use absolute values as range
        Mean <- mean(mydata[[pollutant]], na.rm = TRUE)
        
        if (percentile[3] < 0) {
          
          Pval <- percentile[1:2]
          
        } else  {
          Pval <- quantile(subset(mydata[[pollutant]],
                                  mydata[[pollutant]] >= Mean *
                                    percentile[3]),
                           probs = percentile[1:2] / 100,
                           na.rm = TRUE)
        }
        
      } else {
        
        Pval <- quantile(mydata[[pollutant]],
                         probs = percentile / 100, na.rm = TRUE)
        
      }
      sub <- paste("CPF (", format(Pval[1], digits = 2), " to ",
                   format(Pval[2], digits = 2), ")", sep = "")
      
    } else {
      Pval <- quantile(mydata[, pollutant], probs = percentile / 100, na.rm = TRUE)
      sub <- paste("CPF at the ", percentile,
                   "th percentile (=", format(Pval, digits = 2), ")", sep = "")
    }
  } else {
    sub <- NULL
  }
  
  
  prepare.grid <- function(mydata) {
    
    ## identify which ws and wd bins the data belong
    wd <- cut(wd.int * ceiling(mydata[[wd]] / wd.int - 0.5),
              breaks = seq(0, 360, wd.int), include.lowest = TRUE)
    
    x <- cut(mydata[[x]], breaks = seq(0, max.ws, length = 31), 
             include.lowest = TRUE)
    
    if (statistic != "r") {
    
    binned <- switch(
      statistic,
      frequency = tapply(mydata[[pollutant]], list(wd, x), function(x)
        length(na.omit(x))),
      mean =  tapply(mydata[[pollutant]], list(wd, x), function(x)
        mean(x, na.rm = TRUE)),
      median = tapply(mydata[[pollutant]], list(wd, x), function(x)
        median(x, na.rm = TRUE)),
      max = tapply(mydata[[pollutant]], list(wd, x), function(x)
        max(x, na.rm = TRUE)),
      stdev = tapply(mydata[[pollutant]], list(wd, x), function(x)
        sd(x, na.rm = TRUE)),
      cpf =  tapply(mydata[[pollutant]], list(wd, x),
                    function(x)
                      (length(which(
                        x > Pval
                      )) / length(x))),
      cpfi =  tapply(mydata[[pollutant]], list(wd, x),
                     function(x)
                       (length(
                         which(x > Pval[1] & x <= Pval[2])
                       ) / length(x))),
      weighted.mean = tapply(mydata[[pollutant]], list(wd, x),
                             function(x)
                               (mean(x) * length(x) / nrow(mydata))),
      percentile = tapply(mydata[[pollutant]], list(wd, x), function(x)
        quantile(x, probs = percentile / 100, na.rm = TRUE))
      
    )
  
    binned <- as.vector(t(binned))
    
    # statistic is the weighted correlation coefficient
    } else {

      binned <- rowwise(ws.wd) %>% 
        do(calCor(., mydata, x = nam.x, y = nam.wd, 
                  pol_1 = pollutant[1], pol_2 = pollutant[2]))
      
     binned <- binned$r
      
      
    }
    
   
    
    ## frequency - remove points with freq < min.bin
    bin.len <- tapply(mydata[[pollutant[1]]], list(x, wd), length)
    binned.len <- as.vector(bin.len)
    
    ## apply weights
    W <- rep(1, times = length(binned))
    ids <- which(binned.len == 1)
    W[ids] <- W[ids] * weights[1]
    ids <- which(binned.len == 2)
    W[ids] <- W[ids] * weights[2]
    ids <- which(binned.len == 3)
    W[ids] <- W[ids] * weights[3]
    
    ## set missing to NA
    ids <- which(binned.len < min.bin)
    binned[ids] <- NA
    
    # for removing missing data later
    binned.len[ids] <- NA
    
     if (force.positive) n <- 0.5 else n <- 1
    
    ## no uncertainty to calculate
    if (!uncertainty) {
      ## catch errors when not enough data to calculate surface
      Mgam <- try(gam(binned ^ n ~ s(u, v, k = k), weights = W), TRUE)
      if (!inherits(Mgam, "try-error")) {
        pred <- predict.gam(Mgam, input.data)
        pred <- pred ^ (1 / n)
        pred <- as.vector(pred)
        results <- data.frame(u = input.data$u, v = input.data$v, z = pred)
      } else {
        results <- data.frame(u = u, v = v, z = binned)
        exclude.missing <- FALSE
        warning(call. = FALSE, paste("Not enough data to fit surface.\nTry reducing the value of the smoothing parameter, k to less than ", k, ".",  sep = ""))
      }
      
    } else {
      
      ## uncertainties calculated, weighted by number of points in each bin
      Mgam <- gam(binned ^ n ~ s(u, v, k = k), weights = binned.len)
      pred <- predict.gam(Mgam, input.data, se.fit = TRUE)
      uncer <- 2 * as.vector(pred[[2]]) ## for approx 95% CI
      pred <- as.vector(pred[[1]]) ^ (1 / n)
      
      
      ## do not weight for central prediction
      Mgam <- gam(binned ^ n ~ s(u, v, k = k))
      pred <- predict.gam(Mgam, input.data)
      pred <- as.vector(pred)
      Lower <- (pred - uncer) ^ (1 / n)
      Upper <- (pred + uncer) ^ (1 / n)
      pred <- pred ^ (1 / n)
      
      n <- length(pred)
      
      results <-  
        data.frame(u = rep(input.data$u, 3), v = rep(input.data$v, 3),
                   z = c(pred, Lower, Upper),
                   default = rep(c("prediction", "lower uncertainty",
                                   "upper uncertainty"), each = n))
    }
    
   
    ## function to remove points too far from original data
    exclude <- function(results) {
      
      ## exclude predictions too far from data (from mgcv)
      x <- seq(-upper, upper, length = int)
      y <- x
      res <- int
      wsp <- rep(x, res)
      wdp <- rep(y, rep(res, res))
      
      ## data with gaps caused by min.bin
      all.data <- na.omit(data.frame(u, v, binned.len))
      ind <- with(all.data, exclude.too.far(wsp, wdp, u, v, dist = 0.05))
      
      results$z[ind] <- NA
      results
    }
    
    if (exclude.missing) results <- exclude(results)
    
    results
  }
  
  ## if min.bin >1 show the missing data. Work this out by running twice:
  ## first time with no missings, second with min.bin.
  ## Plot one surface on top of the other.
  
  if (!missing(min.bin)) {
    tmp <- min.bin
    min.bin <- 0
    res1 <- group_by_(mydata, .dots = type) %>%
      do(prepare.grid(.))
    
    min.bin <- tmp
    
    res <- group_by_(mydata, .dots = type) %>%
      do(prepare.grid(.))
    res$miss <- res1$z
    
  } else {
    
    res <- group_by_(mydata, .dots = type) %>%
      do(prepare.grid(.))
  }
  
  ## with CPF make sure not >1 due to surface fitting
  if (any(res$z > 1, na.rm = TRUE) & statistic %in% c("cpf", "cpfi")) {
    id <- which(res$z > 1)
    res$z[id] <- 1
  }
  
  ## remove wind speeds > upper to make a circle
  if (clip) res$z[(res$u ^ 2 + res$v ^ 2) ^ 0.5 > upper] <- NA
  
  ## proper names of labelling 
  strip.dat <- strip.fun(res, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]
  if (uncertainty) strip <- TRUE
  
  ## normalise by divining by mean conditioning value if needed
  if (normalise){
    
    res <- mutate(res, z = z / mean(z, na.rm = TRUE))
    
    if (missing(key.footer)) key.footer <- "normalised \nlevel"
  }
  
  # correlation notation
  if (statistic == "r") {
    if (missing(key.footer)) 
      key.footer <- 
        paste0("corr(", pollutant[1], ", ", pollutant[2], ")")
    
    # make sure smoothing does not results in r>1 or <-1
    # sometimes happens with little data at edges
    id <- which(res$z > 1)
    if (length(id) > 0) res$z[id] <- 1
    
    id <- which(res$z < -1)
    if (length(id) > 0) res$z[id] <- -1
    
  }
  
  
  ## auto-scaling
  nlev <- 200  ## preferred number of intervals
  
  ## handle missing breaks arguments
  
  if (missing(limits)) {
    # breaks <- pretty(res$z, n = nlev)
    breaks <- seq(min(res$z, na.rm = TRUE), max(res$z, na.rm = TRUE),
                  length.out = nlev)
    labs <- pretty(breaks, 7)
    labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
    at <- labs
    
  } else {
    
    ## handle user limits and clipping
    breaks <- seq(min(limits), max(limits), length.out = nlev)
    labs <- pretty(breaks, 7)
    labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
    at <- labs
    
    ## case where user max is < data max
    if (max(limits) < max(res[["z"]], na.rm = TRUE)) {
      id <- which(res[["z"]] > max(limits))
      res[["z"]][id] <- max(limits)
      labs[length(labs)] <- paste(">", labs[length(labs)])
    }
    
    ## case where user min is > data min
    if (min(limits) > min(res[["z"]], na.rm = TRUE)) {
      id <- which(res[["z"]] < min(limits))
      res[["z"]][id] <- min(limits)
      labs[1] <- paste("<", labs[1])
    }
    
  }
  
  nlev2 <- length(breaks)
  
  col <- openColours(cols, (nlev2 - 1))
  
  col.scale <- breaks
  
  ## special handling of layout for uncertainty
  if (uncertainty & is.null(extra.args$layout)) {
    extra.args$layout <- c(3, 1)
  }
  
  
  legend <- list(col = col, at = col.scale, 
                 labels = list(labels = labs, at = at),
                 space = key.position, auto.text = auto.text,
                 footer = key.footer, header = key.header,
                 height = 1, width = 1.5, fit = "all")
  
  legend <- makeOpenKeyLegend(key, legend, "polarPlot")
  
  
  ## scaling
  ## scaling of 'zeroed' data
  ## note - add upper because user can set this to be different to data
  intervals <- pretty(c(mydata[[x]], upper))
  
  ## labels for scaling
  labels <- pretty(c(mydata[[x]], upper) + min.scale)
  ## offset the lines/labels if necessary
  intervals <- intervals + (min(labels) - min.scale)
  
  ## add zero in the middle if it exists
  if (min.scale != 0){
    labels <- labels[-1]
    intervals <- intervals[-1]
  }
  
  
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("z ~ u * v | ", temp, sep = ""))
  
  Args <- list(x = myform, res, axes = FALSE,
               as.table = TRUE,
               strip = strip,
               strip.left = strip.left,
               col.regions = col,
               region = TRUE,
               aspect = 1,
               sub = sub,
               par.strip.text = list(cex = 0.8),
               scales = list(draw = FALSE),
               xlim = c(-upper * 1.025, upper * 1.025),
               ylim = c(-upper * 1.025, upper * 1.025),
               colorkey = FALSE, legend = legend,
               
               panel = function(x, y, z, subscripts,...) {
                 
                 ## show missing data due to min.bin
                 if (min.bin > 1)
                   panel.levelplot(x, y, res$miss,
                                   subscripts,
                                   col.regions = mis.col,
                                   labels = FALSE)
                 
                 panel.levelplot(x, y, z,
                                 subscripts,
                                 at = col.scale,
                                 pretty = TRUE,
                                 col.regions = col,
                                 labels = FALSE)
                 
                 angles <- seq(0, 2 * pi, length = 360)
                 
                 sapply(intervals, function(x) 
                   llines(x * sin(angles), x * cos(angles),
                          col = "grey", lty = 5))
                 
                 
                 ltext(1.07 * intervals * sin(pi * angle.scale / 180),
                       1.07 * intervals * cos(pi * angle.scale / 180),
                       sapply(paste(labels, c("", "", units, rep("", 7))), 
                              function(x)
                         quickText(x, auto.text)) , cex = 0.7, pos = 4)
                 
                 ## add axis line to central polarPlot
                 larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
                 larrows(0, -upper, 0, upper, code = 3, length = 0.1)
                 
                 ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7)
                 ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7)
                 ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7)
                 ltext(upper * 0.95, 0.07 *upper, "E", cex = 0.7)
                 
               })
  
  ## reset for extra.args
  Args<- listUpdate(Args, extra.args)
  
  plt <- do.call(levelplot, Args)
  
  
  if (length(type) == 1)
    plot(plt)
  else
    plot(useOuterStrips(plt, strip = strip,
                        strip.left = strip.left))
  
  newdata <- res
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  
  invisible(output)
  
  }


calCor <- function(data, mydata, x = "ws", y = "wd", pol_1 = "nox", 
                   pol_2 = "pm10") {
  
  # function to work out weighted correlation between two surfaces
  # uses Gaussian kernel smoothing to weight wd/ws
  
  
  ws1 <- data[[1]] # centre of ws
  wd1 <- data[[2]] # centre of wd
  
  # scale ws, note 20 gives sensible spread for kernel
  mydata$ws.scale <- 20 * (mydata[[x]] - ws1) / 
    (max(mydata[[x]], na.rm = TRUE) - min(mydata[[x]], na.rm = TRUE))
  
  # Gaussian kernel
  mydata$ws.scale <-  (2 * pi) ^ -0.5 * exp(-0.5 * mydata$ws.scale ^ 2)
  
  # wd direction scaling, use Gaussian kernel
  mydata$wd.scale <- mydata[[y]] - wd1
  id <- which(mydata$wd.scale < 0)
  if (length(id) > 0) mydata$wd.scale[id] <- mydata$wd.scale[id] + 360
  id <- which(mydata$wd.scale > 180)
  if (length(id) > 0)  mydata$wd.scale[id] <- abs(mydata$wd.scale[id] - 360)
  
  # note, 4 gives a sensible spread for kernel
  mydata$wd.scale <- 4 * mydata$wd.scale * 2 * pi / 360
  mydata$wd.scale <- (2 * pi) ^ -0.5 * exp(-0.5 * mydata$wd.scale ^ 2)
  
  # final weighting multiplies two kernels for ws and wd
  mydata$weight <- mydata$ws.scale * mydata$wd.scale
  mydata$weight <- mydata$weight / max(mydata$weight, na.rm = TRUE)
  
  # the data we need
  thedata <- select_(mydata, pol_1, pol_2, "weight")
  thedata <- na.omit(thedata)
  
  # useful for showing what the weighting looks like as a surface
  #  scatterPlot(mydata, x= "ws", y = "wd", z = "weight", method = "level", col = "jet")
  
  # call weighted Pearson correlation
  r <- corr(cbind(thedata[[pol_1]], thedata[[pol_2]]), 
                  w = thedata$weight)
  
  result <- data.frame(ws1, wd1, r)
  
  return(result)
  
}


# taken directly from the boot package to save importing
corr <- function(d, w = rep(1, nrow(d)) / nrow(d))
{
  s <- sum(w)
  m1 <- sum(d[, 1L] * w) / s
  m2 <- sum(d[, 2L] * w) / s
  (sum(d[, 1L] * d[, 2L] * w) / s - m1 * m2) / 
    sqrt((sum(d[, 1L] ^ 2 * w) / s - m1 ^ 2) * 
           (sum(d[, 2L] ^ 2 * w) / s - m2 ^ 2))
}
