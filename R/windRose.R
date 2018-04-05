pollutionRose <- function(mydata, pollutant = "nox", key.footer = pollutant,
                          key.position = "right", key = TRUE,
                          breaks = 6, paddle = FALSE, seg = 0.9, normalise = FALSE,
                          ...) {

  ## extra args setup
  extra <- list(...)

  ## check to see if two met data sets are being compared.
  ## if so, set pollutant to one of the names
  if ("ws2" %in% names(extra)) {
    pollutant <- extra$ws
    if (missing(breaks)) breaks <- NA
  }

  if (is.null(breaks)) breaks <- 6

  if (is.numeric(breaks) & length(breaks) == 1) {

    ## breaks from the minimum to 90th percentile, which generally gives sensible
    ## spacing for skewed data. Maximum is added later.
 breaks <- unique(pretty(c(
  min(mydata[[pollutant]], na.rm = TRUE),
  quantile(mydata[[pollutant]], probs = 0.9, na.rm = TRUE),
  breaks
)))
 
 breaks <- c(breaks, max(mydata[[pollutant]], na.rm = TRUE))
  }

  windRose(
    mydata,
    pollutant = pollutant, paddle = paddle, seg = seg,
    key.position = key.position, key.footer = key.footer, key = key,
    breaks = breaks, normalise = normalise, ...
  )
}




##' Traditional wind rose plot and pollution rose variation
##'
##' The traditional wind rose plot that plots wind speed and wind
##' direction by different intervals. The pollution rose applies the
##' same plot structure but substitutes other measurements, most
##' commonly a pollutant time series, for wind speed.
##'
##' For \code{windRose} data are summarised by direction, typically by
##' 45 or 30 (or 10) degrees and by different wind speed categories.
##' Typically, wind speeds are represented by different width
##' "paddles". The plots show the proportion (here represented as a
##' percentage) of time that the wind is from a certain angle and wind
##' speed range.
##'
##' By default \code{windRose} will plot a windRose in using "paddle"
##' style segments and placing the scale key below the plot.
##'
##' The argument \code{pollutant} uses the same plotting structure but
##' substitutes another data series, defined by \code{pollutant}, for
##' wind speed.
##'
##' The option \code{statistic = "prop.mean"} provides a measure of
##' the relative contribution of each bin to the panel mean, and is
##' intended for use with \code{pollutionRose}.
##'
##' \code{pollutionRose} is a \code{windRose} wrapper which brings
##' \code{pollutant} forward in the argument list, and attempts to
##' sensibly rescale break points based on the \code{pollutant} data
##' range by by-passing \code{ws.int}.
##'
##' By default, \code{pollutionRose} will plot a pollution rose of
##' \code{nox} using "wedge" style segments and placing the scale key
##' to the right of the plot.
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
##'   ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
##'   = "default", grid.line = NULL, width = 1, seg = NULL, auto.text
##'   = TRUE, breaks = 4, offset = 10, normalise = FALSE, max.freq =
##'   NULL, paddle = TRUE, key.header = NULL, key.footer = "(m/s)",
##'   key.position = "bottom", key = TRUE, dig.lab = 5, statistic =
##'   "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
##'   315, border = NA, ...)
##'
##'
##'   pollutionRose(mydata, pollutant = "nox", key.footer = pollutant,
##'   key.position = "right", key = TRUE, breaks = 6, paddle = FALSE,
##'   seg = 0.9, normalise = FALSE, ...)
##'
##'
##' @aliases windRose pollutionRose
##' @param mydata A data frame containing fields \code{ws} and
##'   \code{wd}
##' @param ws Name of the column representing wind speed.
##' @param wd Name of the column representing wind direction.
##' @param ws2 The user can supply a second set of wind speed and wind
##'   direction values with which the first can be compared. See
##'   details below for full explanation.
##' @param wd2 see \code{ws2}.
##' @param ws.int The Wind speed interval. Default is 2 m/s but for
##'   low met masts with low mean wind speeds a value of 1 or 0.5 m/s
##'   may be better. Note, this argument is superseded in
##'   \code{pollutionRose}. See \code{breaks} below.
##' @param angle Default angle of \dQuote{spokes} is 30. Other
##'   potentially useful angles are 45 and 10. Note that the width of
##'   the wind speed interval may need adjusting using \code{width}.
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
##' @param bias.corr When \code{angle} does not divide exactly into
##'   360 a bias is introduced in the frequencies when the wind
##'   direction is already supplied rounded to the nearest 10 degrees,
##'   as is often the case. For example, if \code{angle = 22.5}, N, E,
##'   S, W will include 3 wind sectors and all other angles will be
##'   two. A bias correction can made to correct for this problem. A
##'   simple method according to Applequist (2012) is used to adjust
##'   the frequencies.
##' @param cols Colours to be used for plotting. Options include
##'   \dQuote{default}, \dQuote{increment}, \dQuote{heat},
##'   \dQuote{jet}, \dQuote{hue} and user defined. For user defined
##'   the user can supply a list of colour names recognised by R (type
##'   \code{colours()} to see the full list). An example would be
##'   \code{cols = c("yellow", "green", "blue", "black")}.
##' @param grid.line Grid line interval to use. If \code{NULL}, as in
##'   default, this is assigned by \code{windRose} based on the
##'   available data range. However, it can also be forced to a
##'   specific value, e.g. \code{grid.line = 10}. \code{grid.line} can
##'   also be a list to control the interval, line type and colour.
##'   For example \code{grid.line = list(value = 10, lty = 5, col =
##'   "purple")}.
##' @param width For \code{paddle = TRUE}, the adjustment factor for
##'   width of wind speed intervals. For example, \code{width = 1.5}
##'   will make the paddle width 1.5 times wider.
##' @param seg For \code{pollutionRose} \code{seg} determines with
##'   width of the segments. For example, \code{seg = 0.5} will
##'   produce segments 0.5 * \code{angle}.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and
##'   format pollutant names and units properly e.g.  by subscripting
##'   the \sQuote{2} in NO2.
##' @param breaks Most commonly, the number of break points for wind
##'   speed in \code{windRose} or pollutant in \code{pollutionRose}.
##'   For \code{windRose} and the \code{ws.int} default of 2 m/s, the
##'   default, 4, generates the break points 2, 4, 6, 8 m/s. For
##'   \code{pollutionRose}, the default, 6, attempts to breaks the
##'   supplied data at approximately 6 sensible break points. However,
##'   \code{breaks} can also be used to set specific break points. For
##'   example, the argument \code{breaks = c(0, 1, 10, 100)} breaks
##'   the data into segments <1, 1-10, 10-100, >100.
##' @param offset The size of the 'hole' in the middle of the plot,
##'   expressed as a percentage of the polar axis scale, default 10.
##' @param normalise If \code{TRUE} each wind direction segment of a
##'   pollution rose is normalised to equal one. This is useful for
##'   showing how the concentrations (or other parameters) contribute
##'   to each wind sector when the proprtion of time the wind is from
##'   that direction is low. A line showing the probability that the
##'   wind directions is from a particular wind sector is also shown.
##' @param max.freq Controls the scaling used by setting the maximum
##'   value for the radial limits. This is useful to ensure several
##'   plots use the same radial limits.
##' @param paddle Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} plots rose using `paddle' style spokes. If
##'   \code{FALSE} plots rose using `wedge' style spokes.
##' @param key.header Adds additional text/labels above and/or below
##'   the scale key, respectively. For example, passing
##'   \code{windRose(mydata, key.header = "ws")} adds the addition
##'   text as a scale header. Note: This argument is passed to
##'   \code{drawOpenKey} via \code{quickText}, applying the auto.text
##'   argument, to handle formatting.
##' @param key.footer see \code{key.footer}.
##' @param key.position Location where the scale key is to plotted.
##'   Allowed arguments currently include \dQuote{top},
##'   \dQuote{right}, \dQuote{bottom} and \dQuote{left}.
##' @param key Fine control of the scale key via \code{drawOpenKey}.
##'   See \code{drawOpenKey} for further details.
##' @param dig.lab The number of signficant figures at which
##'   scientific number formatting is used in break point and key
##'   labelling. Default 5.
##' @param statistic The \code{statistic} to be applied to each data
##'   bin in the plot. Options currently include \dQuote{prop.count},
##'   \dQuote{prop.mean} and \dQuote{abs.count}. The default
##'   \dQuote{prop.count} sizes bins according to the proportion of
##'   the frequency of measurements.  Similarly, \dQuote{prop.mean}
##'   sizes bins according to their relative contribution to the mean.
##'   \dQuote{abs.count} provides the absolute count of measurements
##'   in each bin.
##' @param pollutant Alternative data series to be sampled instead of
##'   wind speed. The \code{windRose} default NULL is equivalent to
##'   \code{pollutant = "ws"}.
##' @param annotate If \code{TRUE} then the percentage calm and mean
##'   values are printed in each panel together with a description of
##'   the statistic below the plot. If \code{" "} then only the 
##'   stastic is below the plot. Custom annotations may be added by 
##'   setting value to \code{c("annotation 1", "annotation 2")}. 
##' @param angle.scale The wind speed scale is by default shown at a
##'   315 degree angle. Sometimes the placement of the scale may
##'   interfere with an interesting feature. The user can therefore
##'   set \code{angle.scale} to another value (between 0 and 360
##'   degrees) to mitigate such problems. For example
##'   \code{angle.scale = 45} will draw the scale heading in a NE
##'   direction.
##' @param border Border colour for shaded areas. Default is no
##'   border.
##' @param ... For \code{pollutionRose} other parameters that are
##'   passed on to \code{windRose}. For \code{windRose} other
##'   parameters that are passed on to \code{drawOpenKey},
##'   \code{lattice:xyplot} and \code{cutData}. Axis and title
##'   labelling options (\code{xlab}, \code{ylab}, \code{main}) are
##'   passed to \code{xyplot} via \code{quickText} to handle routine
##'   formatting.
##'
##' @export windRose pollutionRose
##' @import dplyr
##' @importFrom graphics abline
##' @importFrom grDevices col2rgb colorRampPalette grey rgb xy.coords
##' @importFrom methods is
##' @importFrom stats aggregate approx as.dendrogram as.dist ave coef
##'   cor dist formula hclust lm median na.omit optimize
##'   order.dendrogram predict qchisq qnorm qt quantile reshape sd
##'   smooth.spline spline stl ts update var
##' @importFrom utils compareVersion modifyList packageDescription
##'   read.csv read.table
##' @return As well as generating the plot itself, \code{windRose} and
##'   \code{pollutionRose} also return an object of class
##'   \dQuote{openair}. The object includes three main components:
##'   \code{call}, the command used to generate the plot; \code{data},
##'   the data frame of summarised information used to make the plot;
##'   and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- windRose(mydata)}, this output can be used to
##'   recover the data, reproduce or rework the original plot or
##'   undertake further analysis.
##'
##'   An openair output can be manipulated using a number of generic
##'   operations, including \code{print}, \code{plot} and
##'   \code{summarise}.
##'
##'   Summarised proportions can also be extracted directly using the
##'   \code{$data} operator, e.g.  \code{object$data} for \code{output
##'   <- windRose(mydata)}. This returns a data frame with three set
##'   columns: \code{cond}, conditioning based on \code{type};
##'   \code{wd}, the wind direction; and \code{calm}, the
##'   \code{statistic} for the proportion of data unattributed to any
##'   specific wind direction because it was collected under calm
##'   conditions; and then several (one for each range binned for the
##'   plot) columns giving proportions of measurements associated with
##'   each \code{ws} or \code{pollutant} range plotted as a discrete
##'   panel.
##' @note \code{windRose} and \code{pollutionRose} both use
##'   \link{drawOpenKey} to produce scale keys.
##' @author David Carslaw (with some additional contributions by Karl
##'   Ropkins)
##' @seealso See \code{\link{drawOpenKey}} for fine control of the
##'   scale key.
##'
##'   See \code{\link{polarFreq}} for a more flexible version that
##'   considers other statistics and pollutant concentrations.
##' @keywords methods
##' @references
##'
##' Applequist, S, 2012: Wind Rose Bias Correction. J. Appl. Meteor.
##' Climatol., 51, 1305-1309.
##'
##' This paper seems to be the original?
##'
##' Droppo,  J.G. and B.A. Napier (2008) Wind Direction Bias in
##' Generating Wind Roses and Conducting Sector-Based Air Dispersion
##' Modeling, Journal of the Air & Waste Management Association, 58:7,
##' 913-918.
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
windRose <- function(mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA,
                     ws.int = 2, angle = 30, type = "default", 
                     bias.corr = TRUE, cols = "default", grid.line = NULL, 
                     width = 1, seg = 0.9, auto.text = TRUE, breaks = 4, 
                     offset = 10, normalise = FALSE, max.freq = NULL, 
                     paddle = TRUE, key.header = NULL, key.footer = "(m/s)", 
                     key.position = "bottom", key = TRUE, dig.lab = 5, 
                     statistic = "prop.count", pollutant = NULL, 
                     annotate = TRUE, angle.scale = 315, border = NA,
                     ...) {
  
  
  ################## Literal Expressions ###################################### 
  VALIDSTATS <- c("prop.count", "prop.mean", "abs.count", "frequency")
  statisticDEFAULT <- formals(windRose)$statistic  
  angleDEFAULT <- formals(windRose)$angle
  wsIntDEFAULT <- formals(windRose)$ws.int
  breaksDEFAULT <- formals(windRose)$breaks
  maxFreqDEFAULT <- formals(windRose)$max.freq
  radOffsetDEFAULT <- formals(windRose)$offset
  
  twoDatasetAngleDEFAULT <- 10
  twoDatasetOffsetDEFAULT <- 20  
  
  angleMIN <- 3
  angleMAX <- 360
  gridLtyDEFAULT <- 1 
  gridLwdDEFAULT <- 1
  gridResDEFAULT <- 360
  gridCexDEFAULT <- 0.7
  numGridlinesDEFAULT <- 10
  
  calmANGLE <- -999
  calmThreshold <- 0 # m/s

  statisticDigits <- 5
  
  
  
  ################## Wrapper Functions ########################################
  
  # wrapper for Windrose Warning Messages  
  warningWindRose <- function(...){
    warning("In windRose(...):\n", list(...), call. = FALSE)
  }

  # Wrapper for statistics warning messages
  warningStat <- function(... , StatInfo = list(statistic, statisticDEFAULT)){
    warningWindRose("statistic = '", unlist(StatInfo[1]) ,"' is invalid\n",
                    ..., "\n",
                    "defaulting to statistic = '", unlist(StatInfo[2]), "'\n")
    return(statisticDEFAULT)
  }  
    
  # wrapper to coerce angle between 0 and 360 degrees
  angle360 <- function(angleRaw, calmAngle = calmANGLE) {
    angleRaw <- as.numeric(angleRaw)
    validAngles <- angleRaw != calmAngle && !is.na(angleRaw)
    angleRaw[validAngles] <- angleRaw[validAngles] %% 360
    angleRaw[angleRaw == 0] <- 360
    return(angleRaw)
  } 
  
  degToRad <- function(angleRaw){
    return(angleRaw * pi / 180)
  }
  
  dispDecPlaces <- function(num, digits) {
    trimws(format(round(num, digits), nsmall=digits))
  }
  
  
  
  ################## Data validation ##########################################
  
  ##### primary data guards #####
  
  if (!is.data.frame(mydata)){
    stop("mydata must be of type data.frame")
  }

  if (!is.numeric(mydata[[ws]]) || !is.numeric(mydata[[wd]])){
    stop(deparse(substitute(mydata)), "[[", ws, "]] needs to be numeric \n",
         "and ", 
         deparse(substitute(mydata)), "[[", wd, "]] needs to be numeric \n", 
         "Check parameters ws and wd were specified properly")
  }
  

  ##### two datasets data guards #####
  
  twoDatasetsPresent <- !(is.na(ws2) || is.na(wd2))
  if (twoDatasetsPresent) {
    if (!is.numeric(mydata[[ws2]]) || !is.numeric(mydata[[wd2]])){
      warningWindRose("Second set of wind values not numeric \n",
                      "Ignoring second set of wind values")
      twoDatasetsPresent <- FALSE
    }
  }
  
  
  ##### angle guards ##### 
  
  if (!is.numeric(angle)) {
    warningWindRose("angle must be numeric, using angle = ", angleDEFAULT)
    angle <- angleDEFAULT
  }
  
  ## If angle is not divisible by 360 ##
  if (360 %% angle) {
    warningWindRose("angle will produce spoke overlap\n",
                    "suggested angles: 5, 6, 8, 9, 10, 12, 15, 30, 45, 90")
  }
  
  if (angle < angleMIN) {
    warningWindRose("angle too small \n enforcing 'angle = ", angleMIN ,"'")
    angle <- angleMIN
  }
  
  if (angle > angleMAX) {
    warningWindRose("angle too large \n enforcing 'angle = ", angleMAX ,"'")
    angle <- angleMAX
  } 
  
  if (twoDatasetsPresent & missing(angle)) {
    angle <- twoDatasetAngleDEFAULT
  }
  
  mydata[[wd]] <- angle360(mydata[[wd]]) #apply correction to angle 
  AngleScaleRad <- degToRad(angle.scale)
    
  ##### ws.int guards #####
  
  if (!is.numeric(ws.int)) {
    warningWindRose("ws.int must be numeric\n",
                    "defaulting to ws.int = ",  wsIntDEFAULT)
    ws.int <- wsIntDEFAULT
  }
  if (ws.int <= 0) { 
    warningWindRose("ws.int must be positive\n",
                    "defaulting to ws.int = ",  wsIntDEFAULT)
    ws.int <- wsIntDEFAULT
  }
  
  
  ##### breaks guards #####
 
  noMaxInterval <- if (is.list(breaks)){ 
    if ("noMaxInterval" %in% breaks) TRUE else FALSE} else FALSE
  
  if (is.list(breaks)){
    # if list only keep numeric elements of the list
    breaks <- unlist(breaks[unlist(lapply(breaks, is.numeric))])
    
    if (is.null(breaks)){breaks <- breaksDEFAULT}
  }
  
  
  if (!is.numeric(breaks)) {
    warningWindRose("breaks must be numeric\n",
                    "defaulting to breaks = ",  breaksDEFAULT)
    breaks <- breaksDEFAULT
  }
  
  if (length(breaks) == 1) {
    if (breaks <= 0) {
      warningWindRose("breaks must be positive\n",
                      "defaulting to breaks = ",  breaksDEFAULT)
      breaks <- breaksDEFAULT}
    
    if (breaks%%1 != 0){
      warningWindRose("breaks must be a whole number\n",
                      "defaulting to breaks = ",  breaksDEFAULT)
      breaks <- breaksDEFAULT      
      
    }
  }else{
    if (any(duplicated(breaks))) {
      warningWindRose("breaks cannot have repeating values\n",
                      "defaulting to breaks = ",  breaksDEFAULT)
      breaks <- breaksDEFAULT
    }
    
    if (is.unsorted(breaks)) {
      warningWindRose("breaks must be specified in numerical order\n",
                      "defaulting to breaks = ",  breaksDEFAULT)
      breaks <- breaksDEFAULT
    }
  }

   
  ##### statistics guards #####
  
  statIsCustom <- length(statistic) > 1
  
  if (!statIsCustom) {
    if (!statistic %in% VALIDSTATS || !is.character(statistic)) {
      statistic <- warningStat("unknown value for parameter statistic")}
  }else{
    if (!is.list(statistic)) 
      statistic <- warningStat("custom statistics must be in a list form")
    if (!"fun" %in% names(statistic)){ 
      statistic <- warningStat("custom statistics must include a function") 
    }else{
      if (!is.function(statistic$fun)){
        statistic <- warningStat("custom statistic 'fun' must be a function")
      }
    }
    statIsCustom <- length(statistic) > 1   # reassess if custom
  }

  
  ##### custom statistics guard #####
  
  if (statIsCustom) {
   
    customStatFun <- statistic$fun
    
    ## custom units guard ##
    if ("unit" %in% names(statistic)){
      customStatUnit <- statistic$unit
    }else{
      warningWindRose("no custom unit specified \n",
                      "defaulting to unit = ''\n")
      customStatUnit <- ""
    }
    
    ## custom scale guard ##
    customScale <- if ("scale" %in% names(statistic)){
      statistic$scale
    }else{
      "none"
    }
    
    ## custom label guard ##
    if ("lab" %in% names(statistic)){
      customLab <- statistic$lab
    }else{
      warningWindRose("no custom label specified \n",
                      "defaulting to lab = ''\n")      
      customLab <- ""
    } 
    
    ## custom function 2 guard ##
    if ("fun2" %in% names(statistic)){
      if (is.function(statistic$fun2)){
        customFun2 <- statistic$fun2
        customLab2 <- if ("lab2" %in% names(statistic)) statistic$lab2 else ""
      }else{
        warningWindRose("fun2 is not a valid function d \n",
                        "changing fun to to prop.count defaults\n") 
        customFun2 <- function(x) 
          format(mean(x, na.rm = TRUE), digits = statisticDigits)
        customLab2 <- "mean"
      }
    }else{
      customFun2 <- function(x) 
        format(mean(x, na.rm = TRUE), digits = statisticDigits)    
      customLab2 <- "mean"
    }
    ## custom calm label guard ##
    customLabcalm <- if ("labcalm" %in% names(statistic)){
      statistic$labcalm
    }else{
      function(x) round(x, 0)
    }
  }
 
  
  
  ##### custom max.freq guard #####
  
  if(!is.null(max.freq)){
    if (!is.numeric(max.freq)){
      warningWindRose(
        "max.freq is not numeric or NULL, max.freq set to default")
      max.freq <- maxFreqDEFAULT
    }
    if (max.freq <= 0 && !is.null(max.freq)){
      warningWindRose(
        "max.freq must be postive or NULL, max.freq set to default")
      max.freq <- maxFreqDEFAULT
    }    
  }
    
    
  
  ##### Changes made if two datasets are present #####
  
  if (twoDatasetsPresent){
    pollutant <- NULL
    if (missing(key.footer)) {key.footer <- "ws"}
    if (missing(offset)) offset <- twoDatasetOffsetDEFAULT
    if (missing(cols)) cols <- c("lightskyblue", "tomato")
    seg <- 1
  }
 
  
  
  
  ##################  Data Preparation ######################################## 
  
  ##### condition Data #####
  
  if (!twoDatasetsPresent) {
    statdata <- cutData(mydata, type, ...) # data conditioning
    statdata[[ws]] <- mydata[[ws]] 
    statdata[[wd]] <- angle360(mydata[[wd]]) 
  }else{    
    #Two Dataset implementation to preserve compatibility
    statdata <- mydata
    statdata$wsDiff <- statdata[[ws2]] - statdata[[ws]]
    statdata$wdDiff <- angle360(mydata[[wd2]] - mydata[[wd]])
    tmpWS <- statdata[[ws]]
    tmpWD <- statdata[[wd]]
    statdata[[ws]] <- statdata$wsDiff
    statdata[[wd]] <- statdata$wdDiff   
    statdata <- cutData(statdata, type, ...) # data conditioning
    statdata$wsDiff <- statdata[[ws]]
    statdata$wdDiff <- statdata[[wd]]
    statdata[[ws]] <- tmpWS
    statdata[[wd]] <- tmpWD
  }
  pollutantPresent <- !is.null(pollutant)
  
  statdata <- checkPrep(statdata,
                        c(wd,ws,
                          if(twoDatasetsPresent){c(ws2,wd2)},
                          if(any(type %in% dateTypes)){"date"},
                          if (pollutantPresent){pollutant}),
                        type,
                        remove.calm = FALSE,
                        remove.neg = !twoDatasetsPresent)
  
  statdata  <- statdata[complete.cases(statdata[[ws]]),] # remove row if ws==NA
  
  ##### Create Calm Mask ##### 
  
  statdata$calmMask <- statdata[[ws]] == 0  
  
  ##### Create Value for Statistics calculations ##### 
  
  statdataValue <- if (pollutantPresent) pollutant else ws
  statdata$value <- statdata[[statdataValue]]


  
  ################## Interval Determination ###################################
  
  ## min and max for breaks ## 
  value_max <- max(statdata$value, na.rm = TRUE)
  value_min <- min(statdata$value, na.rm = TRUE) 
  
  ##### guards for breaks ##### 

  if (length(breaks) == 1){
    if (breaks != 1 )
    {
      breaks <- seq(0, (breaks - 1) * ws.int, by = ws.int)
    }else{
      breaks <- c(0, value_max)
    }
  }else{
    if (min(breaks) > value_min) {
      warningWindRose("Some values are less than minimum break.")
    }
  }
  
  if (max(breaks) < value_max) {
    if (!noMaxInterval) {
      breaks <- c(breaks, value_max)
    }else{
      warningWindRose("Some values are greater than the maximum break ")
    }
  }
  
  
  
  ##### Create intervals for statistical value  ##### 
  
  
  numIntervals <- length(breaks) - 1  
  statdata$intervals <-  cut(statdata$value,
                               breaks = breaks, 
                               include.lowest = FALSE, 
                               dig.lab = dig.lab, 
                               labels = paste0("Interval", 
                                               as.character(1:numIntervals)))


  ##### Create intervals for Wind direction #####
  
  statdata$wdInterval <- angle360(angle * ceiling(statdata[[wd]] / angle - 0.5)) 

  
  ##### adjust number of colors to match the number of intervals ##### 

  radialCol <- if (numIntervals < length(cols)) {
    cols[1:numIntervals] 
  }else{ 
    openColours(cols, numIntervals)
  }
  
     
  ################## Set Statistics Variables #################################
  
  statFun <- switch(statistic,
                     prop.count = length,
                     prop.mean = function(x) sum(x, na.rm = TRUE),
                     abs.count = length,
                     frequency = length,
                     custom = customStatFun)
  
  statUnit <- switch(statistic,
                      prop.count = "%",
                      prop.mean = "%",
                      abs.count = "",
                      frequency = "",
                      custom = customStatUnit)    
  
  statScale <- switch(statistic,
                       prop.count = "all",
                       prop.mean = "panel",
                       abs.count = "none",
                       frequency = "none",
                       custom = customScale)    
  
  statLabel <- switch(statistic,
                     prop.count = "Frequency of counts by wind direction (%)",
                     prop.mean = "Proportion contribution to the mean (%)",
                     abs.count = "Count by wind direction",
                     frequency = "Count by wind direction",
                     custom = customLab) 
  
  statFun2 <- switch(statistic,
                      prop.count = function(x) format(mean(x, na.rm = TRUE), 
                                                      digits = statisticDigits),
                      prop.mean = function(x) format(mean(x, na.rm = TRUE), 
                                                     digits = statisticDigits),
                      abs.count = function(x) length(x),
                      frequency = function(x) length(x),
                      custom = customFun2)
  
  statLabel2 <- switch(statistic,
                      prop.count = "mean",
                      prop.mean = "mean",
                      abs.count = "count",
                      frequency = "count",
                      custom = customLab2) 
  
  statLabelcalm <- switch(statistic,
                         prop.count = function(x) round(x, 1),
                         prop.mean = function(x) round(x, 1),
                         abs.count = function(x) round(x, 0),
                         frequency = function(x) round(x, 0),
                         custom = customLabcalm)     
  
  
  
  ################### Functions to Generate Graphics  ####################
  
  
  ##### calculate the mean wind direction  ##### 
  ## useful for cases comparing two met data sets ##
  u <- mean(sin(degToRad(statdata[[wd]])), na.rm = TRUE)
  v <- mean(cos(degToRad(statdata[[wd]])), na.rm = TRUE)
  
  meanWindDir <- angle360(atan2(u, v) * 360 / 2 / pi)
  
  
  ##### calculate the mean wind speed  #####   
  meanWindSpd <- mean(statdata[[wd]], na.rm = TRUE)
  
  
  ##### calculate the grid  ##### 
  
  prepare.grid <- function(statdata) {
    
    if (all(is.na(statdata$intervals))) { # for all calms...
      AngIntCrosstab <- data_frame(Interval1 = NA, wd = NA, calm = 100, 
                            panel.fun = NA , meanWindDir = NA, freqs = NA)
    } else {
     
      calm <- statFun(statdata[statdata[[wd]] == calmANGLE, ][[statdataValue]])
      
      AngIntCrosstab <- tapply(statdata[[statdataValue]], 
                        list(statdata$wdInterval, statdata$intervals),
                        statFun)

      freqs <- tapply(statdata[[statdataValue]], statdata$wdInterval, length)  

      ## scale the grid ##
      if (statScale == "all") {
        all <- statFun(statdata$wdInterval)
        calm <- calm / all * 100
        AngIntCrosstab <- AngIntCrosstab / all * 100
      }
      
      if (statScale == "panel") {
        calm <- calm / (statFun(statFun(AngIntCrosstab)) + calm) * 100
        AngIntCrosstab <- AngIntCrosstab / (statFun(statFun(AngIntCrosstab)) + calm) * 100
      }
      
      AngIntCrosstab[is.na(AngIntCrosstab)] <- 0

      if (numIntervals > 1){
        AngIntCrosstab <- t(apply(AngIntCrosstab, 1, cumsum))
      }
      
      panel.fun <- statFun2(statdata[[statdataValue]])
 
      if (all(is.na(meanWindDir))) {meanWindDir <- NA}
     
        
        AngIntCrosstab <- bind_cols(as_data_frame(AngIntCrosstab),
                                    data_frame(
                                      wd = as.numeric(row.names(AngIntCrosstab)),
                                      calm = calm, panel.fun = panel.fun,
                                      meanWindDir = meanWindDir, freqs = freqs
                                    ))

    }
    return(AngIntCrosstab)
  }
  


  ################## Create Plot Function Arguments ###########################
  
 
  ##### data for plot ##### 
  plotData <- group_by(statdata, UQS(syms(type))) %>% do(prepare.grid(.))

  ## calm results ##
  plotData$calm <- statLabelcalm(plotData$calm)
  plotData$meanWindDir <- statLabelcalm(plotData$meanWindDir)

  
  ## correct bias when angle does not divide exactly into 360 ## 
  if (bias.corr) {
    correctBias <- function(plotData) {
      
      # check to see if data for this type combination are rounded to 10 degrees
      valueSelect <-  merge(statdata, plotData[1, type], by = type)
      
      if (!all(valueSelect[[wd]] %% 10 == 0, na.rm = TRUE)) return(plotData)
      
      vars <- grep("Interval[1-9]", names(plotData)) ## the frequencies, without any calms
      
      tmp <- table(angle360(angle * ceiling(seq(10, 360, 10) / angle - 0.5)))
      plotData[plotData[[wd]] != calmANGLE, vars] <-
        plotData[plotData[[wd]] != calmANGLE, vars] * mean(tmp) / tmp
      
      return(plotData)
    }    
    plotData <- group_by(plotData, UQS(syms(type))) %>% do(correctBias(.))
  }
 
  ## normalise by sector ## 
  if (normalise) {
    vars <- grep("Interval[1-9]", names(plotData))
    
    # for plotting the wind frequency line
    plotData$freq <- ave(plotData[[max(vars)]],
                        plotData[type], 
                        FUN = function(x) x / sum(x))
    
    # scale by maximum frequency
    plotData$norm <- plotData$freq / max(plotData$freq)
    
    # normalise
    plotData[, vars] <- plotData[, vars] / plotData[[max(vars)]]
    
    statLabel <- "Normalised by wind sector"
    statUnit <- ""
  }

  
  ##### define lattice strip arguments for plot ##### 

  stripData <- strip.fun(plotData, type, auto.text)
  strip <- stripData[[1]]
  stripLeft <- stripData[[2]]
  pol.name <- stripData[[3]]  ### unused 
  
  
  ##### preparation for defining panel function argument #####   
  
  maxCalcFreq <- max(
    plotData[plotData$wd != calmANGLE, grep("Interval", names(plotData))],
    na.rm = TRUE)
  
  maxFrequency <- if (is.null(max.freq)) maxCalcFreq else max.freq
 
  radialOffset <- maxFrequency * (offset / 100)
 

 
  gridMax <- 2 * maxFrequency
  doubleMax <- 2 * maxFrequency
  

  
  ## set grid line properties ## 
  
  gridlty <- if ("lty" %in% names(grid.line)) grid.line$lty else gridLtyDEFAULT  
  
  gridCol <- if ("col" %in% names(grid.line)) grid.line$col else "grey85"   
  
  gridMax <- if ("max" %in% names(grid.line)) grid.line$max else gridMax
  ScaleMax <- gridMax
  
  gridLwd <- if ("lwd" %in% names(grid.line)) grid.line$lwd else gridLwdDEFAULT

  gridRes <- gridResDEFAULT * 
    (if ("res" %in% names(grid.line)) grid.line$res else 1) 

  
  gridCex <- if ("cex" %in% names(grid.line)) grid.line$cex else gridCexDEFAULT
  
  gridArrow <- if ("gridArrow" %in% names(grid.line)) grid.line$gridArrow else NULL 
  
  if(!is.null(gridArrow) && missing(offset)){
    radialOffset <- maxFrequency * radOffsetDEFAULT * 2  / 100
  } 
  
  gridAveragePt <- if ("gridAveragePoint" %in% names(grid.line)) {
    grid.line$gridAveragePoint} else {FALSE}

  
  gridUserInt <- if(is.numeric(grid.line) && is.null(names(grid.line))) {
    grid.line
  } else {
    if ("value" %in% names(grid.line)) {
      if(is.numeric(grid.line$value)) grid.line$value else NULL
    } else NULL
  } 
  
  gridInterval <- if (!is.null(gridUserInt)) gridUserInt else{
    if (pretty(c(0, 2 * maxFrequency), 
               n = numGridlinesDEFAULT)[2] / doubleMax < 0.9) {
    pretty(c(0, 2 * maxFrequency), n = numGridlinesDEFAULT)[2]}
    else 1.8 * maxFrequency
  }
  

  ## extra Arguments ## 
  
  extra <- list(...)
  
  extra$xlab <- quickText(
    if ("xlab" %in% names(extra)) extra$xlab else "", auto.text)
  
  extra$ylab <- quickText(
    if ("ylab" %in% names(extra)) extra$ylab else "", auto.text)
  
  extra$main <- quickText(
    if ("main" %in% names(extra)) extra$main else "", auto.text)
  
  if ("fontsize" %in% names(extra)) {
    trellis.par.set(fontsize = list(text = extra$fontsize))
  }

  ##### defining parts of the panel function argument #####  
  
  #apothem is a geometric line from the center of the midpoint to the edge
  pltApothemLength <- maxFrequency + radialOffset  
  
  
  ## draw the axis ## 
  
  drawAxis <- function(apothem = pltApothemLength,
                       RadOffset = radialOffset,
                       drawArrow = gridArrow){
    
    if(is.null(drawArrow)){ # regular axis
      
      lsegments(-apothem, 0, apothem, 0)
      lsegments(0, -apothem, 0, apothem)  
      
    }else{ # axis with an arrow
      arrowScaling <- 0.8 

      lpolygon(c(sin(degToRad(drawArrow)) * RadOffset * arrowScaling,
                 -sin(degToRad(drawArrow - 20)) * RadOffset * arrowScaling,
                 -sin(degToRad(drawArrow)) * 0.8 * RadOffset * arrowScaling, 
                 -sin(degToRad(drawArrow + 20)) * RadOffset * arrowScaling),
               c(cos(degToRad(drawArrow)) * RadOffset * arrowScaling,
                 -cos(degToRad(drawArrow - 20)) * RadOffset * arrowScaling,
                 -cos(degToRad(drawArrow)) * 0.8 * RadOffset * arrowScaling,
                 -cos(degToRad(drawArrow + 20)) * RadOffset * arrowScaling),
               col = "navy")
      
      
      lsegments(-apothem, 0, -RadOffset, 0)
      lsegments(RadOffset, 0, apothem, 0)
      lsegments(0, -apothem, 0, -RadOffset)  
      lsegments(0, RadOffset, 0, apothem)  
    }
 }
  
  
  ## Direction Labels ## 
  
  drawDirectionLabels <- function(apothem = pltApothemLength,
                                  TextMagnification = gridCex){
    ltext(apothem * -1 * 0.95, 0.07 * apothem, "W", cex = TextMagnification)
    ltext(0.07 * apothem, apothem * -1 * 0.95, "S", cex = TextMagnification)
    ltext(0.07 * apothem, apothem * 0.95, "N", cex = TextMagnification)
    ltext(apothem * 0.95, 0.07 * apothem, "E", cex = TextMagnification)
  }
 
  ## Interval Circles ## 
  
  drawIntervalCircles <- function(IntCircleAngle = AngleScaleRad,
                                  IntCircleMaximum = ScaleMax,
                                  IntCircleInterval = gridInterval,
                                  IntCircleOffset = radialOffset,
                                  IntCircleColor = gridCol,
                                  IntCircleLineType = gridlty,
                                  IntCircleLineWidth = gridLwd,
                                  IntCircleResolution = gridRes 
                                  ){
    
    radialCircleIntervals <-  seq(IntCircleOffset,
                                  IntCircleMaximum + IntCircleOffset,
                                  by = IntCircleInterval)


    circleAngles <- seq(0, 2 * pi, length = IntCircleResolution)

    # draw circles by connecting lines together
    for (radialInterval in radialCircleIntervals) {
      llines(radialInterval * sin(circleAngles),
             radialInterval * cos(circleAngles),
             col = IntCircleColor,
             lwd = IntCircleLineWidth,
             lty = IntCircleLineType)
    }
    

    
    
  }
  
  ## Interval Scale Text ## 
  
  placeRadialScaleText <- function(ScaleAngle = AngleScaleRad,
                                   ScaleMaximum = ScaleMax,
                                   ScaleInterval = gridInterval,
                                   ScaleOffset = radialOffset,
                                   ScaleUnit = statUnit,
                                   ScaleTextMagnification = gridCex){
    
    radialScaleIntervals <-  seq(ScaleInterval, ScaleMaximum, ScaleInterval)

    ltext((ScaleOffset + radialScaleIntervals) * sin(ScaleAngle),
          (ScaleOffset + radialScaleIntervals) * cos(ScaleAngle),
          paste(radialScaleIntervals, ScaleUnit, sep = "") ,
          cex = ScaleTextMagnification
    )
    
  }
  
  ##### draw the spokes of the windrose ##### 
  
  drawSpokes <- function(spokeData = dat, radialCols = radialCol){
    if (nrow(spokeData) > 0) {
      
      drawPoly <- if (paddle) {  # with paddles
        function(wd, len1, len2, width, colour, xOffset = 0, yOffset = 0) {
          theta <- degToRad(wd)
          len1 <- len1 + radialOffset
          len2 <- len2 + radialOffset
          
          lpolygon(c(len1 * sin(theta) - width * cos(theta) + xOffset,  #x1
                     len1 * sin(theta) + width * cos(theta) + xOffset,  #x2
                     len2 * sin(theta) + width * cos(theta) + xOffset,  #x4
                     len2 * sin(theta) - width * cos(theta) + xOffset), #x3 
                   c(len1 * cos(theta) + width * sin(theta) + yOffset,  #y1
                     len1 * cos(theta) - width * sin(theta) + yOffset,  #y2
                     len2 * cos(theta) - width * sin(theta) + yOffset,  #y4
                     len2 * cos(theta) + width * sin(theta) + yOffset), #y3
                   col = colour,
                   border = border)
        }
      } else { # without paddles
        function(wd, len1, len2, width, colour, xOffset = 0, yOffset = 0) {
          len1 <- len1 + radialOffset
          len2 <- len2 + radialOffset
          
          theta <- degToRad(angle360(seq((wd - seg * angle / 2), 
                                         (wd + seg * angle / 2),
                                         length.out = (angle - 2) * 10)))
          
          lpolygon(c(len1 * sin(theta) + xOffset,       #x1
                     rev(len2 * sin(theta) + xOffset)), #x2
                   c(len1 * cos(theta) + xOffset,       #y1
                     rev(len2 * cos(theta) + xOffset)), #y2
                   col = colour, 
                   border = border)
        }
      } 
      
      
      boxWidths <- seq(0.002 ^ 0.25, 0.016 ^ 0.25, length.out = numIntervals) ^ 4 * 
        maxFrequency * angle / 5
      
      spokeData$Interval0 <- 0 ## make a lower bound to refer to
      
      for (spokeIndex in 1:nrow(spokeData)) { ## go through wind angles 30, 60, ...
        
        for (radialDivide in seq(numIntervals)) { ## go through paddles x1, x2, ...
          
          eval(parse(text = paste(
            "drawPoly(spokeData$wd[spokeIndex], spokeData$Interval", 
            radialDivide - 1,
            "[spokeIndex], spokeData$Interval", 
            radialDivide, "[spokeIndex], width * boxWidths[",
            radialDivide, "], colour = radialCols[", radialDivide, "])",
            sep = ""
          )))
        }
      }
    }
  }
  
  
  
  drawProbLines <- function(normalise, dat, seg, angle, radialOffset) {
    if (normalise) {
      # line shows probability wind direction is from a particular sector  
      makeline <- function(i, dat) {
        
        theta <- degToRad(angle360(seq((dat$wd[i] - seg * angle / 2), 
                                       (dat$wd[i] + seg * angle / 2),
                                       length.out = (angle - 2) * 10)))
        
        lpolygon(c(radialOffset * sin(theta),                       #x1
                   rev((dat$norm[i] + radialOffset) * sin(theta))), #x2 
                 c(radialOffset * cos(theta),                       #y1
                   rev((dat$norm[i] + radialOffset) * cos(theta))), #y2
                 col = "transparent", 
                 border = "black", 
                 lwd = 2)
      }
      
      lapply(1:nrow(dat), makeline, dat)
    }
    
  }
  

 
  
  placeAnnotations <- function (dat){
    
    ## check annotations ## 
    isAnnotated <- !(annotate %in% c(FALSE, NA, NaN)) && !is.null(annotate)
    if (isAnnotated) sub <- statLabel else sub <- NULL
    
    ## annotations e.g. calms, means etc
    if (isAnnotated) { ## don't add calms for prop.mean for now...
      if (annotate == "TRUE") {
        if (!twoDatasetsPresent) annotate <- c("statistic" , "calm")
        if (twoDatasetsPresent) annotate <- c("mean_ws" , "mean_wd")
      }
      annotate.col <- if (length(cols) == 1 && cols == "greyscale") {
        "black" } else {"forestgreen"}
      
      annotations_to_place <- NULL
      for(annotate.index in annotate){
        annotations_to_place <- paste(
          annotations_to_place, 
          switch(
            annotate.index,
            statistic = paste(statLabel2, " = ", dat$panel.fun[1]),
            calm = paste("calm = ", dat$calm[1], statUnit),
            mean_ws = 
              paste("mean ws = ", round(as.numeric(dat$panel.fun[1]), 1)),
            mean_wd = paste("mean wd = ", round(dat$meanWindDir[1], 1)),
            annotate.index
          ), sep = "\n"
        )
      }
      
      ltext(
        maxFrequency + radialOffset, 
        -maxFrequency - radialOffset,
        label = annotations_to_place,
        adj = c(1, 0), 
        cex = 0.7, 
        col = annotate.col
      )    
    }
    
  }
  
  placeAveragePoint <- function (drawMeanPoint = gridAveragePt,
                                 meanWD = meanWindDir,
                                 meanSp = meanWindSpd,
                                 RadOffset = radialOffset){
    
    if(drawMeanPoint){
      lpoints(
        sin(degToRad(meanWD)) * (RadOffset + meanSp),
        cos(degToRad(meanWD)) * (RadOffset + meanSp),
        pch = 8, cex = 2, col = "black")
    }
  }
  
 
  
  
  ##### defining panel function argument #####   

  panel <- function(x, y, subscripts, ...) {
    panel.xyplot(x, y, ...)
    dat <- filter(plotData[subscripts, ], wd <= 360, wd >= 0)
    placeRadialScaleText()   
    drawSpokes(dat, radialCol)
    drawProbLines(normalise, dat, seg, angle, radialOffset)
    drawIntervalCircles()   
    drawDirectionLabels()   
    drawAxis()    
    placeAnnotations(dat)
    placeAveragePoint()
  }

  
  ##### defining legend argument ##### 

    ## Set label values ## 
  legend <- makeOpenKeyLegend(key, list(
    col = radialCol, space = key.position, auto.text = auto.text,
    labels = paste0(dispDecPlaces(breaks[1:numIntervals], dig.lab),
                    " to " , 
                    dispDecPlaces(breaks[2:(numIntervals + 1)], dig.lab)) , 
    footer = key.footer, header = key.header,
    height = 0.60, width = 1.5, fit = "scale",
    plot.style = if (paddle) "paddle" else "other"
  ), "windRose") 
  
  
    
  ################## Set Plot Function Arguments ##############################
  
  xyArgs <- list(
    x = formula(paste("Interval1 ~ wd | ",paste(type, collapse="+"),sep = "")),
    data = plotData,
    xlim = 1.03 * c(-maxFrequency - radialOffset, maxFrequency + radialOffset),
    ylim = 1.03 * c(-maxFrequency - radialOffset, maxFrequency + radialOffset),
    type = "n",
    sub = sub,
    strip = strip,
    strip.left = stripLeft,
    as.table = TRUE,
    aspect = 1,
    par.strip.text = list(cex = 0.8),
    scales = list(draw = FALSE),
    panel = panel, 
    legend = legend
  )
  
  
  
  ################## Set Lattice Graphics Information #########################
  
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))}  
  
  ## reset graphic parameters ## 
  on.exit(trellis.par.set(
    strip.background = current.strip,
    fontsize = current.font
  )) 
  
  
 
  ################## Display Output ###########################################  
  
  xyArgs <- listUpdate(xyArgs, extra)  ## reset for extra
 
  plt <- do.call(xyplot, xyArgs)  ## plot
  
  if (length(type) == 1) {
    plot(plt)
  } else {
    plt <- useOuterStrips(plt, strip = strip, strip.left = stripLeft)
    plot(plt)
  }
 
  newdata <- plotData
  
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
  }
  
