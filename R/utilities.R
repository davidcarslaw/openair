## TODO: Add comment
#
## Author: David Carslaw
## useful utility functions
## with some updates and modification by Karl Ropkins
###############################################################################

startYear <- function(dat) as.numeric(format(min(dat[order(dat)]), "%Y"))
endYear <- function(dat) as.numeric(format(max(dat[order(dat)]), "%Y"))
startMonth <- function(dat) as.numeric(format(min(dat[order(dat)]), "%m"))
endMonth <- function(dat) as.numeric(format(max(dat[order(dat)]), "%m"))

## these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c(
  "year", "hour", "month", "season", "weekday", "weekend",
  "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
  "seasonyear", "yearseason"
)

## sets up how openair graphics look by default and resets on exit

setGraphics <- function(fontsize = 5) {
  current.strip <- trellis.par.get("strip.background")
  trellis.par.set(fontsize = list(text = fontsize))

  ## reset graphic parameters
  font.orig <- trellis.par.get("fontsize")$text
  on.exit(trellis.par.set(
     
    fontsize = list(text = font.orig)
  ))
}


# function to test of a suggested package is available and warn if not
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop(
    "Package `", package, "` required for `", fun, "`.\n",
    "Please install and try again.",
    call. = FALSE
  )
}

###############################################################################

## function to find averaging period of data, returns "xx sec"
## for use in filling in gaps in time series data
## it finds the table of values of time gaps and picks the biggest
## can't think of better way unless user specifies what the time interval is meant to be

find.time.interval <- function(dates) {

  ## could have several sites, dates may be unordered
  ## find the most common time gap in all the data
  dates <- unique(dates) ## make sure they are unique

  # work out the most common time gap of unique, ordered dates
  id <- which.max(table(diff(as.numeric(unique(dates[order(dates)])))))
  seconds <- as.numeric(names(id))

  if ("POSIXt" %in% class(dates)) seconds <- paste(seconds, "sec")

  if (class(dates)[1] == "Date") {
    seconds <- seconds * 3600 * 24
    seconds <- paste(seconds, "sec")
  }

  seconds
}



date.pad2 <- function(mydata, type = NULL, interval = "month") {

  # assume by the time we get here the data have been split into types
  # This means we just need to pad out the missing types based on first
  # line.

  start.date <- min(mydata$date, na.rm = TRUE)
  end.date <- max(mydata$date, na.rm = TRUE)

  all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
  mydata <- mydata %>% full_join(all.dates, by = "date")

  # add in missing types if gaps are made
  if (!is.null(type)) {
    mydata[type] <- mydata[1, type]
  }

  # make sure order is correct
  mydata <- arrange(mydata, date)

  return(mydata)
}


## #################################################################
# Function to pad out missing time data
# assumes data have already been split by type, so just take first
# tries to work out time interval of input based on most common gap
# can print assumed gap to screen

date.pad <- function(mydata, type = NULL, print.int = FALSE) {

  # if one line, just return
  if (nrow(mydata) < 2) return(mydata)

  ## time zone of data
  TZ <- attr(mydata$date, "tzone")
  if (is.null(TZ)) TZ <- "GMT" ## as it is on Windows for BST

  ## function to fill missing data gaps
  ## assume no missing data to begin with


  ## pad out missing data
  start.date <- min(mydata$date, na.rm = TRUE)
  end.date <- max(mydata$date, na.rm = TRUE)

  ## interval in seconds
  interval <- find.time.interval(mydata$date)

  ## equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
    24 / 3600

  ## find time interval of data
  if (class(mydata$date)[1] == "Date") {
    interval <- paste(days, "day")
  } else {
    ## this will be in seconds
    interval <- find.time.interval(mydata$date)
  }

  ## better interval, most common interval in a year
  if (days == 31) interval <- "month"
  if (days %in% c(365, 366)) interval <- "year"

  ## only pad if there are missing data
  if (length(unique(diff(mydata$date))) != 1L) {
    all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
    mydata <- mydata %>% full_join(all.dates, by = "date")

    # add missing types - if type is present
    if (!is.null(type)) {
      mydata[type] <- mydata[1, type]
    }
  }

  ## return the same TZ that we started with
  attr(mydata$date, "tzone") <- TZ

  if (print.int) print(paste0("Input data time interval assumed is ", interval))

  # make sure date-sorted
  mydata <- arrange(mydata, date)

  mydata
}
#############################################################################################


## unitility function to convert decimal date to POSIXct
decimalDate <- function(x, date = "date") {
  thedata <- x
  x <- x[, date]
  x.year <- floor(x)
  ## fraction of the year
  x.frac <- x - x.year
  ## number of seconds in each year
  x.sec.yr <- unclass(ISOdate(x.year + 1, 1, 1, 0, 0, 0)) - unclass(ISOdate(x.year, 1, 1, 0, 0, 0))
  ## now get the actual time
  x.actual <- ISOdate(x.year, 1, 1, 0, 0, 0) + x.frac * x.sec.yr
  x.actual <- as.POSIXct(trunc(x.actual, "hours"), "GMT")
  thedata$date <- x.actual
  thedata
}




##' Calculate rollingMean values
##'
##' Calculate rollingMean values taking account of data capture thresholds
##'
##' This is a utility function mostly designed to calculate rolling
##' mean statistics relevant to some pollutant limits e.g. 8 hour
##' rolling means for ozone and 24 hour rolling means for
##' PM10. However, the function has a more general use in helping to
##' display rolling mean values in flexible ways e.g. with the rolling
##' window width left, right or centre aligned.
##'
##' The function will try and fill in missing time gaps to get a full
##' time sequence but return a data frame with the same number of rows
##' supplied.
##'
##' @param mydata A data frame containing a \code{date}
##' field. \code{mydata} must contain a \code{date} field in
##' \code{Date} or \code{POSIXct} format. The input time series must
##' be regular e.g. hourly, daily.
##' @param pollutant The name of a pollutant e.g. \code{pollutant = "o3"}.
##' @param width The averaging period (rolling window width) to use
##' e.g. \code{width = 8} will generate 8-hour rolling mean values
##' when hourly data are analysed.
##' @param new.name The name given to the new rollingMean variable. If
##' not supplied it will create a name based on the name of the
##' pollutant and the averaging period used.
##' @param data.thresh The data capture threshold in %. No values are
##' calculated if data capture over the period of interest is less
##' than this value. For example, with \code{width = 8} and
##' \code{data.thresh = 75} at least 6 hours are required to calculate
##' the mean, else \code{NA} is returned.
##' @param align specifyies how the moving window should be
##' aligned. \code{"right"} means that the previous \code{hours}
##' (including the current) are averaged. This seems to be the default
##' for UK air quality rolling mean statistics. \code{"left"} means
##' that the forward \code{hours} are averaged, and \code{"centre"} or
##' \code{"center"}, which is the default.
##' @param ... other arguments, currently unused.
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## rolling 8-hour mean for ozone
##' mydata <- rollingMean(mydata, pollutant = "o3", width = 8, new.name =
##' "rollingo3", data.thresh = 75, align = "right")
##'
##'
rollingMean <- function(mydata, pollutant = "o3", width = 8, new.name = "rolling",
                        data.thresh = 75, align = "centre", ...) {
  ## function to calculate rolling means
  ## uses C++ code

  ## get rid of R check annoyances
  site <- NULL
  if (!align %in% c("left", "right", "centre", "center")) stop("align should be one of 'right', 'left', 'centre' or 'center'.")


  if (missing(new.name)) new.name <- paste("rolling", width, pollutant, sep = "")
  if (data.thresh < 0 | data.thresh > 100) stop("Data threshold must be between 0 and 100.")

  calc.rolling <- function(mydata, ...) {

    ## data needs to be numeric
    if (!is.numeric(mydata[[pollutant]])) {
      warning("Data are not numeric.")
      return(mydata)
    }

    ## need to know whether dates added
    dates <- mydata$date

    ## pad missing hours
    mydata <- date.pad(mydata)

    ## make sure function is not called with window width longer than data
    if (width > nrow(mydata)) return(mydata)

    mydata[[new.name]] <- .Call(
      "rollMean", mydata[[pollutant]],
      width, data.thresh, align,
      PACKAGE = "openair"
    )

    if (length(dates) != nrow(mydata)) {
      ## return what was put in
      ## avoids adding missing data e.g. for factors
      mydata <- mydata[mydata$date %in% dates, ]
    }

    mydata
  }

  ## split if several sites
  if ("site" %in% names(mydata)) { ## split by site

    mydata <- group_by(mydata, site) %>%
      do(calc.rolling(., ...))

    mydata
  } else {
    mydata <- calc.rolling(mydata, ...)
    mydata
  }
}




convert.date <- function(mydata, format = "%d/%m/%Y %H:%M") {
  mydata$date <- as.POSIXct(strptime(mydata$date, format = format), "GMT")
  mydata
}



#############################################################################################
## splits data frame into date chunks. Allows users to supply simple dates and labels
## useful for type = "site", interventions



##' Divide up a data frame by time
##'
##' Utility function to prepare input data for use in openair functions
##'
##' This function partitions a data frame up into different time segments. It
##' produces a new column called controlled by \code{name} that can be used in many
##' \code{openair} functions. Note that there must be one more label than there
##' are dates. See examples below and in full \code{openair} documentation.
##'
##' @param mydata A data frame containing a \code{date} field in hourly or high
##'   resolution format.
##' @param dates A date or dates to split data by.
##' @param labels Labels for each time partition.
##' @param name The name to give the new column to identify the periods split
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## split data up into "before" and "after"
##' mydata <- splitByDate(mydata, dates = "1/04/2000",
##' labels = c("before", "after"))
##'
##' ## split data into 3 partitions:
##' mydata <- splitByDate(mydata, dates = c("1/1/2000", "1/3/2003"),
##' labels = c("before", "during", "after"))
##'
##'
splitByDate <- function(mydata, dates = "1/1/2003", labels = c("before", "after"), name = "split.by") {
  ## if date in format dd/mm/yyyy hh:mm (basic check)
  if (missing(mydata)) stop("No data frame was supplied!")

  mydata <- checkPrep(mydata, names(mydata), "default", remove.calm = FALSE)
  ## check there are sufficent labels for number of dates
  if (length(dates) != length(labels) - 1) {
    stop("There is a mis-match between dates and labels. There should be
one more label than date")
  }

  if (length(grep("/", as.character(dates))) > 0) {
    if (class(mydata$date)[1] == "Date") {
      dates <- as_date(as.POSIXct(strptime(dates, "%d/%m/%Y"), "GMT"))
    } else {
      dates <- as.POSIXct(strptime(dates, "%d/%m/%Y"), "GMT")
    }
  } else { ## asume format yyyy-mm-dd

    if (class(mydata$date)[1] == "Date") {
      dates <- as_date(dates)
    } else {
      dates <- as.POSIXct(dates, "GMT")
    }
  }


  mydata[, name] <- cut(
    as.numeric(mydata$date),
    breaks = c(
      0, as.numeric(dates),
      max(mydata$date)
    ), labels = labels,
    ordered_result = TRUE
  )
  mydata
}
#############################################################################################

## function to make it easy to use d/m/y format for subsetting by date


##' Subset a data frame based on date
##'
##' Utility function to make it easier to select periods from a data frame
##' before sending to a function
##'
##' This function makes it much easier to select periods of interest from a data
##' frame based on dates in a British format. Selecting date/times in R format
##' can be intimidating for new users. This function can be used to select quite
##' complex dates simply - see examples below.
##'
##' Dates are assumed to be inclusive, so \code{start = "1/1/1999"} means that
##' times are selected from hour zero. Similarly, \code{end = "31/12/1999"} will
##' include all hours of the 31st December. \code{start} and \code{end} can also
##' be in standard R format as a string i.e. "YYYY-mm-dd", so \code{start =
##' "1999-01-01"} is fine.
##'
##' All options are applied in turn making it possible to select quite complex
##' dates
##'
##' @param mydata A data frame containing a \code{date} field in hourly or high
##'   resolution format.
##' @param start A start date string in the form d/m/yyyy e.g. \dQuote{1/2/1999}
##'   or in \sQuote{R} format i.e. \dQuote{YYYY-mm-dd}, \dQuote{1999-02-01}
##' @param end See \code{start} for format.
##' @param year A year or years to select e.g. \code{year = 1998:2004} to select
##'   1998-2004 inclusive or \code{year = c(1998, 2004)} to select 1998 and
##'   2004.
##' @param month A month or months to select. Can either be numeric e.g.
##'   \code{month = 1:6} to select months 1-6 (January to June), or by name e.g.
##'   \code{month = c("January", "December")}. Names can be abbreviated to 3
##'   letters and be in lower or upper case.
##' @param day A day name or or days to select. \code{day} can be numeric (1 to
##'   31) or character. For example \code{day = c("Monday", "Wednesday")} or
##'   \code{day = 1:10} (to select the 1st to 10th of each month). Names can be
##'   abbreviated to 3 letters and be in lower or upper case. Also accepts
##'   \dQuote{weekday} (Monday - Friday) and \dQuote{weekend} for convenience.
##' @param hour An hour or hours to select from 0-23 e.g. \code{hour = 0:12} to
##'   select hours 0 to 12 inclusive.
##' @importFrom lubridate dst year month hour wday force_tz day as_date dmy ymd_hm
##'   round_date parse_date_time floor_date ceiling_date
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## select all of 1999
##' data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999")
##' head(data.1999)
##' tail(data.1999)
##'
##' # or...
##' data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31")
##'
##' # easier way
##' data.1999 <- selectByDate(mydata, year = 1999)
##'
##'
##' # more complex use: select weekdays between the hours of 7 am to 7 pm
##' sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)
##'
##' # select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
##' sub.data <- selectByDate(mydata, day = "weekend", hour = 7:19, month =
##' c("dec", "jan", "feb"))
##' 
selectByDate <- function(mydata, start = "1/1/2008",
                         end = "31/12/2008", year = 2008,
                         month = 1, day = "weekday", hour = 1) {
  ## extract variables of interest
  vars <- names(mydata)

  ## check data - mostly date format
  mydata <- checkPrep(
    mydata, vars, "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )

  weekday.names <- format(ISOdate(2000, 1, 3:9), "%A")


  if (!missing(start)) {


    ## assume R date format
    start <- as_date(parse_date_time(start, c("ymd", "dmy")))

    mydata <- subset(mydata, as_date(date) >= start)
  }
  
  if (!missing(end)) {
    
    
    ## assume R date format
    end <-as_date(parse_date_time(end, c("ymd", "dmy")))
    
    mydata <- subset(mydata, as_date(date) <= end)
  }
  

  if (!missing(year)) {
    mydata <- mydata[which(year(mydata$date) %in% year), ]
  }


  if (!missing(month)) {
    if (is.numeric(month)) {
      if (any(month < 1 | month > 12)) {
        stop("Month must be between 1 to 12.")
      }

      mydata <- mydata[which(month(mydata$date) %in% month), ]
    }

    else {
      mydata <- subset(mydata, substr(tolower(format(
        date,
        "%B"
      )), 1, 3) %in% substr(tolower(month), 1, 3))
    }
  }
  if (!missing(hour)) {
    if (any(hour < 0 | hour > 23)) stop("Hour must be between 0 to 23.")

    mydata <- mydata[which(hour(mydata$date) %in% hour), ]
  }

  if (!missing(day)) {
    days <- day

    if (is.numeric(day)) {
      if (any(day < 1 | day > 31)) {
        stop("Day must be between 1 to 31.")
      }
      mydata <- mydata[which(day(mydata$date) %in% day), ]
    } else {
      if (day[1] == "weekday") {
        days <- weekday.names[1:5]
      }
      if (day[1] == "weekend") {
        days <- weekday.names[6:7]
      }
      mydata <- subset(mydata, substr(tolower(format(date, "%A")), 1, 3) %in%
        substr(tolower(days), 1, 3))
    }
  }
  mydata
}


## from Deepayan Sarkar
panel.smooth.spline <-
  function(x, y,
           w = NULL, df, spar = NULL, cv = FALSE,
           lwd = lwd, lty = plot.line$lty, col, col.line = plot.line$col,
           type, horizontal = FALSE, all.knots = TRUE, ...) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) {
      return()
    }
    if (!missing(col)) {
      if (missing(col.line)) {
        col.line <- col
      }
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
      spline <-
        smooth.spline(
          y[ok], x[ok],
          w = w, df = df, spar = spar, cv = cv
        )
      panel.lines(
        x = spline$y, y = spline$x, col = col.line,
        lty = lty, lwd = lwd, ...
      )
    }
    else {
      spline <-
        smooth.spline(
          x[ok], y[ok],
          w = w, df = df, spar = spar, cv = cv
        )
      panel.lines(
        x = spline$x, y = spline$y, col = col.line,
        lty = lty, lwd = lwd, ...
      )
    }
  }

### panel functions for plots based on lattice ####################################################

panel.gam <- function(x, y, form = y ~ x, method = "loess", k = k, Args, ..., simulate = FALSE, n.sim = 200,
                      autocor = FALSE, se = TRUE,
                      level = 0.95, n = 100, col = plot.line$col, col.se = col,
                      lty = plot.line$lty, lwd = plot.line$lwd, alpha = plot.line$alpha,
                      alpha.se = 0.20, border = NA, subscripts, group.number, group.value,
                      type, col.line, col.symbol, fill, pch, cex, font, fontface,
                      fontfamily) {

  ## panel function to add a smooth line to a plot
  ## Uses a GAM (mgcv) to fit smooth
  ## Optionally can plot 95% confidence intervals and run bootstrap simulations
  ## to estimate uncertainties. Simple block bootstrap is also available for correlated data

  ## get rid of R check annoyances#
  plot.line <- NULL

  thedata <- data.frame(x = x, y = y)
  thedata <- na.omit(thedata)

  tryCatch({
    if (!simulate) {
      if (is.null(k)) {
        mod <- suppressWarnings(gam(y ~ s(x), select = TRUE, data = thedata, ...))
      } else {
        mod <- suppressWarnings(gam(y ~ s(x, k = k), select = TRUE, data = thedata, ...))
      }


      lims <- current.panel.limits()
      xrange <- c(max(min(lims$x), min(x, na.rm = TRUE)), min(max(lims$x), max(x, na.rm = TRUE)))
      xseq <- seq(xrange[1], xrange[2], length = n)

      ## for uncertainties
      std <- qnorm(level / 2 + 0.5)

      pred <- predict(mod, data.frame(x = xseq), se = TRUE)

      panel.lines(xseq, pred$fit, col = col, alpha = alpha, lty = lty, lwd = 2)

      results <- data.frame(
        date = xseq, pred = pred$fit,
        lower = pred$fit - std * pred$se,
        upper = pred$fit + std * pred$se
      )

      if (se) {
        panel.polygon(
          x = c(xseq, rev(xseq)), y = c(pred$fit -
            std * pred$se, rev(pred$fit + std * pred$se)),
          col = col.se, alpha = alpha.se, border = border
        )
        pred <- pred$fit
      }
      
    } else { ## simulations required

      x <- thedata$x
      y <- thedata$y
      
      sam.size <- length(x)

      lims <- current.panel.limits()
      xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
      xseq <- seq(xrange[1], xrange[2], length = sam.size)

      boot.pred <- matrix(nrow = sam.size, ncol = n.sim)

      print("Taking bootstrap samples. Please wait...")

      ## set up bootstrap
      block.length <- 1

      if (autocor) block.length <- round(sam.size ^ (1 / 3))
      index <- samp.boot.block(sam.size, n.sim, block.length)

      ## predict first
      if (is.null(k)) {
        mod <- gam(y ~ s(x), data = thedata, ...)
      } else {
        mod <- gam(y ~ s(x, k = k), data = thedata, ...)
      }

      residuals <- residuals(mod) ## residuals of the model

      pred.input <- predict(mod, thedata)

      for (i in 1:n.sim) {
        ## make new data
        new.data <- data.frame(x = xseq, y = pred.input + residuals[index[, i]])

        mod <- gam(y ~ s(x), data = new.data, ...)

        pred <- predict(mod, new.data)

        boot.pred[, i] <- as.vector(pred)
      }

      ## calculate percentiles
      percentiles <- apply(boot.pred, 1, function(x) quantile(x, probs = c(0.025, 0.975)))

      results <- as.data.frame(cbind(
        pred = rowMeans(boot.pred),
        lower = percentiles[1, ], upper = percentiles[2, ]
      ))

      if (se) {
        panel.polygon(
          x = c(xseq, rev(xseq)), y = c(results$lower, rev(results$upper)),
          col = col.se, alpha = alpha.se, border = border
        )
      }

      panel.lines(xseq, pred.input, col = col, alpha = alpha, lty = lty, lwd = 2)
    }
    results
  }, error = function(x) return)
}


## version of GAM fitting not for plotting - need to rationalise both...
fitGam <- function(thedata, x = "date", y = "conc", form = y ~ x, k = k,
                   Args, ..., simulate = FALSE, n.sim = 200, autocor = FALSE, se = TRUE,
                   level = 0.95, n = 100) {

  ## panel function to add a smooth line to a plot
  ## Uses a GAM (mgcv) to fit smooth
  ## Optionally can plot 95% confidence intervals and run bootstrap simulations
  ## to estimate uncertainties. Simple block bootstrap is also available for correlated data

  data.orig <- thedata ## return this if all else fails

  id <- which(names(thedata) == x)
  names(thedata)[id] <- "x"
  id <- which(names(thedata) == y)
  names(thedata)[id] <- "y"

  # can only fit numeric, so convert back after fitting
  class_x <- class(thedata$x)

  thedata$x <- as.numeric(thedata$x)

  tryCatch({
    if (!simulate) {
      if (is.null(k)) {
        mod <- suppressWarnings(gam(y ~ s(x), select = TRUE, data = thedata, ...))
      } else {
        mod <- suppressWarnings(gam(y ~ s(x, k = k), select = TRUE, data = thedata, ...))
      }

      xseq <- seq(min(thedata$x, na.rm = TRUE), max(thedata$x, na.rm = TRUE), length = n)

      ## for uncertainties
      std <- qnorm(level / 2 + 0.5)

      pred <- predict(mod, data.frame(x = xseq), se = se)


      results <- data.frame(
        date = xseq, pred = pred$fit,
        lower = pred$fit - std * pred$se,
        upper = pred$fit + std * pred$se
      )
    } else { ## simulations required

      sam.size <- nrow(thedata)

      xseq <- seq(min(thedata$x, na.rm = TRUE), max(thedata$x, na.rm = TRUE), length = n)

      boot.pred <- matrix(nrow = sam.size, ncol = n.sim)

      print("Taking bootstrap samples. Please wait...")

      ## set up bootstrap
      block.length <- 1

      if (autocor) block.length <- round(sam.size ^ (1 / 3))
      index <- samp.boot.block(sam.size, n.sim, block.length)

      ## predict first
      if (is.null(k)) {
        mod <- gam(y ~ s(x), data = thedata, ...)
      } else {
        mod <- gam(y ~ s(x, k = k), data = thedata, ...)
      }

      residuals <- residuals(mod) ## residuals of the model

      pred.input <- predict(mod, thedata)

      for (i in 1:n.sim) {
        ## make new data
        new.data <- data.frame(x = xseq, y = pred.input + residuals[index[, i]])

        mod <- gam(y ~ s(x), data = new.data, ...)

        pred <- predict(mod, new.data)

        boot.pred[, i] <- as.vector(pred)
      }

      ## calculate percentiles
      percentiles <- apply(boot.pred, 1, function(x) quantile(x, probs = c(0.025, 0.975)))

      results <- as.data.frame(cbind(
        pred = rowMeans(boot.pred),
        lower = percentiles[1, ], upper = percentiles[2, ]
      ))
    }

    # convert class back to orginal
    class(results[[x]]) <- class_x
    return(results)
  }, error = function(x) {
    data.orig
  })
}


#########################################################################################################



## error in mean from Hmisc

errorInMean <- function(x, mult = qt((1 + conf.int) / 2, n - 1), conf.int = 0.95,
                        na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if (n < 2) {
    return(c(Mean = mean(x), Lower = NA, Upper = NA))
  }
  xbar <- sum(x) / n
  se <- sqrt(sum((x - xbar) ^ 2) / n / (n - 1))
  c(Mean = xbar, Lower = xbar - mult * se, Upper = xbar + mult *
    se)
}

## bootsrap confidence intervals in the mean from Hmisc
bootMean <- function(x, conf.int = 0.95, B = 1000, ...) {
  x <- x[!is.na(x)] # remove missings
  n <- length(x)
  xbar <- mean(x)
  if (n < 2) {
    return(c(Mean = xbar, Lower = NA, Upper = NA))
  }
  z <- unlist(lapply(1:B, function(i, x, N)
    sum(x[(sample.int(N, N, TRUE, NULL))]), x = x, N = n)) / n
  quant <- quantile(z, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  names(quant) <- NULL
  res <- c(Mean = xbar, Lower = quant[1], Upper = quant[2])

  res
}


#' Bootsrap confidence intervals in the mean
#'
#' A utility function to calculation the uncertainty intervals in the mean of a
#' vector. The function removes any missing data before the calculation.
#'
#' @param x A vector from which the mean and bootstrap confidence intervals in
#'   the mean are to be calculated
#' @param conf.int The confidence interval; default = 0.95.
#' @param B The number of bootstrap simulations
#'
#' @return Returns a data frame with the mean, lower uncertainty, upper
#'   uncertainty and number of values used in the calculation
#' @export
#'
#' @examples
#' test <- rnorm(20, mean = 10)
#' bootMeanDF(test)
bootMeanDF <- function(x, conf.int = 0.95, B = 1000) {
  if (!is.vector(x)) {
    stop("x should be a vector.")
  }

  res <- bootMean(x = x, conf.int = conf.int, B = B)
  res <- data.frame(mean = res[1], min = res[2], max = res[3], n = length(na.omit(x)))
  res <- return(res)
}


bootMeanDiff <- function(mydata, x = "x", y = "y", conf.int = 0.95, B = 1000) {

  ## calculates bootstrap mean differences
  ## assumes y - x
  x.name <- x
  y.name <- y
  x <- na.omit(mydata[[x]])
  y <- na.omit(mydata[[y]])
  Mean <- mean(y) - mean(x)

  if (nrow(mydata) < 2) {
    res1 <- data.frame(variable = x.name, Mean = mean(x), Lower = NA, Upper = NA,
                       stringsAsFactors = FALSE)
    
    res2 <- data.frame(variable = y.name, Mean = mean(y), Lower = NA, Upper = NA,
                       stringsAsFactors = FALSE)
    
    res <- data.frame(variable = paste(y.name, "-", x.name), Mean = Mean, 
                      Lower = NA, Upper = NA,
                      stringsAsFactors = FALSE)

    res <- bind_rows(res1, res2, res)
    res$variable <- factor(res$variable)
    return(res)
  }

  x <- bootMean(x, B = B)
  y <- bootMean(y, B = B)
  quant1 <- quantile(x, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  quant2 <- quantile(y, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  quant <- quantile(y - x, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  names(quant1) <- NULL
  names(quant2) <- NULL
  names(quant) <- NULL

  res1 <- data.frame(variable = x.name, Mean = mean(x), 
                     Lower = quant1[1], Upper = quant1[2],
                     stringsAsFactors = FALSE)
  
  res2 <- data.frame(variable = y.name, Mean = mean(y), Lower = quant2[1], 
                     Upper = quant2[2],
                     stringsAsFactors = FALSE)
  
  res <- data.frame(variable = paste(y.name, "-", x.name), 
                    Mean = Mean, Lower = quant[1], Upper = quant[2],
                    stringsAsFactors = FALSE)

  res <- bind_rows(res1, res2, res)
  res$variable <- factor(res$variable)
  res
}

###########################################################################################################

## list update function
## for lattice type object structure and ... handling

## (currently used by)
## (all openair plots that include colorkey controlled by drawOpenKey)

## listUpdate function
# [in development]
listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}

#############################################################################################################

## makeOpenKeyLegend v0.1

## common code for making legend list
## objects for use with drawOpenkey outputs

## uses listUpdate in utilities

makeOpenKeyLegend <- function(key, default.key, fun.name = "function") {
  # handle logicals and lists
  if (is.logical(key)) {
    legend <- if (key) default.key else NULL
  } else if (is.list(key)) {
    legend <- listUpdate(default.key, key)
  } else {
    if (!is.null(key)) {
      warning(
        paste(
          "In ", fun.name, "(...):\n unrecognised key not exported/applied\n",
          " [see ?drawOpenKey for key structure/options]",
          sep = ""
        ),
        call. = FALSE
      )
    }
    legend <- NULL
  }

  # structure like legend for drawOpenKey
  if (!is.null(legend)) {
    legend <- list(right = list(
      fun = drawOpenKey, args = list(key = legend),
      draw = FALSE
    ))
    if ("space" %in% names(legend$right$args$key)) {
      names(legend)[[1]] <- legend$right$args$key$space
    }
  }
  legend
}

## polygon that can deal with missing data for use in lattice plots with groups
poly.na <- function(x1, y1, x2, y2, group.number, myColors, alpha = 0.4, border = NA) {
  for (i in seq(2, length(x1)))
    if (!any(is.na(y2[c(i - 1, i)]))) {
      lpolygon(
        c(x1[i - 1], x1[i], x2[i], x2[i - 1]),
        c(y1[i - 1], y1[i], y2[i], y2[i - 1]),
        col = myColors[group.number], border = border, alpha = alpha
      )
    }
}


## gives names of lattice strips
strip.fun <- function(results.grid, type, auto.text) {
  ## proper names of labelling ###################################################
  pol.name <- sapply(
    levels(factor(results.grid[[type[1]]])),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)

  if (length(type) == 1) {
    strip.left <- FALSE
  } else { ## two conditioning variables

    pol.name <- sapply(
      levels(factor(results.grid[[type[2]]])),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
  list(strip, strip.left, pol.name)
}



## from lattice
chooseFace <- function(fontface = NULL, font = 1) {
  if (is.null(fontface)) {
    font
  } else {
    fontface
  }
}


## .smoothScatterCalcDensity() is also in graphics, but not exported.
.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x) {
  if (!("KernSmooth" %in% loadedNamespaces())) {
    ns <- try(loadNamespace("KernSmooth"))
    if (isNamespace(ns)) {
      message("(loaded the KernSmooth namespace)")
    } else {
      stop("panel.smoothScatter() requires the KernSmooth package, but unable to load KernSmooth namespace")
    }
  }
  if (length(nbin) == 1) {
    nbin <- c(nbin, nbin)
  }
  if (!is.numeric(nbin) || (length(nbin) != 2)) stop("'nbin' must be numeric of length 1 or 2")
  if (missing(bandwidth)) {
    bandwidth <- diff(apply(x, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE)) / 25
  } else {
    if (!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
  }
  bandwidth[bandwidth == 0] <- 1
  ## create density map
  if (missing(range.x)) {
    rv <- KernSmooth::bkde2D(x, gridsize = nbin, bandwidth = bandwidth)
  } else {
    rv <- KernSmooth::bkde2D(x, gridsize = nbin, bandwidth = bandwidth, range.x = range.x)
  }
  rv$bandwidth <- bandwidth
  return(rv)
}




## simple rounding function from plyr
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

## pretty gap calculator
prettyGap <- function(x, n = 100) {
  return(diff(pretty(x, n))[1])
}

# function to check variables are numeric, if not force with warning
checkNum <- function(mydata, vars) {
  for (i in seq_along(vars)) {
    if (!is.numeric(mydata[[vars[i]]])) {
      mydata[[vars[i]]] <- as.numeric(as.character(mydata[[vars[i]]]))

      warning(
        paste(vars[i], "is not numeric, forcing to numeric..."),
        call. = FALSE
      )
    }
  }

  return(mydata)
}



#' Bin data, calculate mean and bootstrap 95\% confidence interval in the mean
#'
#' Bin a variable and calculate mean an uncertainties in mean
#'
#' This function summarises data by intervals and calculates the mean and
#' bootstrap 95\% confidence intervals in the mean of a chosen variable in a data
#' frame. Any other numeric variables are summarised by their mean intervals.
#'
#' There are three options for binning. The default is to bon \code{bin} into 40
#' intervals. Second, the user can choose an binning interval e.g.
#' \code{interval = 5}. Third, the user can supply their own breaks to use as
#' binning intervals.
#'
#' @param mydata Name of the data frame to process.
#' @param bin The name of the column to divide into intervals
#' @param uncer The name of the column for which the mean, lower and upper
#'   uncertainties should be calculated for each interval of \code{bin}.
#' @param n The number of intervals to split \code{bin} into.
#' @param interval The interval to be used for binning the data.
#' @param breaks User specified breaks to use for binning.
#'
#' @return Retruns a summarised data frame with new columns for the mean and
#'   upper / lower 95\% confidence intervals in the mean.
#' @export
#'
#' @examples
#' # how does nox vary by intervals of wind speed?
#' results <- binData(mydata, bin = "ws", uncer = "nox")
#'
#' # easy to plot this using ggplot2
#' \dontrun{
#' library(ggplot2)
#' ggplot(results, aes(ws, mean, ymin = min, ymax = max)) +
#' geom_pointrange()
#'
#' }
binData <- function(mydata, bin = "nox", uncer = "no2", n = 40, interval = NA,
                    breaks = NA) {
  if (!is.na(interval)) {
    mydata$interval <- cut(
      mydata[[bin]], sort(unique(round_any(mydata[[bin]], interval))),
      include.lowest = TRUE
    )
  } else if (!anyNA(breaks)) {
    mydata$interval <- cut(mydata[[bin]], breaks = breaks, include.lowest = TRUE)
  } else {
    mydata$interval <- cut(mydata[[bin]], breaks = n)
  }

  # calculate 95% CI in mean
  uncert <- group_by(mydata, interval) %>%
    do(bootMeanDF(.[[uncer]]))

  mydata <- group_by(mydata, interval) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

  mydata <- inner_join(mydata, uncert, by = "interval")

  mydata
}

# see forecast package
auto.arima <- function(y, d=NA, D=NA, max.p=5, max.q=5,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=2, start.q=2, start.P=1, start.Q=1,
                       stationary=FALSE, seasonal=TRUE, ic=c("aicc", "aic", "bic"),
                       stepwise=TRUE, nmodels = 94, trace=FALSE,
                       approximation=(length(x) > 150 | frequency(x) > 12),
                       method = NULL, truncate=NULL, xreg=NULL,
                       test=c("kpss", "adf", "pp"), test.args = list(),
                       seasonal.test=c("seas", "ocsb", "hegy", "ch"), seasonal.test.args = list(),
                       allowdrift=TRUE, allowmean=TRUE, lambda=NULL, biasadj=FALSE,
                       parallel=FALSE, num.cores=2, x=y, ...) {
  
  Arima = BoxCox = arimaorder = ndiffs = search.arima = NULL
  # Only non-stepwise parallel implemented so far.
  if (stepwise && parallel) {
    warning("Parallel computer is only implemented when stepwise=FALSE, the model will be fit in serial.")
    parallel <- FALSE
  }
  
  if (trace && parallel) {
    message("Tracing model searching in parallel is not supported.")
    trace <- FALSE
  }
  
  series <- deparse(substitute(y))
  x <- as.ts(x)
  if (NCOL(x) > 1) {
    stop("auto.arima can only handle univariate time series")
  }
  
  # Trim leading NAs and find length of non-missing data
  orig.x <- x
  missing <- is.na(x)
  firstnonmiss <- head(which(!missing),1)
  lastnonmiss <- tail(which(!missing),1)
  serieslength <- lastnonmiss - firstnonmiss + 1
  
  # Trim initial missing values
  x <- subset(x, start=firstnonmiss)
  
  # Check for constant data
  if (is.constant(x)) {
    if(all(is.na(x)))
      stop("All data are missing")
    if (allowmean) {
      fit <- Arima(x, order = c(0, 0, 0), fixed = mean(x, na.rm = TRUE), ...)
    } else {
      fit <- Arima(x, order = c(0, 0, 0), include.mean = FALSE, ...)
    }
    fit$x <- orig.x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x = x)
    fit$constant <- TRUE
    return(fit)
  }
  ic <- match.arg(ic)
  test <- match.arg(test)
  seasonal.test <- match.arg(seasonal.test)
  
  # Only consider non-seasonal models
  if (seasonal) {
    m <- frequency(x)
  } else {
    m <- 1
  }
  if (m < 1) {
    # warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    m <- 1
  }
  else {
    m <- round(m)
  } # Avoid non-integer seasonal periods
  
  max.p <- min(max.p, floor(serieslength / 3))
  max.q <- min(max.q, floor(serieslength / 3))
  max.P <- min(max.P, floor(serieslength / 3 / m))
  max.Q <- min(max.Q, floor(serieslength / 3 / m))
  
  # Use AIC if npar <= 3
  # AICc won't work for tiny samples.
  if (serieslength <= 3L) {
    ic <- "aic"
  }
  
  # Transform data if requested
  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
    lambda <- attr(x, "lambda")
    attr(lambda, "biasadj") <- biasadj
  }
  
  # Check xreg and do regression if necessary
  if (!is.null(xreg)) {
    # Make sure it is a matrix with column names
    if("data.frame" %in% class(xreg))
      stop("xreg should be a numeric matrix or vector")
    nmxreg <- deparse(substitute(xreg))
    xregg <- as.matrix(xreg)
    if (ncol(xregg) == 1 && length(nmxreg) > 1) {
      nmxreg <- "xreg"
    }
    if (is.null(colnames(xregg))) {
      colnames(xregg) <- if (ncol(xregg) == 1) {
        nmxreg
      } else {
        paste(nmxreg, 1:ncol(xregg), sep = "")
      }
    }
    
    # Check that xreg is not rank deficient
    # First check if any columns are constant
    constant_columns <- apply(xregg, 2, is.constant)
    if (any(constant_columns)) { # Remove first one
      xregg <- xregg[, -which(constant_columns)[1]]
    }
    
    # Now check if it is rank deficient
    sv <- svd(na.omit(cbind(rep(1, NROW(xregg)), xregg)))$d
    if (min(sv) / sum(sv) < .Machine$double.eps) {
      stop("xreg is rank deficient")
    }
    
    # Finally find residuals from regression in order
    # to estimate appropriate level of differencing
    j <- !is.na(x) & !is.na(rowSums(xregg))
    xx <- x
    xx[j] <- residuals(lm(x ~ xregg))
  }
  else {
    xx <- x
    xregg <- NULL
  }
  
  # Choose order of differencing
  if (stationary) {
    d <- D <- 0
  }
  if (m == 1) {
    D <- max.P <- max.Q <- 0
  }
  else if(is.na(D))
  {
    D <- do.call("nsdiffs", c(list(xx, test=seasonal.test, max.D=max.D), seasonal.test.args))
    # Make sure xreg is not null after differencing
    if (D > 0 && !is.null(xregg)) {
      diffxreg <- diff(xregg, differences = D, lag = m)
      if (any(apply(diffxreg, 2, is.constant))) {
        D <- D - 1
      }
    }
    # Make sure xx is not all missing after differencing
    if (D > 0) {
      dx <- diff(xx, differences = D, lag = m)
      if (all(is.na(dx)))
        D <- D - 1
    }
  }
  if (D > 0) {
    dx <- diff(xx, differences = D, lag = m)
  } else {
    dx <- xx
  }
  if (!is.null(xregg)) {
    if (D > 0) {
      diffxreg <- diff(xregg, differences = D, lag = m)
    } else {
      diffxreg <- xregg
    }
  }
  if (is.na(d)) {
    d <- do.call("ndiffs", c(list(dx, test = test, max.d = max.d), test.args))
    # Make sure xreg is not null after differencing
    if (d > 0 && !is.null(xregg)) {
      diffxreg <- diff(diffxreg, differences = d, lag = 1)
      if (any(apply(diffxreg, 2, is.constant))) {
        d <- d - 1
      }
    }
    # Make sure dx is not all missing after differencing
    if (d > 0) {
      diffdx <- diff(dx, differences=d, lag=1)
      if(all(is.na(diffdx)))
        d <- d - 1
    }
  }
  
  # Check number of differences selected
  if (D >= 2) {
    warning("Having more than one seasonal differences is not recommended. Please consider using only one seasonal difference.")
  } else if (D + d > 2) {
    warning("Having 3 or more differencing operations is not recommended. Please consider reducing the total number of differences.")
  }
  
  if (d > 0) {
    dx <- diff(dx, differences = d, lag = 1)
  }
  
  if (is.constant(dx)) {
    if (is.null(xreg)) {
      if (D > 0) {
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m), include.constant = TRUE, fixed = mean(dx/m, na.rm = TRUE), method = method, ...)
      } else if (d < 2) {
        fit <- Arima(x, order = c(0, d, 0), include.constant = TRUE, fixed = mean(dx, na.rm = TRUE), method = method, ...)
      } else {
        stop("Data follow a simple polynomial and are not suitable for ARIMA modelling.")
      }
    }
    else # Perfect regression
    {
      if (D > 0) {
        fit <- Arima(x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m), xreg = xreg, method = method, ...)
      } else {
        fit <- Arima(x, order = c(0, d, 0), xreg = xreg, method = method, ...)
      }
    }
    fit$x <- orig.x
    fit$series <- series
    fit$call <- match.call()
    fit$call$x <- data.frame(x = x)
    return(fit)
  }
  
  if (m > 1) {
    if (max.P > 0) {
      max.p <- min(max.p, m - 1)
    }
    if (max.Q > 0) {
      max.q <- min(max.q, m - 1)
    }
  }
  
  # Find constant offset for AIC calculation using white noise model
  if (approximation) {
    if (!is.null(truncate)) {
      tspx <- tsp(x)
      if (length(x) > truncate) {
        x <- ts(tail(x, truncate), end = tspx[2], frequency = tspx[3])
      }
    }
    if (D == 0) {
      fit <- try(stats::arima(x, order = c(0, d, 0), xreg = xreg, ...), silent = TRUE)
    } else {
      fit <- try(stats::arima(
        x, order = c(0, d, 0), seasonal = list(order = c(0, D, 0), period = m),
        xreg = xreg, ...
      ), silent = TRUE)
    }
    if (!is.element("try-error", class(fit))) {
      offset <- -2 * fit$loglik - serieslength * log(fit$sigma2)
    } else # Not sure this should ever happen
    {
      # warning("Unable to calculate AIC offset")
      offset <- 0
    }
  }
  else {
    offset <- 0
  }
  
  allowdrift <- allowdrift & (d + D) == 1
  allowmean <- allowmean & (d + D) == 0
  
  constant <- allowdrift | allowmean
  
  if (approximation && trace) {
    cat("\n Fitting models using approximations to speed things up...\n")
  }
  
  if (!stepwise) {
    bestfit <- search.arima(
      x, d, D, max.p, max.q, max.P, max.Q, max.order, stationary,
      ic, trace, approximation, method = method, xreg = xreg, offset = offset,
      allowdrift = allowdrift, allowmean = allowmean,
      parallel = parallel, num.cores = num.cores, ...
    )
    # Check stationarity of regression residuals.
    if (!is.null(xreg)) {
      d <- arimaorder(bestfit)[2]
      D <- arimaorder(bestfit)[5]
      res <- residuals(bestfit, type = "regression")
      if (ndiffs(res) != d) {
        # Refit model with revised differencing
        bestfit <- search.arima(
          x, d = ndiffs(res), D, max.p, max.q, max.P, max.Q, max.order, stationary,
          ic, trace, approximation, method = method, xreg = xreg, offset = offset,
          allowdrift = allowdrift, allowmean = allowmean,
          parallel = parallel, num.cores = num.cores, ...
        )
      }
    }
    bestfit$call <- match.call()
    bestfit$call$x <- data.frame(x = x)
    bestfit$lambda <- lambda
    bestfit$x <- orig.x
    bestfit$series <- series
    bestfit$fitted <- fitted(bestfit)
    if (trace) {
      cat("\n\n Best model:", arima.string(bestfit, padding = TRUE), "\n\n")
    }
    return(bestfit)
  }
  
  # Starting model
  if (length(x) < 10L) {
    start.p <- min(start.p, 1L)
    start.q <- min(start.q, 1L)
    start.P <- 0L
    start.Q <- 0L
  }
  p <- start.p <- min(start.p, max.p)
  q <- start.q <- min(start.q, max.q)
  P <- start.P <- min(start.P, max.P)
  Q <- start.Q <- min(start.Q, max.Q)
  
  results <- matrix(NA, nrow = nmodels, ncol = 8)
  
  bestfit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
  results[1, ] <- c(p, d, q, P, D, Q, constant, bestfit$ic)
  # Null model with possible constant
  fit <- myarima(x, order = c(0, d, 0), seasonal = c(0, D, 0), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
  results[2, ] <- c(0, d, 0, 0, D, 0, constant, fit$ic)
  if (fit$ic < bestfit$ic) {
    bestfit <- fit
    p <- q <- P <- Q <- 0
  }
  k <- 2
  # Basic AR model
  if (max.p > 0 || max.P > 0) {
    fit <- myarima(x, order = c(max.p > 0, d, 0), seasonal = c((m > 1) & (max.P > 0), D, 0), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
    results[3, ] <- c(max.p > 0, d, 0, (m > 1) & (max.P > 0), D, 0, constant, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- (max.p > 0)
      P <- (m > 1) & (max.P > 0)
      q <- Q <- 0
    }
    k <- k + 1
  }
  # Basic MA model
  if (max.q > 0 || max.Q > 0) {
    fit <- myarima(x, order = c(0, d, max.q > 0), seasonal = c(0, D, (m > 1) & (max.Q > 0)), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
    results[4, ] <- c(0, d, max.q > 0, 0, D, (m > 1) & (max.Q > 0), constant, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- P <- 0
      Q <- (m > 1) & (max.Q > 0)
      q <- (max.q > 0)
    }
    k <- k + 1
  }
  # Null model with no constant
  if (constant) {
    fit <- myarima(x, order = c(0, d, 0), seasonal = c(0, D, 0), constant = FALSE, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
    results[5, ] <- c(0, d, 0, 0, D, 0, 0, fit$ic)
    if (fit$ic < bestfit$ic) {
      bestfit <- fit
      p <- q <- P <- Q <- 0
    }
    k <- k + 1
  }
  
  startk <- 0
  while (startk < k && k < nmodels) {
    startk <- k
    if (P > 0 && newmodel(p, d, q, P - 1, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P - 1, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        P <- (P - 1)
        next
      }
    }
    if (Q > 0 && newmodel(p, d, q, P, D, Q - 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q - 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P, D, Q - 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        next
      }
    }
    if (P < max.P && newmodel(p, d, q, P + 1, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P + 1, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        P <- (P + 1)
        next
      }
    }
    if (Q < max.Q && newmodel(p, d, q, P, D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q + 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P, D, Q + 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
      }
    }
    if (Q > 0 && P > 0 && newmodel(p, d, q, P - 1, D, Q - 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q - 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P - 1, D, Q - 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        P <- (P - 1)
        next
      }
    }
    if (Q < max.Q && P > 0 && newmodel(p, d, q, P - 1, D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P - 1, D, Q + 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P - 1, D, Q + 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
        P <- (P - 1)
        next
      }
    }
    if (Q > 0 && P < max.P && newmodel(p, d, q, P + 1, D, Q - 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q - 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P + 1, D, Q - 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q - 1)
        P <- (P + 1)
        next
      }
    }
    if (Q < max.Q && P < max.P && newmodel(p, d, q, P + 1, D, Q + 1, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q), seasonal = c(P + 1, D, Q + 1), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q, P + 1, D, Q + 1, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        Q <- (Q + 1)
        P <- (P + 1)
        next
      }
    }
    
    if (p > 0 && newmodel(p - 1, d, q, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p - 1, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p - 1, d, q, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        p <- (p - 1)
        next
      }
    }
    if (q > 0 && newmodel(p, d, q - 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q - 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q - 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        next
      }
    }
    if (p < max.p && newmodel(p + 1, d, q, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p + 1, d, q), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p + 1, d, q, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        p <- (p + 1)
        next
      }
    }
    if (q < max.q && newmodel(p, d, q + 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p, d, q + 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p, d, q + 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        next
      }
    }
    if (q > 0 && p > 0 && newmodel(p - 1, d, q - 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p - 1, d, q - 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p - 1, d, q - 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        p <- (p - 1)
        next
      }
    }
    if (q < max.q && p > 0 && newmodel(p - 1, d, q + 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p - 1, d, q + 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p - 1, d, q + 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        p <- (p - 1)
        next
      }
    }
    if (q > 0 && p < max.p && newmodel(p + 1, d, q - 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p + 1, d, q - 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p + 1, d, q - 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q - 1)
        p <- (p + 1)
        next
      }
    }
    if (q < max.q && p < max.p && newmodel(p + 1, d, q + 1, P, D, Q, constant, results[1:k, ])) {
      k <- k + 1; if(k>nmodels) next
      fit <- myarima(x, order = c(p + 1, d, q + 1), seasonal = c(P, D, Q), constant = constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
      results[k, ] <- c(p + 1, d, q + 1, P, D, Q, constant, fit$ic)
      if (fit$ic < bestfit$ic) {
        bestfit <- fit
        q <- (q + 1)
        p <- (p + 1)
        next
      }
    }
    if (allowdrift || allowmean) {
      if (newmodel(p, d, q, P, D, Q, !constant, results[1:k, ])) {
        k <- k + 1; if(k>nmodels) next
        fit <- myarima(x, order = c(p, d, q), seasonal = c(P, D, Q), constant = !constant, ic, trace, approximation, method = method, offset = offset, xreg = xreg, ...)
        results[k, ] <- c(p, d, q, P, D, Q, !constant, fit$ic)
        if (fit$ic < bestfit$ic) {
          bestfit <- fit
          constant <- !constant
        }
      }
    }
  }
  
  if(k > nmodels){
    warning(sprintf("Stepwise search was stopped early due to reaching the model number limit: `nmodels = %i`", nmodels))
  }
  
  # Refit using ML if approximation used for IC
  if (approximation && !is.null(bestfit$arma)) {
    if (trace) {
      cat("\n\n Now re-fitting the best model(s) without approximations...\n")
    }
    icorder <- order(results[, 8])
    nmodels <- sum(!is.na(results[, 8]))
    for (i in seq(nmodels))
    {
      k <- icorder[i]
      fit <- myarima(
        x, order = c(results[k, 1], d, results[k, 3]),
        seasonal = c(results[k, 4], D, results[k, 6]),
        constant = results[k, 7] == 1,
        ic, trace, approximation = FALSE, method = method, xreg = xreg, ...
      )
      if (fit$ic < Inf) {
        bestfit <- fit
        break
      }
    }
  }
  
  # Nothing fitted
  if (bestfit$ic == Inf && !isTRUE(method=="CSS")) {
    if (trace) {
      cat("\n")
    }
    stop("No suitable ARIMA model found")
  }
  
  # Check stationarity of regression residuals.
  if (!is.null(xreg)) {
    d <- arimaorder(bestfit)[2]
    D <- arimaorder(bestfit)[5]
    res <- residuals(bestfit, type = "regression")
    if (ndiffs(res) != d) {
      # Refit model with revised differencing
      bestfit <- auto.arima(
        x, d = ndiffs(res), D=D, max.p=max.p, max.q=max.q, max.P=max.P, max.Q=max.Q,
        start.p=start.p, start.q=start.q, start.P=start.P, start.Q=start.Q,
        seasonal=seasonal, ic=ic, trace=trace, approximation=approximation,
        method = method, truncate=truncate, xreg=xreg,
        allowdrift=allowdrift, allowmean=allowmean, lambda=lambda, biasadj=biasadj,
        parallel=parallel, num.cores=num.cores, ...
      )
    }
  }
  
  # Return best fit
  bestfit$x <- orig.x
  bestfit$series <- series
  bestfit$ic <- NULL
  bestfit$call <- match.call()
  bestfit$call$x <- data.frame(x = x)
  bestfit$lambda <- lambda
  bestfit$fitted <- fitted(bestfit)
  
  if (trace) {
    cat("\n\n Best model:", arima.string(bestfit, padding = TRUE), "\n\n")
  }
  
  return(bestfit)
}

# Calls arima from stats package and adds data to the returned object
# Also allows refitting to new data
# and drift terms to be included.
myarima <- function(x, order = c(0, 0, 0), seasonal = c(0, 0, 0), constant=TRUE, ic="aic", trace=FALSE, approximation=FALSE, offset=0, xreg=NULL, method = NULL, ...) {
  # Length of non-missing interior
  missing <- is.na(x)
  firstnonmiss <- head(which(!missing),1)
  lastnonmiss <- tail(which(!missing),1)
  n <- lastnonmiss - firstnonmiss + 1
  m <- frequency(x)
  use.season <- (sum(seasonal) > 0) & m > 0
  diffs <- order[2] + seasonal[2]
  if(is.null(method)){
    if (approximation) {
      method <- "CSS"
    } else {
      method <- "CSS-ML"
    }
  }
  if (diffs == 1 && constant) {
    xreg <- cbind(drift = 1:length(x), xreg)
    if (use.season) {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, seasonal = list(order = seasonal, period = m), xreg = xreg, method = method, ...), silent = TRUE))
    } else {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, xreg = xreg, method = method, ...), silent = TRUE))
    }
  }
  else {
    if (use.season) {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, seasonal = list(order = seasonal, period = m), include.mean = constant, method = method, xreg = xreg, ...), silent = TRUE))
    } else {
      suppressWarnings(fit <- try(stats::arima(x = x, order = order, include.mean = constant, method = method, xreg = xreg, ...), silent = TRUE))
    }
  }
  if (is.null(xreg)) {
    nxreg <- 0
  } else {
    nxreg <- ncol(as.matrix(xreg))
  }
  if (!is.element("try-error", class(fit))) {
    nstar <- n - order[2] - seasonal[2] * m
    if (diffs == 1 && constant) {
      # fitnames <- names(fit$coef)
      # fitnames[length(fitnames)-nxreg] <- "drift"
      # names(fit$coef) <- fitnames
      fit$xreg <- xreg
    }
    npar <- length(fit$coef) + 1
    if (method == "CSS") {
      fit$aic <- offset + nstar * log(fit$sigma2) + 2 * npar
    }
    if (!is.na(fit$aic)) {
      fit$bic <- fit$aic + npar * (log(nstar) - 2)
      fit$aicc <- fit$aic + 2 * npar * (npar + 1) / (nstar - npar - 1)
      fit$ic <- switch(ic, bic = fit$bic, aic = fit$aic, aicc = fit$aicc)
    }
    else {
      fit$aic <- fit$bic <- fit$aicc <- fit$ic <- Inf
    }
    # Adjust residual variance to be unbiased
    fit$sigma2 <- sum(fit$residuals ^ 2, na.rm = TRUE) / (nstar - npar + 1)
    
    # Check for unit roots
    minroot <- 2
    if (order[1] + seasonal[1] > 0) {
      testvec <- fit$model$phi
      k <- abs(testvec) > 1e-8
      if (sum(k) > 0) {
        last.nonzero <- max(which(k))
      } else {
        last.nonzero <- 0
      }
      if (last.nonzero > 0) {
        testvec <- testvec[1:last.nonzero]
        proots <- try(polyroot(c(1,-testvec)))
        if (!is.element("try-error", class(proots))) {
          minroot <- min(minroot, abs(proots))
        }
        else fit$ic <- Inf
      }
    }
    if (order[3] + seasonal[3] > 0 & fit$ic < Inf) {
      testvec <- fit$model$theta
      k <- abs(testvec) > 1e-8
      if (sum(k) > 0) {
        last.nonzero <- max(which(k))
      } else {
        last.nonzero <- 0
      }
      if (last.nonzero > 0) {
        testvec <- testvec[1:last.nonzero]
        proots <- try(polyroot(c(1,testvec)))
        if (!is.element("try-error", class(proots))) {
          minroot <- min(minroot, abs(proots))
        }
        else fit$ic <- Inf
      }
    }
    if (minroot < 1 + 1e-2) { # Previously 1+1e-3
      fit$ic <- Inf
    } # Don't like this model
    if (trace) {
      cat("\n", arima.string(fit, padding = TRUE), ":", fit$ic)
    }
    fit$xreg <- xreg
    
    return(structure(fit, class = c("ARIMA", "Arima")))
  }
  else {
    # Catch errors due to unused arguments
    if (length(grep("unused argument", fit)) > 0L) {
      stop(fit[1])
    }
    
    if (trace) {
      cat("\n ARIMA(", order[1], ",", order[2], ",", order[3], ")", sep = "")
      if (use.season) {
        cat("(", seasonal[1], ",", seasonal[2], ",", seasonal[3], ")[", m, "]", sep = "")
      }
      if (constant && (order[2] + seasonal[2] == 0)) {
        cat(" with non-zero mean")
      } else if (constant && (order[2] + seasonal[2] == 1)) {
        cat(" with drift        ")
      } else if (!constant && (order[2] + seasonal[2] == 0)) {
        cat(" with zero mean    ")
      } else {
        cat("                   ")
      }
      cat(" :", Inf)
    }
    return(list(ic = Inf))
  }
}

newmodel <- function(p, d, q, P, D, Q, constant, results) {
  n <- nrow(results)
  for (i in 1:n)
  {
    if (identical(c(p, d, q, P, D, Q, constant), results[i, 1:7])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

arima.string <- function(object, padding=FALSE) {
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  m <- order[7]
  result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3], ")", sep = "")
  if (m > 1 && sum(order[4:6]) > 0) {
    result <- paste(result, "(", order[4], ",", order[5], ",", order[6], ")[", m, "]", sep = "")
  }
  if (padding && m > 1 && sum(order[4:6]) == 0) {
    result <- paste(result, "         ", sep = "")
    if (m <= 9) {
      result <- paste(result, " ", sep = "")
    } else if (m <= 99) {
      result <- paste(result, "  ", sep = "")
    } else {
      result <- paste(result, "   ", sep = "")
    }
  }
  if (!is.null(object$xreg)) {
    if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
      result <- paste(result, "with drift        ")
    } else {
      result <- paste("Regression with", result, "errors")
    }
  }
  else {
    if (is.element("constant", names(object$coef)) || is.element("intercept", names(object$coef))) {
      result <- paste(result, "with non-zero mean")
    } else if (order[2] == 0 && order[5] == 0) {
      result <- paste(result, "with zero mean    ")
    } else {
      result <- paste(result, "                  ")
    }
  }
  if (!padding) {
    # Strip trailing spaces
    result <- gsub("[ ]*$", "", result)
  }
  return(result)
}

# Check that Arima object has positive coefficient variances without returning warnings
checkarima <- function(object) {
  suppressWarnings(test <- any(is.nan(sqrt(diag(object$var.coef)))))
  return(test)
}

is.constant <- function(x) {
  x <- as.numeric(x)
  y <- rep(x[1], length(x))
  return(isTRUE(all.equal(x, y)))
}
