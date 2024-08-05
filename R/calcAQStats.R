#' Calculate user-defined air quality statistics
#'
#' [calcAQStats()] calculates user-defined annual air-quality statistics
#' constructed using [aqStat()]. This pair of functions allows users to flexibly
#' define air quality statistics and limits relevant to their local air quality
#' legislation and quickly track compliance.
#'
#' @rdname calc-aq-stats
#' @order 1
#'
#' @param mydata A data frame of time series. Must include a `date` field and
#'   any variables required by the provided `aqstats`.
#' @param aqstats A list of [aqStat()] objects to calculate.
#' @param period A time period to calculate statistics for. Currently, only
#'   `"year"` is supported, which will calculate annual statistics.
#' @param progress Show a progress bar when large numbers of statistics are
#'   being calculated? Defaults to `TRUE`.
#'
#' @author Jack Davison
#' @export
#'
#' @seealso [aqStats()], for a simpler but more prescriptive way of calculating
#'   air quality statistics
#'
#' @examples
#' # calculate some UK AQ limits
#' calcAQStats(
#'   mydata = openair::mydata,
#'   aqstats = list(
#'     # PM10 limits
#'     aqStat("pm10", "limit", "day", limit = 50),
#'     aqStat("pm10", "limit", "year", limit = 40),
#'     # NO2 limits
#'     aqStat("no2", "limit", "hour", limit = 200),
#'     aqStat("no2", "limit", "year", limit = 40),
#'     # O3 limit
#'     aqStat("o3", "limit", "hour", roll = 8L, limit = 100),
#'     # SO2 limits
#'     aqStat("so2", "limit", "15 min", limit = 266),
#'     aqStat("so2", "limit", "hour", limit = 350),
#'     aqStat("so2", "limit", "day", limit = 125),
#'     # CO limits
#'     aqStat("co", "limit", "hour", roll = 8L, roll.avg.time = "day", limit = 10)
#'   ),
#'   period = "year"
#' )
calcAQStats <- function(mydata,
                        aqstats,
                        period = c("year"),
                        progress = TRUE) {
  period <- match.arg(period)
  purrr::map(
    aqstats,
    ~ calculate_aqstat(mydata, .x, period = period),
    .progress = ifelse(progress, "Calculating Statistics", FALSE)
  ) %>%
    purrr::reduce(function(x, y)
      dplyr::left_join(x, y, by = "date"))
}

#' @rdname calc-aq-stats
#' @order 2
#'
#' @param pollutant The pollutant (or other named variable) of interest, e.g.,
#'   `"no2"`. This should align with a column name in the `data.frame` passed to
#'   [calcAQStats()].
#' @param stat The grand statistic to calculate. Simple statistics include
#'   `"mean"` (the default), `"min"`, `"median"` or `"max"`. Can also be
#'   `"percentile"`, the percentile being defined using the `percentile` option.
#'   Finally, can be `"limit"`, which compares values against the `limit` value
#'   and returns the number of instances where the value exceeds the limit.
#' @param avg.time An initial averaging time to calculate *before* the `stat` is
#'   calculated; passed directly to [timeAverage()].
#' @param roll If an integer value is provided, a rolling mean will be
#'   calculated *after* time averaging and before *stat*. For example, to
#'   calculate an 8-hour rolling mean set `avg.time = "1 hour", roll = 8L`.
#' @param roll.avg.time If `roll` is specified, users can set `roll.avg.time` to
#'   time average the rolling mean values. This could be useful to calculate a
#'   *max* daily running 8 hour mean to compare with an ozone limit, for example.
#' @param limit The limit value, for `stat = "limit"`.
#' @param percentile The percentile value, for `stat = "percentile"`.
#' @param name Optionally change the output column name for this air quality
#'   statistic.
#'
#' @author Jack Davison
#' @export
#' 
aqStat <- function(pollutant = "no2",
                   stat = c("mean", "min", "median", "max", "percentile", "limit"),
                   avg.time = "hour",
                   limit = NA,
                   percentile = NA,
                   roll = NA,
                   roll.avg.time = NA,
                   name = NULL) {
  stat <- match.arg(stat)
  
  out <-
    list(
      pollutant = pollutant,
      stat = stat,
      avg.time = avg.time,
      limit = limit,
      percentile = percentile,
      roll = roll,
      roll.avg.time = roll.avg.time,
      name = name
    )
  
  class(out) <- "aqStat"
  
  return(out)
}

#' Helper for calculating air quality statistics
#' @noRd
calculate_aqstat <- function(data, aqstat, period) {
  thedata <- dplyr::select(data, dplyr::all_of(c("date", aqstat$pollutant)))
  
  thedata <- openair::timeAverage(thedata, avg.time = aqstat$avg.time)
  
  if (!is.na(aqstat$roll)) {
    thedata <-
      openair::rollingMean(
        thedata,
        pollutant = aqstat$pollutant,
        width = aqstat$roll,
        new.name = aqstat$pollutant
      )
    
    if (!is.na(aqstat$roll.avg.time)) {
      thedata <-
        openair::timeAverage(
          thedata,
          pollutant = aqstat$pollutant, 
          avg.time = aqstat$roll.avg.time
        )
    }
  }
  
  if (aqstat$stat != "limit") {
    thedata <-
      openair::timeAverage(
        thedata,
        avg.time = period,
        statistic = aqstat$stat,
        percentile = aqstat$percentile
      )
  } else {
    thedata <-
      thedata %>%
      openair::cutData(period) %>%
      dplyr::mutate(date = min(date, na.rm = TRUE),
                    .by = dplyr::all_of(period)) %>%
      dplyr::summarise(calcstat = sum(.data[[aqstat$pollutant]] > aqstat$limit, na.rm = TRUE),
                       .by = "date")
  }
  
  if (is.null(aqstat$name)) {
    newname <- paste(aqstat$pollutant, aqstat$stat, aqstat$avg.time, sep = ".")
    if (aqstat$stat == "limit") {
      newname <- paste(newname, aqstat$limit, sep = ".")
    }
    if (aqstat$stat == "percentile") {
      newname <- paste(newname, aqstat$percentile, sep = ".")
    }
  } else {
    newname <- aqstat$name
  }
  
  names(thedata)[2] <- newname
  
  return(thedata)
}

#' Nice printing for aqStat objects
#' @author Jack Davison
#' @noRd
print.aqStat <- function(x) {
  cli::cli_h2("Air Quality Statistic")
  cli::cli_inform(c("i" = "Please use in {.fun calcAQStats}"))
  
  cli::cli_h3("Parameters")
  purrr::imap(x, ~ paste0("{.strong ", toupper(.y), "}: '", .x, "'")) %>%
    purrr::list_c() %>%
    cli::cli_inform()
}

