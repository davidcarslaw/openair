#' Import data from the UK Air Pollution Networks
#'
#' Functions for importing air pollution data from a range of UK networks
#' including the Automatic Urban and Rural Network. Files are imported from a
#' remote server operated by Ricardo that provides air quality data files as R
#' data objects.
#'
#' This family of functions has been written to make it easy to import data from
#' across several UK air quality networks. Ricardo have provided .RData files (R
#' workspaces) of all individual sites and years, as well as up to date meta
#' data. These files are updated on a daily basis. This approach requires a link
#' to the Internet to work.
#'
#' For an up to date list of available sites that can be imported, see
#' [importMeta()].
#'
#' The site codes and pollutant names can be upper or lower case.
#'
#' There are several advantages over the web portal approach where .csv files
#' are downloaded. First, it is quick to select a range of sites, pollutants and
#' periods (see examples below). Second, storing the data as .RData objects is
#' very efficient as they are about four times smaller than .csv files --- which
#' means the data downloads quickly and saves bandwidth. Third, the function
#' completely avoids any need for data manipulation or setting time formats,
#' time zones etc. The function also has the advantage that the proper site name
#' is imported and used in \code{openair} functions.
#'
#' The data are imported by stacking sites on top of one another and will have
#' field names \code{site}, \code{code} (the site code) and \code{pollutant}.
#'
#' By default, the function returns hourly average data. However, annual,
#' monthly, daily and 15 minute data (for SO2) can be returned using the option
#' \code{data_type}. Annual and monthly data provide whole network information
#' including data capture statistics.
#'
#' All units are expressed in mass terms for gaseous species (ug/m3 for NO, NO2,
#' NOx (as NO2), SO2 and hydrocarbons; and mg/m3 for CO). PM10 concentrations
#' are provided in gravimetric units of ug/m3 or scaled to be comparable with
#' these units. Over the years a variety of instruments have been used to
#' measure particulate matter and the technical issues of measuring PM10 are
#' complex. In recent years the measurements rely on FDMS (Filter Dynamics
#' Measurement System), which is able to measure the volatile component of PM.
#' In cases where the FDMS system is in use there will be a separate volatile
#' component recorded as 'v10' and non-volatile component 'nv10', which is
#' already included in the absolute PM10 measurement. Prior to the use of FDMS
#' the measurements used TEOM (Tapered Element Oscillating. Microbalance) and
#' these concentrations have been multiplied by 1.3 to provide an estimate of
#' the total mass including the volatile fraction.
#'
#' Some sites report hourly and daily PM10 and / or PM2.5. When \code{data_type
#' = "daily"} and there are both hourly and 'proper' daily measurements
#' available, these will be returned as e.g. "pm2.5" and "gr_pm2.5"; the former
#' corresponding to data based on original hourly measurements and the latter
#' corresponding to daily gravimetric measurements.
#'
#' The function returns modelled hourly values of wind speed (\code{ws}), wind
#' direction (\code{wd}) and ambient temperature (\code{air_temp}) if available
#' (generally from around 2010). These values are modelled using the WRF model
#' operated by Ricardo.
#'
#' The BAM (Beta-Attenuation Monitor) instruments that have been incorporated
#' into the network throughout its history have been scaled by 1.3 if they have
#' a heated inlet (to account for loss of volatile particles) and 0.83 if they
#' do not have a heated inlet. The few TEOM instruments in the network after
#' 2008 have been scaled using VCM (Volatile Correction Model) values to account
#' for the loss of volatile particles. The object of all these scaling processes
#' is to provide a reasonable degree of comparison between data sets and with
#' the reference method and to produce a consistent data record over the
#' operational period of the network, however there may be some discontinuity in
#' the time series associated with instrument changes.
#'
#' No corrections have been made to the PM2.5 data. The volatile component of
#' FDMS PM2.5 (where available) is shown in the 'v2.5' column.
#'
#'
#' @param site Site code of the site to import e.g. \dQuote{my1} is Marylebone
#'   Road. Several sites can be imported with \code{site = c("my1", "nott")}
#'   --- to import Marylebone Road and Nottingham for example.
#' @param year Year or years to import. To import a sequence of years from 1990
#'   to 2000 use \code{year = 1990:2000}. To import several specific years use
#'   \code{year = c(1990, 1995, 2000)} for example.
#' @param data_type The data type averaging period. These include:
#'
#'   \itemize{
#'   \item{"hourly"}{ Default is to return hourly data.}
#'   \item{"daily"}{ Daily average data.}
#'   \item{"monthly"}{ Monthly average
#'   data with data capture information for the whole network.}
#'   \item{"annual"}{ Annual average data with data capture information for the
#'   whole network.}
#'   \item{"15_min"}{ To import 15-minute average SO2
#'   concentrations.}
#'   \item{"8_hour"}{ To import 8-hour rolling mean
#'   concentrations for O3 and CO.}
#'   \item{"24_hour"}{ To import 24-hour rolling
#'   mean concentrations for particulates.}
#'   \item{"daily_max_8"}{ To import maximum daily rolling 8-hour maximum for O3 and CO.}
#'   \item{"daqi"}{ To import Daily
#'   Air Quality Index (DAQI). See
#'   \href{https://uk-air.defra.gov.uk/air-pollution/daqi?view=more-info&pollutant=ozone#pollutant}{here}
#'    for more details of how the index is defined.} }
#' @param pollutant Pollutants to import. If omitted will import all pollutants
#'   from a site. To import only NOx and NO2 for example use \code{pollutant =
#'   c("nox", "no2")}.
#' @param hc A few sites have hydrocarbon measurements available and setting
#'   \code{hc = TRUE} will ensure hydrocarbon data are imported. The default is
#'   however not to as most users will not be interested in using hydrocarbon
#'   data and the resulting data frames are considerably larger.
#' @param meta Should meta data be returned? If \code{TRUE} the site type,
#'   latitude and longitude are returned.
#' @param meteo Should modelled meteorological data be returned if available.
#'   The default is \code{TRUE} and will return wind speed (\code{ws}), wind
#'   direction (\code{wd}) and ambient temperature (\code{air_temp}). The
#'   variables are calculated from using the WRF model run by Ricardo Energy &
#'   Environment and are available for most but not all networks. Setting
#'   \code{meteo = FALSE} is useful if you have other meteorological data to use
#'   in preference e.g. from \code{\link[worldmet]{worldmet}}.
#' @param ratified If \code{TRUE} columns are returned indicating when each
#'   species was ratified i.e. quality-checked. Available for hourly data only.
#' @param to_narrow By default the returned data has a column for each
#'   pollutant/variable. When \code{to_narrow = TRUE} the data are stacked into
#'   a narrow format with a column identifying the pollutant name.
#' @param verbose Should the function give messages when downloading files?
#'   Default is \code{FALSE}.
#' @param progress Show a progress bar when many sites/years are being imported?
#'   Defaults to `TRUE`.
#'
#' @export
#' @return Returns a data frame of hourly mean values with date in POSIXct class
#'   and time zone GMT.
#' @author David Carslaw and Trevor Davies
#' @family import functions
#' @examples
#'
#' ## import all pollutants from Marylebone Rd from 1990:2009
#' \dontrun{mary <- importAURN(site = "my1", year = 2000:2009)}
#'
#' ## import nox, no2, o3 from Marylebone Road and Nottingham Centre for 2000
#' \dontrun{thedata <- importAURN(site = c("my1", "nott"), year = 2000,
#' pollutant = c("nox", "no2", "o3"))}
#'
#' # Other functions work in the same way e.g. to import Cardiff Centre data
#'
#' # Import annual data over a period, make it narrow format and return site information
#'
#' \dontrun{aq <- importAURN(year = 2010:2020, data_type = "annual", meta = TRUE, to_narrow = TRUE)}
#'
#' \dontrun{cardiff <- importWAQN(site = "card", year = 2020)}
importAURN <- function(site = "my1",
                       year = 2009,
                       data_type = "hourly",
                       pollutant = "all",
                       hc = FALSE,
                       meta = FALSE,
                       meteo = TRUE,
                       ratified = FALSE,
                       to_narrow = FALSE,
                       verbose = FALSE,
                       progress = TRUE) {
  if (!tolower(data_type) %in%
      c(
        "hourly",
        "daily",
        "15min",
        "monthly",
        "annual",
        "daqi",
        "15_min",
        "24_hour",
        "8_hour",
        "daily_max_8"
      )) {
    warning(
      "data_type should be one of 'hourly', 'daily',
            'monthly', 'annual', '15_min', '24_hour', '8_hour',
            'daily_max_8', 'daqi'"
    )

    data_type <- "hourly"
  }


  if (data_type %in% c("annual", "monthly")) {
    files <- paste0(
      "https://uk-air.defra.gov.uk/openair/R_data/summary_",
      data_type,
      "_AURN_",
      year,
      ".rds"
    )

    if (progress)
      progress <- "Importing Statistics"
    aq_data <- purrr::map(
      files,
      readSummaryData,
      data_type = data_type,
      to_narrow = to_narrow,
      meta = meta,
      hc = hc,
      .progress = progress
    ) %>%
      purrr::list_rbind()

    # add meta data?
    if (meta) {
      aq_data <- add_meta(source = "aurn", aq_data)
    }
  } else if (data_type == "daqi") {
    # daily air quality index
    files <- paste0("https://uk-air.defra.gov.uk/openair/R_data/annual_DAQI_AURN_",
                    year,
                    ".rds")

    if (progress)
      progress <- "Importing DAQI"
    aq_data <-
      purrr::map(files, readDAQI, .progress = progress) %>%
      purrr::list_rbind()

    if (meta) {
      aq_data <- add_meta(source = "aurn", aq_data)
    }
  } else {
    aq_data <- importUKAQ(
      site,
      year,
      data_type,
      pollutant,
      hc,
      meta,
      ratified,
      to_narrow,
      verbose,
      source = "aurn",
      progress = progress
    )
  }
  
  # check to see if met data needed
  if (meteo == FALSE) {
    
  aq_data <- aq_data %>% 
    select(-any_of(c("ws", "wd", "air_temp")))
  
  }

  return(as_tibble(aq_data))
}

# function to read annual or monthly files

readSummaryData <-
  function(fileName, data_type, to_narrow, meta, hc) {
    thedata <- try(readRDS(url(fileName)), TRUE)

    if (inherits(thedata, "try-error")) {
      return()
    }

    names(thedata)[names(thedata) == "NOXasNO2.mean"] <- "nox"
    names(thedata)[names(thedata) == "NOXasNO2.capture"] <-
      "nox_capture"
    names(thedata) <- tolower(names(thedata))
    names(thedata) <- gsub(".mean", "", names(thedata))
    names(thedata) <- gsub(".capture", "_capture", names(thedata))

    if (!hc) {
      thedata <- thedata %>%
        select(any_of(
          c(
            "date",
            "uka_code",
            "code",
            "site",
            "year",
            "o3",
            "o3_capture",
            "o3.summer_capture",
            "o3.daily.max.8hour",
            "o3.aot40v",
            "o3.aot40f",
            "somo35",
            "somo35_capture",
            "no",
            "no_capture",
            "no2",
            "no2_capture",
            "nox",
            "nox_capture",
            "so2",
            "so2_capture",
            "co",
            "co_capture",
            "pm10",
            "pm10_capture",
            "nv10",
            "nv10_capture",
            "v10",
            "v10_capture",
            "pm2.5",
            "pm2.5_capture",
            "nv2.5",
            "nv2.5_capture",
            "v2.5",
            "v2.5_capture",
            "gr10",
            "gr10_capture",
            "gr2.5",
            "gr2.5_capture",
            "gr_pm2.5",
            "gr_pm10"
          )
        ))
    }


    if (data_type == "monthly") {
      thedata <- mutate(thedata, date = ymd(date, tz = "UTC"))
    }

    if (data_type == "annual") {
      thedata <- rename(thedata, date = year) %>%
        drop_na(date) %>%
        mutate(date = ymd(paste0(date, "-01-01"), tz = "UTC"))
    }

    if (to_narrow) {
      # make sure numbers are numbers
      values <- select(thedata,!contains("capture")) %>%
        select(!matches("uka_code"))

      capture <-
        select(thedata, contains("capture") | c(code, date, site)) %>%
        select(!matches("uka_code"))

      values <- pivot_longer(values,
                             -c(date, code, site),
                             values_to = "value",
                             names_to = "species")

      capture <- pivot_longer(capture,
                              -c(date, code, site),
                              values_to = "data_capture",
                              names_to = "species")

      capture$species <- gsub("_capture", "", capture$species)

      thedata <- full_join(values, capture,
                           by = c("date", "code", "site", "species"))
    }

    thedata <- thedata %>%
      mutate(site = as.character(site),
             code = as.character(code))


    return(thedata)
  }

readDAQI <- function(fileName) {
  thedata <- try(readRDS(url(fileName)), TRUE)

  if (inherits(thedata, "try-error")) {
    return()
  }

  thedata <- thedata %>%
    mutate(
      code = as.character(code),
      site = as.character(site),
      pollutant = as.character(pollutant),
      date = ymd(Date, tz = "GMT"),
      measurement_period = as.character(measurement_period)
    ) %>%
    select(-Date) %>%
    relocate(date, .after = pollutant)

  return(thedata)
}

# function to add meta data based on network and supply of aq data
add_meta <- function(source, aq_data) {
  meta_data <- importMeta(source = source)

  meta_data <- distinct(meta_data, site, .keep_all = TRUE) %>%
    select(site, code, latitude, longitude, site_type)

  aq_data <- left_join(aq_data, meta_data, by = c("code", "site"))

  return(aq_data)
}
