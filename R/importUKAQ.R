#' Import data from the UK Air Pollution Networks
#'
#' Functions for importing air pollution data from a range of UK networks
#' including the Automatic Urban and Rural Network (AURN) and individual England
#' (AQE), Scotland (SAQN), Wales (WAQN) and Northern Ireland (NI) Networks.
#' Files are imported from a remote server operated by Ricardo that provides air
#' quality data files as R data objects.
#'
#' @section Importing UK Air Pollution Data:
#'
#'   This family of functions has been written to make it easy to import data
#'   from across several UK air quality networks. Ricardo have provided .RData
#'   files (R workspaces) of all individual sites and years, as well as up to
#'   date meta data. These files are updated on a daily basis. This approach
#'   requires a link to the Internet to work.
#'
#'   For an up to date list of available sites that can be imported, see
#'   [importMeta()].
#'
#'   The site codes and pollutant names can be upper or lower case.
#'
#'   There are several advantages over the web portal approach where .csv files
#'   are downloaded. First, it is quick to select a range of sites, pollutants
#'   and periods (see examples below). Second, storing the data as .RData
#'   objects is very efficient as they are about four times smaller than .csv
#'   files --- which means the data downloads quickly and saves bandwidth.
#'   Third, the function completely avoids any need for data manipulation or
#'   setting time formats, time zones etc. The function also has the advantage
#'   that the proper site name is imported and used in \code{openair} functions.
#'
#'   The data are imported by stacking sites on top of one another and will have
#'   field names \code{site}, \code{code} (the site code) and \code{pollutant}.
#'
#'   By default, the function returns hourly average data. However, annual,
#'   monthly, daily and 15 minute data (for SO2) can be returned using the
#'   option \code{data_type}. Annual and monthly data provide whole network
#'   information including data capture statistics.
#'
#'   All units are expressed in mass terms for gaseous species (ug/m3 for NO,
#'   NO2, NOx (as NO2), SO2 and hydrocarbons; and mg/m3 for CO). PM10
#'   concentrations are provided in gravimetric units of ug/m3 or scaled to be
#'   comparable with these units. Over the years a variety of instruments have
#'   been used to measure particulate matter and the technical issues of
#'   measuring PM10 are complex. In recent years the measurements rely on FDMS
#'   (Filter Dynamics Measurement System), which is able to measure the volatile
#'   component of PM. In cases where the FDMS system is in use there will be a
#'   separate volatile component recorded as 'v10' and non-volatile component
#'   'nv10', which is already included in the absolute PM10 measurement. Prior
#'   to the use of FDMS the measurements used TEOM (Tapered Element Oscillating.
#'   Microbalance) and these concentrations have been multiplied by 1.3 to
#'   provide an estimate of the total mass including the volatile fraction.
#'
#' Some sites report hourly and daily PM10 and / or PM2.5. When \code{data_type
#' = "daily"} and there are both hourly and 'proper' daily measurements
#'   available, these will be returned as e.g. "pm2.5" and "gr_pm2.5"; the
#'   former corresponding to data based on original hourly measurements and the
#'   latter corresponding to daily gravimetric measurements.
#'
#'   The function returns modelled hourly values of wind speed (\code{ws}), wind
#'   direction (\code{wd}) and ambient temperature (\code{air_temp}) if
#'   available (generally from around 2010). These values are modelled using the
#'   WRF model operated by Ricardo.
#'
#'   The BAM (Beta-Attenuation Monitor) instruments that have been incorporated
#'   into the network throughout its history have been scaled by 1.3 if they
#'   have a heated inlet (to account for loss of volatile particles) and 0.83 if
#'   they do not have a heated inlet. The few TEOM instruments in the network
#'   after 2008 have been scaled using VCM (Volatile Correction Model) values to
#'   account for the loss of volatile particles. The object of all these scaling
#'   processes is to provide a reasonable degree of comparison between data sets
#'   and with the reference method and to produce a consistent data record over
#'   the operational period of the network, however there may be some
#'   discontinuity in the time series associated with instrument changes.
#'
#'   No corrections have been made to the PM2.5 data. The volatile component of
#'   FDMS PM2.5 (where available) is shown in the 'v2.5' column.
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
#' @return a [tibble][tibble::tibble-package]
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
#' @rdname import_ukaq
#' @order 1
importAURN <-
  function(site = "my1",
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
    url_slug = "https://uk-air.defra.gov.uk/openair/R_data/"
    url_abbr = "_AURN_"
    source = "aurn"
    missing_site <- missing(site)

    import_network_worker(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      source = source,
      url_slug = url_slug,
      url_abbr = url_abbr,
      missing_site = missing_site
    )
  }

#' @rdname import_ukaq
#' @order 2
#' @export
importAQE <-
  function(site = "yk13",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    url_slug = "https://airqualityengland.co.uk/assets/openair/R_data/"
    url_abbr = "_AQE_"
    source = "aqe"
    missing_site <- missing(site)

    import_network_worker(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      source = source,
      url_slug = url_slug,
      url_abbr = url_abbr,
      missing_site = missing_site
    )
  }

#' @rdname import_ukaq
#' @order 3
#' @export
importSAQN <-
  function(site = "gla4",
           year = 2009,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    url_slug = "https://www.scottishairquality.scot/openair/R_data/"
    url_abbr = "_SCOT_"
    source = "saqn"
    missing_site <- missing(site)

    import_network_worker(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      source = source,
      url_slug = url_slug,
      url_abbr = url_abbr,
      missing_site = missing_site
    )
  }

#' @rdname import_ukaq
#' @order 4
#' @export
importWAQN <-
  function(site = "card",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    url_slug = "https://airquality.gov.wales/sites/default/files/openair/R_data/"
    url_abbr = "_WAQ_"
    source = "waqn"
    missing_site <- missing(site)

    import_network_worker(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      source = source,
      url_slug = url_slug,
      url_abbr = url_abbr,
      missing_site = missing_site
    )
  }

#' @rdname import_ukaq
#' @order 5
#' @export
importNI <-
  function(site = "bel0",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    url_slug = "https://www.airqualityni.co.uk/openair/R_data/"
    url_abbr = "_NI_"
    source = "ni"
    missing_site <- missing(site)

    import_network_worker(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      source = source,
      url_slug = url_slug,
      url_abbr = url_abbr,
      missing_site = missing_site
    )
  }

#' Helper to import AURN/SAQN/WAQN/NIAQN/AQE data
#' @noRd
import_network_worker <-
  function(site,
           year,
           data_type,
           pollutant,
           meta,
           meteo,
           ratified,
           to_narrow,
           progress,
           source,
           url_slug,
           url_abbr,
           missing_site) {
    allowed_types <- c(
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
    )

    if (!tolower(data_type) %in% allowed_types) {
      cli::cli_warn(
        c("!" = "'{data_type}' not recognised. Setting {.arg data_type} to 'hourly'",
          "i" = "{.arg data_type} should be one of: {allowed_types}")
      )

      data_type <- "hourly"
    }

    if (data_type %in% c("annual", "monthly")) {
      files <- paste0(url_slug, "summary_", data_type, url_abbr, year, ".rds")

      if (progress)
        progress <- "Importing Statistics"
      aq_data <- purrr::map(
        files,
        readSummaryData,
        data_type = data_type,
        to_narrow = to_narrow,
        hc = FALSE,
        .progress = progress
      ) %>%
        purrr::list_rbind()

      # filtering
      aq_data <-
        filter_annual_stats(
          aq_data,
          missing_site = missing_site,
          site = site,
          pollutant = pollutant,
          to_narrow = to_narrow
        )

      # add meta data?
      if (meta) {
        aq_data <- add_meta(source = source, aq_data)
      }
    } else if (data_type == "daqi") {
      # daily air quality index
      files <-
        paste0(url_slug, "annual_DAQI", url_abbr, year, ".rds")

      if (progress)
        progress <- "Importing DAQI"
      aq_data <-
        purrr::map(files, readDAQI, .progress = progress) %>%
        purrr::list_rbind()

      # filtering
      aq_data <-
        filter_annual_stats(
          aq_data,
          missing_site = missing_site,
          site = site,
          pollutant = pollutant,
          to_narrow = to_narrow
        )

      # add meta if needed
      if (meta) {
        aq_data <- add_meta(source = "aurn", aq_data)
      }
    } else {
      aq_data <- importUKAQ(
        site = site,
        year = year,
        data_type,
        pollutant = pollutant,
        meta = meta,
        ratified = ratified,
        to_narrow = to_narrow,
        source = source,
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
