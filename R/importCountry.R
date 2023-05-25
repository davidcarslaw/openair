#' Import data from the English, Scottish, Welsh and Northern Ireland Air
#' Quality Networks
#'
#' Functions for importing air pollution data from the "devolved" UK networks;
#' Air Quality England (AQE), Scotland (SAQN), Wales (WAQN) and Northern Ireland
#' (NI). Files are imported from a remote server operated by Ricardo that
#' provides air quality data files as R data objects.
#'
#' @inheritSection importAURN Importing UK Air Pollution Data
#'
#' @inheritParams importAURN
#' @param data_type The data type averaging period. These include:
#'
#'   \itemize{
#'   \item{"hourly"}{ Default is to return hourly data.}
#'   \item{"daily"}{ Daily average data.}
#'   \item{"monthly"}{ Monthly average
#'   data with data capture information for the whole network.}
#'   \item{"annual"}{ Annual average data with data capture information for the
#'   whole network.}
#'   \item{"15_min"}{To import 15-minute average SO2
#'   concentrations.}
#'   \item{"8_hour"}{ To import 8-hour rolling mean
#'   concentrations for O3 and CO.}
#'   \item{"24_hour"}{ To import 24-hour rolling
#'   mean concentrations for particulates.}
#'   \item{"daily_max_8"}{ To import maximum daily rolling 8-hour maximum for O3 and CO.}
#'   }
#' @family import functions
#' @rdname import_country
#' @order 1
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
    url_slug = "https://airqualityengland.co.uk/assets/openair/R_data/summary_"
    url_abbr = "_AQE_"
    source = "aqe"
    missing_site <- missing(site)
    
    import_devolved_network(
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

#' @rdname import_country
#' @order 2
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
    url_slug = "https://www.scottishairquality.scot/openair/R_data/summary_"
    url_abbr = "_SCOT_"
    source = "saqn"
    missing_site <- missing(site)
    
    import_devolved_network(
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

#' @rdname import_country
#' @order 3
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
    url_slug = "https://airquality.gov.wales/sites/default/files/openair/R_data/summary_"
    url_abbr = "_WAQ_"
    source = "waqn"
    missing_site <- missing(site)
    
    import_devolved_network(
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

#' @rdname import_country
#' @order 4
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
    url_slug = "https://www.airqualityni.co.uk/openair/R_data/summary_"
    url_abbr = "_NI_"
    source = "ni"
    missing_site <- missing(site)
    
    import_devolved_network(
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

#' Helper to import the SAQN/WAQN/NIAQN/AQE data
#' @noRd
import_devolved_network <-
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
    if (data_type %in% c("annual", "monthly")) {
      files <- paste0(url_slug, data_type, url_abbr, year, ".rds")
      
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
