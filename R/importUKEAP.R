#'
#' Import Data from the UK Eutrophying & Acidifying Network
#'
#' Functions for importing air pollution data from the [UK Eutrophying &
#' Acidifying
#' Network](https://uk-air.defra.gov.uk/networks/network-info?view=ukeap). Files
#' are imported from a remote server operated by Ricardo that provides air
#' quality data files as R data objects. For an up to date list of available
#' sites that can be imported, see [importMeta()]. Currently, only the NAMN
#' network is available, but it is intended that more data will be made
#' available in the future.
#'
#' @inheritParams importUKAQ
#' @param source The network(s) from which to import site data. Providing a
#'   single network will attempt to import all of the given sites from the
#'   provided network. Available networks include:
#'   - `"namn"`, the UK National Ammonia Network.
#' @param data_type The type of data to be returned. There are two options:
#' - `"multiday"`: Multi-day measurements, provided with separate "date" (start) and "date_end" (end) columns.
#' - `"annual"`: Annual average data with data capture information for the whole network.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @author Jack Davison, Trevor Davies, and David Carslaw
#'
#' @examples
#' \dontrun{
#' importNAMN(
#'   2020:2022,
#'   pollutant = c("delta_nh3_g", "a_nh4_p"),
#'   meta = T,
#'   to_narrow = F
#' )
#' }
importUKEAP <-
  function(year = 2018,
           source = "namn",
           data_type = "annual",
           pollutant = "all",
           meta = TRUE,
           to_narrow = FALSE,
           progress = TRUE) {
    import_namn_helper <- function(x) {
      readr::read_rds(
        paste0(
          "http://uk-air.defra.gov.uk/openair/R_data/summary_annual_NAMN_",
          x,
          ".rds"
        )
      )
    }
    namn <-
      purrr::map(year,
                 import_namn_helper,
                 .progress = ifelse(progress, "Importing AQ Data", FALSE)) %>%
      purrr::list_rbind() %>%
      dplyr::tibble()
    
    namn <- namn %>%
      select(-contains("unit")) %>%
      mutate(
        date = lubridate::ymd_hms(paste0(Year, "-01-01 00:00:00")),
        .after = site,
        .keep = "unused"
      ) %>%
      mutate(site = as.character(site)) %>%
      rename(uka_code = UKA_code)
    
    if (any(pollutant != "all")) {
      namn <-
        select(namn, "uka_code", "site", "date", contains(pollutant))
    }
    
    if (to_narrow) {
      namn <-
        tidyr::pivot_longer(namn,-c("uka_code", "site", "date")) %>%
        mutate(name = gsub("\\.", "$$", name)) %>%
        tidyr::separate_wider_delim(name,
                                    delim = "$$",
                                    names = c("pollutant", "name")) %>%
        tidyr::pivot_wider(names_from = "name", values_from = "value")
    } else {
      namn <-
        rename_with(namn, ~ gsub(".mean", "", tolower(.x)))
    }
    
    if (meta) {
      meta <-
        readr::read_rds("http://uk-air.defra.gov.uk/openair/R_data/NAMN_metadata.rds") %>%
        dplyr::tibble()
      
      namn <-
        dplyr::left_join(
          namn,
          distinct(
            importNAMNmeta(),
            uka_code = UKA_code,
            latitude,
            longitude,
            site_type = location_type
          ),
          by = "uka_code"
        )
    }
    
    return(namn)
  }
