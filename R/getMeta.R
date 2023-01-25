#' Import monitoring site meta data for the UK and European networks
#'
#' Function to import meta data for air quality monitoring sites
#'
#' This function imports site meta data from several networks in the UK and
#' Europe:
#'
#' - `"aurn"`,  The UK Automatic Urban and Rural Network.
#'
#' - `"saqn"`,  The Scottish Air Quality Network.
#'
#' - `"waqn"`,  The Welsh Air Quality Network.
#'
#' - `"ni"`,  The Northern Ireland Air Quality Network.
#'
#' - `"aqe"`,  The Air Quality England Network.
#'
#' - `"local"`,  Locally managed air quality networks in England.
#'
#' - `"kcl"`,  King's College London networks.
#'
#' - `"europe"`,  Import hourly European data (Airbase/e-reporting) based on a
#' simplified version of the `saqgetr` package.
#'
#' By default, the function will return the site latitude, longitude and site
#' type. If the option `all = TRUE` is used, much more detailed information is
#' returned. For most networks, this detailed information includes per-pollutant
#' summaries, opening and closing dates of sites etc.
#'
#' Thanks go to Trevor Davies (Ricardo), Dr Stuart Grange (EMPA) and Dr Ben
#' Barratt (KCL) and  for making these data available.
#' @param source One or more sources of meta data. Can be `aurn`, `saqn` (or
#'   `saqd`), `aqe`, `waqn`, `ni`, `local` (or `lmam`), `kcl` or `europe`; upper
#'   or lower case.
#' @param all When `all = FALSE` only the site code, site name, latitude and
#'   longitude and site type are imported. Setting `all = TRUE` will import all
#'   available meta data and provide details (when available) or the individual
#'   pollutants measured at each site.
#' @param duplicate Some UK air quality sites are part of multiple networks, so
#'   could appear more than once when `source` is a vector of two or more. The
#'   default argument, `FALSE`, drops duplicate sites. `TRUE` will return them.
#' @param year If a single year is selected, only sites that were open at some
#'   point in that year are returned. If \code{all = TRUE} only sites that
#'   measured a particular pollutant in that year are returned. Year can also be
#'   a sequence e.g. \code{year = 2010:2020} or of length 2 e.g. \code{year =
#'   c(2010, 2020)}, which will return only sites that were open over the
#'   duration. Note that \code{year} is ignored when the \code{source} is either
#'   \code{"kcl"} or \code{"europe"}.
#' @return A data frame with meta data.
#' @author David Carslaw
#' @family import functions
#' @seealso the `networkMap()` function from the `openairmaps` package which can
#'   visualise site metadata
#' @export
#' @import readr
#' @examples
#' \dontrun{
#' # basic info
#' meta <- importMeta(source = "aurn")
#'
#' # more detailed information:
#' meta <- importMeta(source = "aurn", all = TRUE)
#'
#' # from the Scottish Air Quality Network
#' meta <- importMeta(source = "saqn", all = TRUE)
#'
#' # from multiple networks
#' meta <- importMeta(source = c("aurn", "aqe", "local"))
#' }

importMeta <- function(source = "aurn", all = FALSE, duplicate = FALSE, year = NA) {

  ## meta data sources
  meta.source <-
    c("aurn", "kcl", "saqn", "saqd", "waqn",
      "aqe", "local", "lmam", "ni", "europe")

  ## ensure lower case
  source <- tolower(source)

  if (any(!source %in% meta.source)) {
    stop("Meta data sources are 'aurn', 'saqn' 'waqn', 'aqe', 'kcl', 'local', 'europe'.")
  }

  # function to import any of the source networks
  get_meta <- function(source, all){
    if (!source %in% c("kcl", "europe")) {
      url <- switch(
        source,
        aurn = "https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData",
        saqn = "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData",
        saqd = "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData",
        ni = "https://www.airqualityni.co.uk/openair/R_data/NI_metadata.RData",
        waqn = "https://airquality.gov.wales/sites/default/files/openair/R_data/WAQ_metadata.RData",
        aqe = "https://airqualityengland.co.uk/assets/openair/R_data/AQE_metadata.RData",
        local = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData",
        lmam = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData"
      )

      meta <- clean_ricardo_meta(url, all = all, year = year)
    }

    # KCL
    if (source == "kcl") {
      con <- url("https://www.londonair.org.uk/r_data/sites.RData")
      meta <- get(load(con))
      close(con)

      ## rename to match imported names e.g. importKCL
      meta <- dplyr::rename(
        meta,
        code = "SiteCode",
        site = "SiteName",
        site_type = "Classification",
        latitude = "Latitude",
        longitude = "Longitude"
      )
    }

    # EUROPE
    if (source == "europe") {
      file <-
        "http://aq-data.ricardo-aea.com/R_data/saqgetr/helper_tables/sites_table.csv.gz"

      # Define data types
      col_types <- cols(
        site = col_character(),
        site_name = col_character(),
        latitude = col_double(),
        longitude = col_double(),
        elevation = col_double(),
        country = col_character(),
        country_iso_code = col_character(),
        site_type = col_character(),
        site_area = col_character(),
        date_start = col_character(),
        date_end = col_character(),
        network = col_character(),
        eu_code = col_character(),
        eoi_code = col_character(),
        data_source = col_character()
      )

      # Read data and parse dates
      meta <-
        read_csv(file, col_types = col_types, progress = FALSE) %>%
        mutate(
          date_start = lubridate::ymd_hms(.data$date_start, tz = "UTC"),
          date_end = lubridate::ymd_hms(.data$date_end, tz = "UTC")
        )

      meta <- rename(meta, code = "site", site = "site_name")
    }

    # return data source
    meta$source <- source

    return(meta)
  }

  # import meta data
  meta <-
    purrr::map(.x = source, .f = ~ get_meta(source = .x, all = all)) %>%
    purrr::list_rbind()

  # drop extra columns if not "all"
  if (!all) {
    meta <-
      dplyr::select(meta, dplyr::all_of(
        c("site", "code", "latitude", "longitude", "site_type", "source")
      ))
  }

  # change some names
  if ("variable" %in% names(meta)) {
    id <- which(meta$variable == "NOXasNO2")

    if (length(id) > 0) {
      meta$variable[id] <- "NOx"
    }
  }

  # deal with duplicates
  if (!duplicate) {
    if (all & "variable" %in% names(meta)) {
      meta <- dplyr::distinct(meta, .data$code, .data$site, .data$latitude,
        .data$longitude, .data$variable,
        .keep_all = TRUE
      )
    } else {
      meta <- dplyr::distinct(meta, .data$code, .data$site, .data$latitude,
        .data$longitude,
        .keep_all = TRUE
      )
    }
  }

  as_tibble(meta)
}

#' Clean data from Ricardo (not KCL or Europe)
#' @param url URL to use
#' @param all inherited from parent function
#' @param year inherited from parent function
#' @noRd
clean_ricardo_meta <- function(url, all, year) {
  tmp <- tempfile()

  # load data
  con <- url(url)
  meta <- get(load(con))
  close(con)

  # manipulate data
  meta <- dplyr::rename(
    .data = meta,
    code = "site_id",
    site = "site_name",
    site_type = "location_type",
    variable = "parameter"
  ) %>%
    dplyr::mutate(
      start_date = lubridate::ymd(.data$start_date, tz = "GMT"),
      site_type = gsub(
        pattern = "Urban traffic",
        replacement = "Urban Traffic",
        .data$site_type
      )
    )

  # sort out ratified to (not needed in LMAM data)
  if ("ratified_to" %in% names(meta)) {
    meta$ratified_to <-
      lubridate::ymd(meta$ratified_to, tz = "GMT", quiet = TRUE)
  }
  
  # select year or period when sites were open
  if (!anyNA(year)) {
    # format end_date - set "ongoing" to current date
    meta$end_date[which(meta$end_date == "ongoing")] <- as.character(Sys.Date())
    meta$end_year <- lubridate::year(as.Date(meta$end_date))
    meta$start_year <- lubridate::year(meta$start_date)
    meta <- dplyr::filter(meta, start_year <= min(year) & end_year >= max(year))
  }

  ## only extract one line per site to make it easier to use file
  ## mostly interested in coordinates
  if (!all) {
    meta <- dplyr::distinct(meta, .data$site, .keep_all = TRUE)
  }

  return(meta)
}
