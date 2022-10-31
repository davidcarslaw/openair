
##' Import monitoring site meta data for the UK and European networks
##'
##' Function to import meta data for air quality monitoring sites
##'
##' This function imports site meta data from several networks in the UK and
##' Europe:
##'
##' \itemize{
##'
##' \item \dQuote{aurn}, The UK Automatic Urban and Rural Network.
##'
##' \item \dQuote{saqn}, The Scottish Air Quality Network.
##'
##' \item \dQuote{waqn}, The Welsh Air Quality Network.
##'
##' \item \dQuote{ni}, The Northern Ireland Air Quality Network.
##'
##' \item \dQuote{aqe}, The Air Quality England Network.
##'
##' \item \dQuote{local}, Locally managed air quality networks in England.
##'
##' \item \dQuote{kcl}, King's College London networks.
##'
##' \item \dQuote{europe}, Import hourly European data (Airbase/e-reporting)
##' based on a simplified version of the \code{saqgetr} package. }
##'
##' By default, the function will return the site latitude, longitude and site
##' type. If the option \code{all = TRUE} is used, much more detailed
##' information is returned. For most networks, this detailed information
##' includes per-pollutant summaries, opening and closing dates of sites etc.
##'
##' Thanks go to Trevor Davies (Ricardo), Dr Stuart Grange (EMPA) and Dr Ben
##' Barratt (KCL) and  for making these data available.
##' @param source The data source for the meta data. Can be \dQuote{aurn},
##'   \dQuote{saqn} (or \dQuote{saqd}), \dQuote{aqe}, \dQuote{waqn},
##'   \dQuote{ni}, \dQuote{local}, \dQuote{kcl} or \dQuote{europe}; upper or
##'   lower case.
##' @param all When \code{all = FALSE} only the site code, site name, latitude
##'   and longitude and site type are imported. Setting \code{all = TRUE} will
##'   import all available meta data and provide details (when available) or the
##'   individual pollutants measured at each site.
##' @return A data frame with meta data.
##' @author David Carslaw
##' @seealso \code{\link{importAURN}}, \code{\link{importSAQN}},
##'   \code{\link{importWAQN}}, \code{\link{importNI}}, \code{\link{importAQE}},
##'   \code{\link{importKCL}}, and  \code{\link{importEurope}}, for importing
##'   air quality data from each network.
##' @keywords methods
##' @export
##' @import readr
##' @importFrom rlang .data
##' @examples
##' ## basic data
##'
##' \dontrun{
##' meta <- importMeta(source = "aurn")
##'
##' # more detailed information:
##' meta <- importMeta(source = "aurn", all = TRUE)
##'
##' # from the Scottish Air Quality Network
##' meta <- importMeta(source = "saqn", all = TRUE)
##' }

importMeta <- function(source = "aurn", all = FALSE) {
  # fix CRAN checks (due to use of `load()`)
  AURN_metadata <- NULL
  metadata <- NULL

  ## meta data sources
  meta.source <- c("aurn", "kcl", "saqn", "saqd", "waqn", "aqe", "local", "lmam", "ni", "europe")

  ## ensure lower case
  source <- tolower(source)

  if (!source %in% meta.source) {
    stop("Meta data sources are 'aurn', 'saqn' 'waqn', 'aqe', 'kcl', 'local', 'europe'.")
  }

  # AURN
  if (source == "aurn") {
    tmp <- tempfile()

    fileName <-
      "https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData"
    download.file(fileName,
      method = "libcurl",
      destfile = tmp,
      quiet = TRUE
    )
    load(tmp)

    meta <- AURN_metadata
    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates

    ## rename to match imported names e.g. importAURN
    # ratified_to - parse fail means not ratified
    meta <- rename(
      meta,
      code = "site_id",
      site = "site_name",
      site_type = "location_type",
      variable = "parameter"
    ) %>%
      mutate(
        start_date = ymd(.data$start_date, tz = "GMT"),
        ratified_to = ymd(.data$ratified_to, tz = "GMT", quiet = TRUE)
      )

    ## unique ids
    if (!all) {
      meta <- distinct(meta, .data$site, .keep_all = TRUE)
    }
  }

  # SAQN/SAQD
  if (source %in% tolower(c("saqn", "saqd"))) {
    tmp <- tempfile()

    # load data
    load(
      url(
        "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData"
      )
    )

    meta <- rename(
      meta,
      code = "site_id",
      site = "site_name",
      site_type = "location_type",
      variable = "parameter"
    ) %>%
      mutate(
        start_date = ymd(.data$start_date, tz = "GMT"),
        ratified_to = ymd(.data$ratified_to, tz = "GMT", quiet = TRUE)
      )

    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) {
      meta <- distinct(meta, .data$site, .keep_all = TRUE)
    }
  }

  # NI
  if (source == "ni") {
    tmp <- tempfile()

    # load data
    load(url(
      "https://www.airqualityni.co.uk/openair/R_data/NI_metadata.RData"
    ))

    meta <- rename(
      metadata,
      code = "site_id",
      site = "site_name",
      site_type = "location_type",
      variable = "parameter"
    ) %>%
      mutate(
        start_date = ymd(.data$start_date, tz = "GMT"),
        ratified_to = ymd(.data$ratified_to, tz = "GMT", quiet = TRUE)
      )

    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) {
      meta <- distinct(meta, .data$site, .keep_all = TRUE)
    }
  }

  # WAQN
  if (source %in% tolower(c("waqn"))) {
    tmp <- tempfile()

    # load data
    load(
      url(
        "https://airquality.gov.wales/sites/default/files/openair/R_data/WAQ_metadata.RData"
      )
    )

    meta <- rename(
      metadata,
      code = "site_id",
      site = "site_name",
      site_type = "location_type",
      variable = "parameter"
    ) %>%
      mutate(
        start_date = ymd(.data$start_date, tz = "GMT"),
        ratified_to = ymd(.data$ratified_to, tz = "GMT", quiet = TRUE)
      )
    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) {
      meta <- distinct(meta, .data$site, .keep_all = TRUE)
    }
  }

  # AQE
  if (source %in% tolower(c("aqe"))) {
    tmp <- tempfile()

    # load data
    load(
      url(
        "https://airqualityengland.co.uk/assets/openair/R_data/AQE_metadata.RData"
      )
    )

    meta <- rename(
      metadata,
      code = "site_id",
      site = "site_name",
      site_type = "location_type",
      variable = "parameter"
    ) %>%
      mutate(
        start_date = ymd(.data$start_date, tz = "GMT"),
        ratified_to = ymd(.data$ratified_to, tz = "GMT", quiet = TRUE),
        site_type = gsub(
          pattern = "Urban traffic",
          replacement = "Urban Traffic", .data$site_type
        )
      )

    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) {
      meta <- distinct(meta, .data$site, .keep_all = TRUE)
    }
  }

  # LOCAL/LMAM
  if (source %in% tolower(c("lmam", "local"))) {
    tmp <- tempfile()

    # load data
    load(url(
      "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData"
    ))

    meta <- rename(
      metadata,
      code = "site_id",
      site = "site_name",
      site_type = "location_type",
      variable = "parameter"
    ) %>%
      mutate(
        start_date = ymd(.data$start_date, tz = "GMT"),
        site_type = gsub(
          pattern = "Urban traffic",
          replacement = "Urban Traffic", .data$site_type
        )
      )

    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) {
      meta <- distinct(meta, .data$site, .keep_all = TRUE)
    }
  }

  # KCL
  if (source == "kcl") {
    con <- url("https://www.londonair.org.uk/r_data/sites.RData")
    meta <- get(load(con))
    close(con)

    ## rename to match imported names e.g. importKCL
    meta <- rename(
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

  # drop extra columns if not "all"
  if (!all) {
    meta <-
      select(meta, all_of(c("site", "code", "latitude", "longitude", "site_type")))
  }

  # change some names
  if ("variable" %in% names(meta)) {
    id <- which(meta$variable == "NOXasNO2")

    if (length(id) > 0) {
      meta$variable[id] <- "NOx"
    }
  }

  as_tibble(meta)
}
