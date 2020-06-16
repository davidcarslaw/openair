
##' Import monitoring site meta data for the UK and European networks
##'
##' Function to import meta data for air quality monitoring sites
##'
##' This function imports site meta data from four networks in the UK: the Defra
##' Automatic Urban and Rural Network (AURN), King's College London networks,
##' the Scottish Air Quality Network, Welsh Air Quality Network and Air Quality
##' England sites. Additionally, meta data are available for Airbase/e-reporting
##' sites across Europe based on the \code{saqgetr} package. The meta data
##' includes site location (latitude, longitude and OS easting and northing ---
##' the latter for KCL networks), site type and it's start/close data, as well
##' as other information.
##'
##' The Scottish and Welsh air quality networks are available as SAQN (or SAQD)
##' and WAQN.
##'
##' The meta information can usefully be combined with matching air pollution
##' data and produce maps of concentration --- see examples below.
##'
##' Thanks go to Trevor Davies (Ricardo), Dr Stuart Grange (EMPA) and Dr Ben
##' Barratt (KCL) and  for making these data available.
##' @param source The data source for the meta data. Can be \dQuote{aurn},
##'   \dQuote{kcl}, \dQuote{saqn} (or \dQuote{aqd}), \dQuote{aqe} or
##'   \dQuote{europe}; upper or lower case.
##' @param all When \code{all = FALSE} only the site code, site name, latitude
##'   and longitude and site type are imported. Setting \code{all = TRUE} will
##'   import all available meta data and provide details (when available) or the
##'   individual pollutants measured at each site.
##' @return A data frame with meta data.
##' @author David Carslaw
##' @seealso \code{\link{importAURN}}, \code{\link{importKCL}} and
##'   \code{\link{importSAQN}} for importing air quality data from each network.
##' @keywords methods
##' @import readr
##' @export
##' @examples
##' ## import AQ data and add meta data to data frame
##' \dontrun{
##' aq <- importAURN(site = c("kc1", "my1"), year = 2009)
##' meta <- importMeta(source = "aurn")
##' aq <- merge(aq, meta, by = "site")
##' }
##'
##' ## aggregate first before adding meta data (useful for many sites/years)
##' \dontrun{
##' aq <- importAURN(site = c("kc1", "my1"), year = 2009)
##' meta <- importMeta(source = "aurn")
##' ## calculate annual means
##' annual <- timeAverage(aq, avg.time = "year", type = "site")
##' annual <- merge(annual, meta, by = "site")
##' }

importMeta <- function(source = "aurn", all = FALSE) {
  
  # keep R check quiet
  AURN_metadata <- NULL
  SCOT_metadata <- NULL
  
  ## get rid of R check annoyances
  site <- code <- latitude <- longitude <- site.type <- site_name <- site_id <- NULL
  location_type <- SiteCode <- SiteName <- Classification <- Latitude <- Longitude <- NULL
  metadata <- site_type <- date_ended <- network_id <- NULL
  
  ## meta data sources
  meta.source <- c("aurn", "kcl", "saqn", "saqd", "waqn", "aqe", "europe")
  
  ## ensure lower case
  source <- tolower(source)
  
  if (!source %in% meta.source) 
    stop("Meta data sources are 'aurn', 'kcl', 'saqn' 'waqn', 'aqe', 'europe'.")
  
  if (source == "aurn") {
    tmp <- tempfile()
    
    fileName <- "http://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData"
    download.file(fileName, method = "libcurl", destfile = tmp, quiet = TRUE)
    load(tmp)
    
    meta <- AURN_metadata
    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    
    ## rename to match imported names e.g. importAURN
    # ratified_to - parse fail means not ratified
    meta <- rename(meta,
                   code = site_id, site = site_name,
                   site_type = location_type, variable = parameter) %>% 
      mutate(start_date = ymd(start_date, tz = "GMT"),
             ratified_to = ymd(ratified_to, tz = "GMT", quiet = TRUE))
    
    ## unique ids
    if (!all) meta <- distinct(meta, site, .keep_all = TRUE)
  }
  
  if (source %in% tolower(c("saqn", "saqd"))) {
    tmp <- tempfile()
    
    # load data
    load(url("http://www.scottishairquality.scot/openair/R_data/scotarc_metadata.RData"))
    
    meta <- metadata %>%
      filter(network_id == "saun") %>%
      mutate(date_ended = ifelse(date_ended == "0000-00-00", NA, date_ended))
    
    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) meta <- distinct(meta, site, .keep_all = TRUE)
    
    ## rename to match imported names e.g. importAURN
    meta <- rename(meta, code = site, site = site_name)
  }
  
  if (source %in% tolower(c("waqn"))) {
    tmp <- tempfile()
    
    # load data
    load(url("https://airquality.gov.wales/sites/default/files/openair/R_data/waq_metadata.RData"))
    
    meta <- metadata %>%
      filter(network_id == "waun") %>%
      mutate(date_ended = ifelse(date_ended == "0000-00-00", NA, date_ended))
    
    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) meta <- distinct(meta, site, .keep_all = TRUE)
    
    ## rename to match imported names e.g. importAURN
    meta <- rename(meta, code = site, site = site_name)
  }
  
  if (source %in% tolower(c("aqe"))) {
    tmp <- tempfile()
    
    # load data
    load(url("https://airqualityengland.co.uk/assets/openair/R_data/aqengland_metadata.RData"))
    
    meta <- metadata %>%
      mutate(date_ended = ifelse(date_ended == "0000-00-00", NA, date_ended))
    
    ## only extract one line per site to make it easier to use file
    ## mostly interested in coordinates
    if (!all) meta <- distinct(meta, site, .keep_all = TRUE)
    
    ## rename to match imported names e.g. importAURN
    meta <- rename(meta, code = site, site = site_name)
  }
  
  
  if (source == "kcl") {
    con <- url("http://www.londonair.org.uk/r_data/sites.RData")
    meta <- get(load(con))
    close(con)
    
    ## rename to match imported names e.g. importKCL
    meta <- rename(meta,
                   code = SiteCode, site = SiteName, site_type = Classification,
                   latitude = Latitude, longitude = Longitude
    )
  }
  
  if (source == "europe") {
    
    file <- "http://aq-data.ricardo-aea.com/R_data/saqgetr/helper_tables/sites_table.csv.gz"
    
    
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
    meta <- read_csv(file, col_types = col_types, progress = FALSE) %>% 
      mutate(date_start = lubridate::ymd_hms(date_start, tz = "UTC"),
             date_end = lubridate::ymd_hms(date_end, tz = "UTC"))
    
    meta <- rename(meta, code = site, site = site_name)
    
  }
  
  if (!all) meta <- subset(meta, select = c(site, code, latitude, longitude, site_type))
  
  # change some names
  if ("variable" %in% names(meta)) {
    id <- which(meta$variable == "NOXasNO2")
    
    if (length(id) > 0) {
      meta$variable[id] <- "NOx"
    }
  }
  
  as_tibble(meta)
}
