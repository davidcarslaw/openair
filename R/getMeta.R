
##' Import monitoring site meta data for the AURN, KCL, SAQN, WAQN networks
##'
##' Function to import meta data from UK air pollution monitoring sites
##'
##' This function imports site meta data from four networks in the UK: the Defra
##' Automatic Urban and Rural Network (AURN), King's College London networks,
##' the Scottish Air Quality Network, Welsh Air Quality Network and Air Quality
##' England sites. The meta data includes site location (latitude, longitude and
##' OS easting and northing --- the latter for KCL networks), site type and it's
##' start/close data, as well as other information.
##'
##' The Scottish and Welsh air quality networks are available as SAQN (or SAQD)
##' and WAQN.
##'
##' The meta information can usefully be combined with matching air pollution
##' data and produce maps of concentration --- see examples below. Note if many
##' sites and/or years of hourly data are imported it may be better to aggregate
##' first and then combine with the meta data information.
##'
##' Thanks go to Dr Ben Barratt (KCL) and Trevor Davies (Riacrdo) for making
##' these data available.
##' @param source The data source for the meta data. Can be "aurn", "kcl",
##'   "saqn" (or "saqd") and "aqe"; upper or lower case.
##' @param all When \code{all = FALSE} only the site code, site name, latitude
##'   and longitude and site type are imported. Setting \code{all = TRUE} will
##'   import all available meta data and provide details (when available) or the
##'   individual pollutants measured at each site.
##' @return A data frame with meta data.
##' @author David Carslaw
##' @seealso \code{\link{importAURN}}, \code{\link{importKCL}} and
##'   \code{\link{importSAQN}} for importing air quality data from each network.
##' @keywords methods
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
    site = code = latitude = longitude = site.type = site_name = site_id = NULL
    location_type = SiteCode = SiteName = Classification = Latitude = Longitude = NULL
    metadata = site_type = date_ended = network_id = NULL

    ## meta data sources
    meta.source <- c("aurn", "kcl", "saqn", "saqd", "waqn", "aqe")

    ## ensure lower case
    source <- tolower(source)

    if (!source %in% meta.source) stop("Meta data sources are 'aurn', 'kcl' and 'saqn.")

    if (source == "aurn") {
        
        tmp <- tempfile()
        
        fileName <- "http://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData"
        download.file(fileName, method = "libcurl", destfile = tmp)
        load(tmp)
        
        meta <- AURN_metadata
        ## only extract one line per site to make it easier to use file
        ## mostly interested in coordinates
        
        ## rename to match imported names e.g. importAURN
        meta <- rename(meta, code = site_id, site = site_name, 
                       site_type = location_type, variable = parameter)

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
        meta <- rename(meta, code = site, site = site_name, site.type = site_type)
        
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
      meta <- rename(meta, code = site, site = site_name, site.type = site_type)
      
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
      meta <- rename(meta, code = site, site = site_name, site.type = site_type)
      
    }
    

    if (source == "kcl") {
        con <- url("http://www.londonair.org.uk/r_data/sites.RData")
        meta <- get(load(con))
        close(con)

        ## rename to match imported names e.g. importKCL
        meta <- rename(meta, code = SiteCode, site = SiteName,  site.type = Classification,
                               latitude = Latitude, longitude = Longitude)

        
    }

    if (!all) meta <-  subset(meta, select = c(site, code, latitude, longitude, site.type))
    
    # change some names
    if ("variable" %in% names(meta)) {
      
      id <- which(meta$variable == "NOXasNO2")
      
      if (length(id) > 0)
        meta$variable[id] <- "NOx"
      
    }

    as_tibble(meta)

}
