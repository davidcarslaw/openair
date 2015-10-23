
##' Import monitoring site meta data for the AURN, KCL and SAQN networks
##'
##' Function to import meta data from UK air pollution monitoring sites
##'
##' This function imports site meta data from three networks in the
##' UK: the Defra Automatic Urban and Rural Network (AURN), King's
##' College London networks and the Scottish Air Quality Network. The
##' meta data includes site location (latitude, longitude and OS
##' easting and northing --- the latter for KCL networks), site type
##' and it's start/close data, as well as other information.
##'
##' The meta information can usefully be combined with matching air
##' pollution data and produce maps of concentration --- see examples
##' below. Note if many sites and/or years of hourly data are imported
##' it may be better to aggregate first and then combine with the meta
##' data information.
##'
##' Thanks go to Dr Ben Barratt (KCL) and Trevor Davies (AEA) for
##' making these data available.
##' @param source The data source for the meta data. Can be "aurn",
##' "kcl" or "saqn"; upper or lower case.
##' @param all When \code{all = FALSE} only the site code, site name,
##' latitude and longitude and site type are imported. Setting
##' \code{all = TRUE} will import all available meta data.
##' @return A data frame with meta data.
##' @author David Carslaw
##' @seealso \code{\link{importAURN}}, \code{\link{importKCL}} and
##' \code{\link{importSAQN}} for importing air quality data from each
##' network, and \code{\link{GoogleMapsPlot}} for plotting
##' concentrations on a map.
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
##' ## make a GoogleMapsPlot
##' GoogleMapsPlot(annual, pollutant = "no2")
##' }
importMeta <- function(source = "aurn", all = FALSE) {

    ## get rid of R check annoyances
    site = code = latitude = longitude = site.type = site_name = site_id = NULL
    location_type = SiteCode = SiteName = Classification = Latitude = Longitude = NULL

    ## meta data sources
    meta.source <- c("aurn", "kcl", "saqn")

    ## ensure lower case
    source <- tolower(source)

    if (!source %in% meta.source) stop ("Meta data sources are 'aurn', 'kcl' and 'saqn.")

    if (source == "aurn") {
        con <- url("http://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData")
        meta <- get(load(con))
        close(con)
        ## only extract one line per site to make it easier to use file
        ## mostly interested in coordinates

        ## unique ids
        ids <- which(!duplicated(meta$site_id))
        meta <- meta[ids, ]

        ## rename to match imported names e.g. importAURN
        meta <- rename(meta, code = site_id, site = site_name, site.type = location_type)

    }

    if (source == "saqn") {
        con <- url("http://www.scottishairquality.co.uk/openair/R_data/SCOT_metadata.RData")
        meta <- get(load(con))
        close(con)
        ## only extract one line per site to make it easier to use file
        ## mostly interested in coordinates

        ## unique ids
        ids <- which(!duplicated(meta$site_id))
        meta <- meta[ids, ]

        ## rename to match imported names e.g. importAURN
        meta <- rename(meta, code = site_id, site = site_name, site.type = location_type)
        
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

    meta

}
