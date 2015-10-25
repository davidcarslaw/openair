##' Helper function to find airbase site codes
##'
##' This function helps to identify airbase site codes based on one or
##' more criteria. The criteria include country code, site type, local
##' site code, city, site name and latitude/longitude ranges.
##' @title Helper function to find EEA airbase site codes
##' @param country A character or vector of characters representing country code.
##' @param site.type One of \dQuote{background}, \dQuote{traffic},
##' \dQuote{industrial}, \dQuote{unknown} representing the type of
##' site.
##' @param area.type The type of area in which the site is
##' located. Can be \dQuote{rural}, \dQuote{urban}, \dQuote{suburban},
##' \dQuote{unknown}.
##' @param local.code A character or vector of characters representing
##' the local site code. For example \dQuote{MY1} is the UK code for
##' Marylebone Road.
##' @param city A city name to search --- using character matching
##' (\code{grep}). The search string can be upper or lower case
##' e.g. \code{city = "london"}. To extract several cities
##' e.g. Copenhagen and Barcelona use \code{city = c("copenhagen",
##' "barcelona")}. Note that by default any matching characters are
##' returned, so \code{city = "london"} would also return Londonderry
##' (Northern Ireland).
##'
##' Regular expression searches are very powerful and potentially
##' complicated. However there are a few useful tips. To match the
##' \emph{beginning} of a name use \sQuote{^}. So \code{city =
##' "^london"} would return London and Londonderry (both begin with
##' \sQuote{london}). To match the end of a name use \sQuote{$}, so
##' \code{city = "london$"} would just return London but not
##' Londonderry.
##'
##' The cities chosen are printed to screen to make it easy to check
##' (and refine the search string) of the selected sites.
##' @param site The name of the site or sites to search, see
##' \code{city} for details of how to search.
##' @param emep Select an EMEP station. Can be \dQuote{yes},
##' \dQuote{no} or \code{NA} (the default, selects everything).
##' @param lat The latitude range to select in the form c(lower, upper).
##' @param lon The longitude range to select in the form c(lower, upper).
##' @return A vector of airbase site codes that can be used directly
##' in \code{\link{importAirbase}}.
##' @seealso \code{\link{importAirbase}},
##' \code{\link{airbaseFindCode}}, \code{\link{airbaseStats}} and
##' \code{\link{airbaseInfo}}
##' @export
##' @author David Carslaw
##' @examples
##'
##' ## select all sites in Denmark
##' \dontrun{sites <- airbaseFindCode(country = "DK")
##'
##' ## traffic sites in Germany and the UK
##' sites <- airbaseFindCode(country = c("DE", "GB"), site.type = "traffic")}
airbaseFindCode <- function(country = c("AL", "AT", "BA", "BE", "BG", "CH", "CY", "CZ",
                                "DE", "DK", "EE", "ES", "FI", "FR", "GB", "GR", "HR", "HU",
                                "IE", "IS", "IT", "LI", "LT", "LU","LV", "ME", "MK", "MT",
                                "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "TR"),
                            site.type = c("background", "traffic", "industrial", "unknown"),
                            area.type = c("rural", "urban", "suburban", "unknown"), 
                            local.code = NA, city = NA, site = NA, emep = NA, 
                            lat = c(-90, 90), lon = c(-180, 180)) {
    ##keep R check quiet
    site.info <- EMEP_station <- NULL
    
    ## initial checks
    airbase.count <-  c("AL", "AT", "BA", "BE", "BG", "CH", "CY", "CZ", "DE", "DK",
                        "EE", "ES", "FI", "FR", "GB", "GR", "HR", "HU", "IE", "IS",
                        "IT", "LI", "LT", "LU","LV", "ME", "MK", "MT", "NL", "NO",
                        "PL", "PT", "RO", "RS", "SE", "SI", "SK", "TR")
    
    country <- sapply(toupper(country), function (x) match.arg(x, airbase.count))

    site.type <- sapply(toupper(site.type), function (x)
                        match.arg(x, toupper(c("background", "traffic", "industrial", "unknown"))))

    area.type <- sapply(toupper(area.type), function (x)
                        match.arg(x, toupper(c("rural", "urban", "suburban", "unknown"))))
    
    
    all <- url("http://www.erg.kcl.ac.uk/downloads/Policy_Reports/airbase/site.info.RData")
    load(all)
                
    all <- site.info

    ## country
    id <- which(toupper(all$country.code) %in% toupper(country))
    res <- all[id, ]

    ## site type
    res <- res[toupper(res$site.type) %in% toupper(site.type) , ]
    
    ## area type
    res <- res[toupper(res$station_type_of_area) %in% toupper(area.type) , ]

    ## lat/lon
    id <- which(res$lat >= lat[1] & res$lat <= lat[2] & res$lon >= lon[1] &
                res$lon <= lon[2])
    res <- res[id, ]

    if (all(!is.na(local.code))) {
        
        id <- which(toupper(res$station_local_code) %in% toupper(local.code))
        res <- res[id, ]
    }

    ## EMEP sites
   if (!is.na(emep)) {
       
       res <- subset(res, EMEP_station == emep)
   }

    ## city, using grep - last because it prints city names
    if (!any(is.na(city))) {
        city <- paste0(city, collapse = "|")
        
        id <- grep(city, res$city, ignore.case = TRUE)
        cities <- data.frame(code = res$code[id], city = res$city[id])
        print(cities)
        res <- res[id, ]              
    }

    ## site name, using grep - last because it prints city names
    if (!any(is.na(site))) {
        site <- paste0(site, collapse = "|")
        
        id <- grep(site, res$site, ignore.case = TRUE)
        sites <- data.frame(code = res$code[id], site = res$site[id])
        print(sites)
        res <- res[id, ]              
    }
    
    as.character(res$code)

}
