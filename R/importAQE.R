##' Air Quality England Network data import for openair
##'
##' @keywords methods
##' @describeIn importAURN Import data from the Air Quality England
##' @export
##'
importAQE <- function(site = "yk13", year = 2018, pollutant = "all",
                      meta = FALSE, ratified = FALSE,
                      to_narrow = FALSE) {
  
  aq_data <- importUKAQ(site = site, year = year, pollutant = pollutant,
                        meta = meta, ratified = ratified,
                        to_narrow = to_narrow, 
                        source = "aqe")  
  
  return(aq_data)
  
}
