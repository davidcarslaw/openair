##' Northern Ireland Network data import for openair
##'
##' @keywords methods
##' @describeIn importAURN Import data from the Northern Ireland Air Quality Network
##' @export
##'
importNI <- function(site = "bel0", year = 2018, pollutant = "all",
                      meta = FALSE, ratified = FALSE,
                      to_narrow = FALSE) {
  
  aq_data <- importUKAQ(site, year, pollutant,
                        meta, ratified,
                        to_narrow, 
                        source = "ni")  
  
  return(aq_data)
  
}
