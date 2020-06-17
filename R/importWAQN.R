##' Welsh Air Quality Network data import for openair
##'
##' @keywords methods
##' @describeIn importAURN Import data from the Welsh Air Quality Network
##' @export
##' 
importWAQN <- function(site = "card", year = 2018, pollutant = "all", 
                       meta = FALSE, ratified = FALSE,
                       to_narrow = FALSE) {
  
  aq_data <- importUKAQ(site, year, pollutant,
                        meta, ratified,
                        to_narrow, 
                        source = "waqn")  
  
  return(aq_data)
  
}
