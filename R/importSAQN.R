##' Scottish Air Quality Network data import for openair
##'
##' @describeIn importAURN Import data from the Scottish Air Quality Network
##' @export
##' 
importSAQN <- function(site = "gla4", year = 2009, pollutant = "all", 
                       meta = FALSE, ratified = FALSE, 
                       to_narrow = FALSE) {
  
  aq_data <- importUKAQ(site, year, pollutant,
                        meta, ratified,
                        to_narrow, 
                        source = "saqn")  
  
  return(aq_data)
}
