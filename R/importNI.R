##' Northern Ireland Network data import for openair
##'
##' @keywords methods
##' @describeIn importAURN Import data from the Northern Ireland Air Quality Network
##' @export
##'
importNI <- function(site = "bel0", year = 2018, data_type = "hourly",
                     pollutant = "all",
                      meta = FALSE, ratified = FALSE,
                      to_narrow = FALSE) {
  
  if (data_type %in% c("annual", "monthly")) {
    
    files <- paste0("https://www.airqualityni.co.uk/openair/R_data/summary_", 
                    data_type, "_NI_", year, ".rds")
    
    
    aq_data <- map_df(files, readSummaryData, 
                      data_type = data_type, 
                      to_narrow = to_narrow,
                      hc = FALSE)
    
    # add meta data?
    if (meta) {
      
      aq_data <- add_meta(source = "ni", aq_data)
      
    }
    
    
  } else {
  
  aq_data <- importUKAQ(site = site, year = year, data_type,
                        pollutant = pollutant,
                        meta = meta, ratified = ratified,
                        to_narrow = to_narrow, 
                        source = "ni")  
  
  }
  
  return(as_tibble(aq_data))
  
}
