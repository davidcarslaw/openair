##' Scottish Air Quality Network data import for openair
##'
##' @describeIn importAURN Import data from the Scottish Air Quality Network
##' @export
##' 
importSAQN <- function(site = "gla4", year = 2009, data_type = "hourly",
                       pollutant = "all", 
                       meta = FALSE, ratified = FALSE, 
                       to_narrow = FALSE) {
  
  if (data_type %in% c("annual", "monthly")) {
    
    files <- paste0("https://www.scottishairquality.scot/openair/R_data/summary_", 
                    data_type, "_SCOT_", year, ".rds")
    
    
    aq_data <- map_df(files, readSummaryData, 
                      data_type = data_type, 
                      to_narrow = to_narrow,
                      hc = FALSE)
    
    # add meta data?
    if (meta) {
      
      aq_data <- add_meta(source = "saqn", aq_data)
      
    }
    
    
  } else {
  
  aq_data <- importUKAQ(site = site, year = year, data_type,
                        pollutant = pollutant,
                        meta = meta, ratified = ratified,
                        to_narrow = to_narrow, 
                        source = "saqn")  
  
  }
  
  return(as_tibble(aq_data))
}
