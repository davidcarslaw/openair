##' Air Quality England Network data import for openair
##'
##' @keywords methods
##' @describeIn importAURN Import data from the Air Quality England
##' @export
##'
importAQE <- function(site = "yk13", year = 2018, data_type = "hourly",
                      pollutant = "all",
                      meta = FALSE, ratified = FALSE,
                      to_narrow = FALSE) {
  
  if (data_type %in% c("annual", "monthly")) {
    
    files <- paste0("https://airqualityengland.co.uk/assets/openair/R_data/summary_", 
                    data_type, "_AQE_", year, ".rds")
    
    
    aq_data <- map_df(files, readSummaryAURN, data_type = data_type, to_narrow = to_narrow)
    
    # add meta data?
    if (meta) {
      
      meta_data <- importMeta(source = "aqe")
      
      meta_data <- distinct(meta_data, site, .keep_all = TRUE) %>% 
        select(site, code, latitude, longitude, site_type)
      # suppress warnings about factors
      aq_data <- left_join(aq_data, meta_data, by = c("code", "site"))
      
    }
    
    
  } else {
  
  aq_data <- importUKAQ(site = site, year = year, data_type,
                        pollutant = pollutant,
                        meta = meta, ratified = ratified,
                        to_narrow = to_narrow, 
                        source = "aqe")  
  
  }
  
  return(as_tibble(aq_data))
  
}
