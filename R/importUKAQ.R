importUKAQ <- function(site = "my1", year = 2009, data_type = "hourly",
                       pollutant = "all",
                       hc = FALSE, meta = FALSE, ratified = FALSE,
                       to_narrow = FALSE, verbose = FALSE,
                       source = "aurn") {
  
  # worker function that downloads data from a range of networks run by Ricardo
  
  source <- tolower(source)
  
  if (source == "aurn") {
    
    url_data <- "https://uk-air.defra.gov.uk/openair/R_data/"
    source_meta <- "aurn"
    
  } 
  
  if (source == "saqn") {
    
    url_data <- "http://www.scottishairquality.scot/openair/R_data/"
    source_meta <- "aurn"
    
  } 
  
  if (source == "aqe") {
    
    url_data <- "https://airqualityengland.co.uk/assets/openair/R_data/"
    source_meta <- "aqe"
    
  } 
  
  if (source == "waqn") {
    
    url_data <- "https://airquality.gov.wales/sites/default/files/openair/R_data/"
    source_meta <- "waqn"
    
  } 
  
  if (source == "ni") {
    
    url_data <- "https://www.airqualityni.co.uk/openair/R_data/"
    source_meta <- "ni"
   
  }
  
  
  # For file name matching, needs to be exact
  site <- toupper(site)
  
  if (meta | ratified)
    meta_data <- importMeta(source = source_meta, all = TRUE)
  
  # Create file name vector
  if (data_type %in% c("annual", "monthly")) {
    
  } else {
  
  files <- map(site, ~ paste0(.x, "_", year)) %>% 
    flatten_chr()
  
  }
  
  # Download and load data. 
    
    thedata <- map_df(files, ~ loadData(.x, verbose, ratified, meta_data, 
                                        url_data, data_type))
    
   
  # Return if no data
  if (nrow(thedata) == 0) return() ## no data
  
  ## suppress warnings for now - unequal factors, harmless
  
  if (is.null(thedata)) {
    stop("No data to import - check site codes and year.", call. = FALSE)
  }
  
  
  ## change names
  names(thedata) <- tolower(names(thedata))
  
  ## change nox as no2
  id <- which(names(thedata) %in% "noxasno2")
  if (length(id) == 1) names(thedata)[id] <- "nox"
  
  
  ## should hydrocarbons be imported?
  if (hc) {
    thedata <- thedata
  } else {
    
    
    ## no hydrocarbons - therefore select conventional pollutants
    theNames <- c(
      "site", "code", "date", "co", "nox", "no2", "no", "o3", "so2", "pm10", 
      "pm2.5", "v10", "v2.5", "nv10", "nv2.5", "ws", "wd", "temp"
    )
    
    thedata <- select(thedata, any_of(theNames) | matches("_qc"))
    
  }
  
  if ("temp" %in% names(thedata))
    thedata <- rename(thedata, air_temp = temp)
  
  ## if particular pollutants have been selected
  if (pollutant != "all") {
    thedata <- thedata[, c("date", pollutant, "site", "code")]
  }
  
  ## make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"
  
  # make sure class is correct for lubridate
  class(thedata$date) <- c("POSIXct", "POSIXt")
  
  
  if (meta) {
    meta_data <- distinct(meta_data, site, .keep_all = TRUE) %>% 
      select(site, code, latitude, longitude, site_type)
    # suppress warnings about factors
    thedata <- left_join(thedata, meta_data, by = c("code", "site"))
  }
  
  
  if (to_narrow) {
    
    if (ratified) {
      warning("Cannot re-shape if ratified is TRUE")
      return()
    }
    
    if (meta) {
      
      thedata <- pivot_longer(thedata, -c(date, site, code, latitude, 
                                          longitude, site_type), 
                              names_to = "pollutant") %>% 
        arrange(site, code, pollutant, date)
      
    } else {
      
      thedata <- pivot_longer(thedata, -c(date, site, code), 
                              names_to = "pollutant") %>% 
        arrange(site, code, pollutant, date)
      
    }
  }
  
  as_tibble(thedata)
}



# Define downloading and loading function
# No export
loadData <- function(x, verbose, ratified, meta_data, url_data, data_type) {
  tryCatch({
    
    # Download file to temp directory
    # need to do this because of https, certificate problems
    tmp <- tempfile()
    
    # Build the file name
    fileName <- paste0(
      url_data, x,
      ".RData"
    )
    
    # No warnings needed, function gives message if file is not present
    suppressWarnings(
      download.file(
        fileName,
        method = "libcurl", destfile = tmp,
        quiet = !verbose
      )
    )
    
    # Load the rdata object
    load(tmp)
    
    if (data_type == "hourly")
      x <- x
    
    if (data_type == "15min")
      x <- paste0(x, "_15min")
    
    if (data_type == "daily")
      x <- paste0(x, "_daily_mean")
    
    
    # Reasign
    dat <- get(x)
    
    # add ratification information
    if (ratified && data_type == "hourly") {
      
      site_code <- strsplit(x, split = "_")[[1]][1]
      
      meta_data <- filter(meta_data, code == site_code, 
                          !variable %in% c("V10", "NV10", "V2.5", "NV2.5",
                                           "ws", "wd", "temp")) %>% 
        select(variable, ratified_to)
      
      for (i in 1:nrow(meta_data)) {
        
        dat <- add_ratified(dat, variable = meta_data$variable[i],
                            ratified_to = meta_data$ratified_to[i])
        
      }
      
      
    }
    
    
    return(dat)
    
  }, error = function(ex) {
    
    # Print a message
    if (verbose) {
      message(x, "does not exist - ignoring that one.")
    }
  })
}

add_ratified <- function(data, variable, ratified_to) {
  
  new_var <- paste0(variable, "_qc")
  data <- mutate(data, {{new_var}} := ifelse(date <= ratified_to, TRUE, FALSE))
  
  return(data)
}
