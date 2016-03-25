                                        # Check input file and prepare data
                                        #
                                        # Author: DCC
###############################################################################

checkPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE,
                      strip.white = TRUE, wd = "wd") {
  
  ## deal with conditioning variable if present, if user-defined, must exist in data
  ## pre-defined types
  ## existing conditioning variables that only depend on date (which is checked)
  conds <- c("default", "year", "hour", "month", "season", "weekday", 
             "weekend", "monthyear", "gmtbst", "bstgmt", "dst", "daylight")
  all.vars <- unique(c(names(mydata), conds))
  
  varNames <- c(Names, type) ## names we want to be there
  matching <- varNames %in% all.vars
  
  if (any(!matching)) {
    ## not all variables are present
    stop(cat("Can't find the variable(s)", varNames[!matching], "\n"))
  }
  
  ## add type to names if not in pre-defined list
  if (any(type %in% conds == FALSE)) {
    ids <- which(type %in% conds == FALSE)
    Names <- c(Names, type[ids])
  }
  
  ## if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }
  
  ## just select data needed
  mydata <- mydata[, Names]
  
  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site
    
    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }
  }
  
  
  ## sometimes ratios are considered which can results in infinite values
  ## make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x){replace(x, x == Inf | x == -Inf, NA)})
  
  if ("ws" %in% Names) {
    if ("ws" %in% Names & is.numeric(mydata$ws)) {
      
      ## check for negative wind speeds
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        
        if (remove.neg) { ## remove negative ws only if TRUE
          warning("Wind speed <0; removing negative data")
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
  }
  
  ## round wd to make processing obvious
  ## data already rounded to nearest 10 degress will not be affected
  ## data not rounded will be rounded to nearest 10 degrees
  ## assumes 10 is average of 5-15 etc
  if (wd %in% Names) {
    if (wd %in% Names & is.numeric(mydata[, wd])) {
      
      ## check for wd <0 or > 360
      if (any(sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
              mydata[[wd]][!is.na(mydata[[wd]])] > 360)) {
        
        warning("Wind direction < 0 or > 360; removing these data")
        mydata[[wd]][mydata[[wd]] < 0] <- NA
        mydata[[wd]][mydata[[wd]] > 360] <- NA
      }
      
      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata[[wd]][mydata$ws == 0]  <- NA ## set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
        
        ## round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360   # angles <5 should be in 360 bin
        
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
    }
  }
  
  
  ## make sure date is ordered in time if present
  if ("date" %in% Names) {
    
    if ("POSIXlt" %in% class(mydata$date))
      stop ("date should be in POSIXct format not POSIXlt")
    
    ## if date in format dd/mm/yyyy hh:mm (basic check)
    if (length(grep("/", as.character(mydata$date[1]))) > 0) {
      
      mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"), "GMT")
      
    }
    
    ## try and work with a factor date - but probably a problem in original data
    if (is.factor(mydata$date))  {
      
      warning("date field is a factor, check date format")
      mydata$date <- as.POSIXct(mydata$date, "GMT")
      
    }
    
    mydata <- arrange(mydata, date)
    
    ## make sure date is the first field
    if (names(mydata)[1] != "date") 
      mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
    
    ## check to see if there are any missing dates, stop if there are
    ids <- which(is.na(mydata$date))
    if (length(ids) > 0) {
      
      mydata <- mydata[-ids, ]
      warning(paste("Missing dates detected, removing",
                    length(ids), "lines"), call. = FALSE)
    }
    
    ## daylight saving time can cause terrible problems - best avoided!!
    
    if (any(dst(mydata$date))) {
      warning("Detected data with Daylight Saving Time, converting to UTC/GMT")
      mydata$date <- lubridate::force_tz(mydata$date, tzone = "GMT")
     
    }
    
  }
  
  
  
  
  if (strip.white) {
    ## set panel strip to white
    suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))
  }
  
  
  ## return data frame
  return(mydata)
}
