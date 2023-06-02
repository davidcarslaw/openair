
#' worker function that downloads data from a range of networks run by Ricardo
#' @noRd
importUKAQ <-
  function(site = "my1",
           year = 2009,
           data_type = "hourly",
           pollutant = "all",
           hc = FALSE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           source = "aurn",
           lmam_subfolder) {
    # force source to be lowercase
  source <- tolower(source)

  # obtain url
  url_data <-
    switch(source,
      aurn = "https://uk-air.defra.gov.uk/openair/R_data/",
      saqn = "https://www.scottishairquality.scot/openair/R_data/",
      aqe = "https://airqualityengland.co.uk/assets/openair/R_data/",
      waqn = "https://airquality.gov.wales/sites/default/files/openair/R_data/",
      ni = "https://www.airqualityni.co.uk/openair/R_data/",
      local = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/",
      stop("Invalid source.")
    )

  # add to path if source = "local"
  if (source == "local") {
    url_data <- paste0(url_data, lmam_subfolder, "/")
  }

  # For file name matching, needs to be exact
  site <- toupper(site)

  # If meta or ratified, get metadata
  if (ratified) {
    meta_data <- importMeta(source = source, all = TRUE)
  }

  # combine site with year to create file names
  files <- paste0(site, "_", year)

  # Download and load data.
  thedata <-
    loadData(files, verbose, ratified, meta_data, url_data, data_type)

  # Return if no data
  if (nrow(thedata) == 0) {
    return()
  } ## no data

  ## suppress warnings for now - unequal factors, harmless
  if (is.null(thedata)) {
    stop("No data to import - check site codes and year.", call. = FALSE)
  }

  ## change names
  names(thedata) <- tolower(names(thedata))

  ## change nox as no2
  id <- which(names(thedata) %in% "noxasno2")
  if (length(id) == 1) names(thedata)[id] <- "nox"

  # change code to character
  thedata$code <- as.character(thedata$code)


  ## should hydrocarbons be imported?
  if (hc) {
    thedata <- thedata
  } else {
    ## no hydrocarbons - therefore select conventional pollutants
    theNames <- c(
      "site", "code", "date", "co", "nox", "no2", "no", "o3", "so2", "pm10",
      "pm2.5", "v10", "v2.5", "nv10", "nv2.5", "gr_pm10", "gr_pm2.5",
      "ws", "wd", "temp"
    )

    thedata <- select(thedata, any_of(theNames) | matches("_qc"))
  }

  if ("temp" %in% names(thedata)) {
    thedata <- rename(thedata, air_temp = temp)
  }

  ## if particular pollutants have been selected
  if (pollutant[1] != "all") {
    thedata <- thedata[, c("date", pollutant, "site", "code")]
  }

  ## make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"

  if (to_narrow) {
    # variables to select or not select
    the_vars <- c(
      "date", "site", "code",
      "latitude", "longitude", "site_type",
      "ws", "wd", "air_temp"
    )

    if (ratified) {
      thedata <- thedata %>%
        tidyr::pivot_longer(-dplyr::any_of(the_vars),
                            names_to = "pollutant",
                            values_to = "temp_val") %>%
        dplyr::mutate(
          name = dplyr::if_else(stringr::str_detect(pollutant, "_qc"), "qc", "value"),
          pollutant = stringr::str_remove(pollutant, "_qc")
        ) %>%
        tidyr::pivot_wider(names_from = "name", values_from = "temp_val") %>%
        dplyr::mutate(qc = as.logical(qc)) %>%
        dplyr::group_by(pollutant, site) %>%
        dplyr::filter(!all(is.na(value))) %>%
        dplyr::ungroup()
    } else {
      thedata <-
        tidyr::pivot_longer(thedata,
                            cols = -dplyr::any_of(the_vars),
                            names_to = "pollutant")
    }

    thedata <- thedata %>%
      dplyr::relocate(dplyr::any_of(the_vars)) %>%
      dplyr::arrange(site, code, pollutant, date)
  }

  as_tibble(thedata)
}



# Define downloading and loading function
# No export
loadData <- function(x, verbose, ratified, meta_data, url_data, data_type) {
  tryCatch(
    {
      # Build the file name
      fileName <- paste0(
        url_data, x,
        ".RData"
      )

      # Load the rdata object
      con <- url(fileName)
      load(con)

      # Find appropriate extension per `data_type`
      x <- switch(data_type,
                  hourly = x,
                  `15min` = paste0(x, "_15min"),
                  daily = paste0(x, "_daily_mean"),
                  `8_hour` = paste0(x, "_8hour_mean"),
                  `24_hour` = paste0(x, "_24hour_mean"),
                  daily_max_8 = paste0(x, "_daily_max_8hour"))

      # Gravimetric PM is in separate file
      # These are measured daily PM measurements rather than daily mean hourly
      x2 <- gsub("_daily_mean", "_daily", x)

      # Reasign
      dat <- get(x)

      # if there are two daily data frames to combine
      if (data_type == "daily" & exists(x2)) {
        dat2 <- get(x2)
        dat <- left_join(dat, dat2,
                         by = c("date", "site", "code"))

        lookup <- c(gr_pm2.5 = "GR2.5", gr_pm10 = "GR10")

        dat <- dat %>%
          rename(any_of(lookup))
      }

      # make sure class is correct for lubridate
      class(dat$date) <- c("POSIXct", "POSIXt")

      # add ratification information
      if (ratified && data_type == "hourly") {
        site_code <- strsplit(x, split = "_")[[1]][1]

        meta_data <- filter(
          meta_data, code == site_code,
          !variable %in% c(
            "V10", "NV10", "V2.5", "NV2.5",
            "ws", "wd", "temp"
          )
        ) %>%
          select(variable, ratified_to)

        for (i in 1:nrow(meta_data)) {
          dat <- add_ratified(dat,
                              variable = meta_data$variable[i],
                              ratified_to = meta_data$ratified_to[i]
          )
        }
      }


      return(dat)
    },
    error = function(ex) {
      # Print a message
      if (verbose) {
        message(x, "does not exist - ignoring that one.")
      }
    },
    finally = {
      close(con)
    }
  )
}

add_ratified <- function(data, variable, ratified_to) {
  new_var <- paste0(variable, "_qc")
  data <- mutate(data, {{ new_var }} := ifelse(date <= ratified_to, TRUE, FALSE))

  return(data)
}

# function to read annual or monthly files

readSummaryData <-
  function(fileName, data_type, to_narrow, meta, hc) {
    thedata <- try(readRDS(url(fileName)), TRUE)

    if (inherits(thedata, "try-error")) {
      return()
    }

    names(thedata)[names(thedata) == "NOXasNO2.mean"] <- "nox"
    names(thedata)[names(thedata) == "NOXasNO2.capture"] <-
      "nox_capture"
    names(thedata) <- tolower(names(thedata))
    names(thedata) <- gsub(".mean", "", names(thedata))
    names(thedata) <- gsub(".capture", "_capture", names(thedata))

    if (!hc) {
      thedata <- thedata %>%
        select(any_of(
          c(
            "date",
            "uka_code",
            "code",
            "site",
            "year",
            "o3",
            "o3_capture",
            "o3.summer_capture",
            "o3.daily.max.8hour",
            "o3.aot40v",
            "o3.aot40f",
            "somo35",
            "somo35_capture",
            "no",
            "no_capture",
            "no2",
            "no2_capture",
            "nox",
            "nox_capture",
            "so2",
            "so2_capture",
            "co",
            "co_capture",
            "pm10",
            "pm10_capture",
            "nv10",
            "nv10_capture",
            "v10",
            "v10_capture",
            "pm2.5",
            "pm2.5_capture",
            "nv2.5",
            "nv2.5_capture",
            "v2.5",
            "v2.5_capture",
            "gr10",
            "gr10_capture",
            "gr2.5",
            "gr2.5_capture",
            "gr_pm2.5",
            "gr_pm10"
          )
        ))
    }


    if (data_type == "monthly") {
      thedata <- mutate(thedata, date = ymd(date, tz = "UTC"))
    }

    if (data_type == "annual") {
      thedata <- rename(thedata, date = year) %>%
        drop_na(date) %>%
        mutate(date = ymd(paste0(date, "-01-01"), tz = "UTC"))
    }

    if (to_narrow) {
      # make sure numbers are numbers
      values <- select(thedata,!contains("capture")) %>%
        select(!matches("uka_code"))

      capture <-
        select(thedata, contains("capture") | c(code, date, site)) %>%
        select(!matches("uka_code"))

      values <- pivot_longer(values,
                             -c(date, code, site),
                             values_to = "value",
                             names_to = "species")

      capture <- pivot_longer(capture,
                              -c(date, code, site),
                              values_to = "data_capture",
                              names_to = "species")

      capture$species <- gsub("_capture", "", capture$species)

      thedata <- full_join(values, capture,
                           by = c("date", "code", "site", "species"))
    }

    thedata <- thedata %>%
      mutate(site = as.character(site),
             code = as.character(code))


    return(thedata)
  }

readDAQI <- function(fileName) {
  thedata <- try(readRDS(url(fileName)), TRUE)

  if (inherits(thedata, "try-error")) {
    return()
  }

  thedata <- thedata %>%
    mutate(
      code = as.character(code),
      site = as.character(site),
      pollutant = as.character(pollutant),
      date = ymd(Date, tz = "GMT"),
      measurement_period = as.character(measurement_period)
    ) %>%
    select(-Date) %>%
    relocate(date, .after = pollutant)

  return(thedata)
}

# function to add meta data based on network and supply of aq data
add_meta <- function(source, aq_data) {
  meta_data <- importMeta(source = source)

  meta_data <- distinct(meta_data, site, .keep_all = TRUE) %>%
    select(site, code, latitude, longitude, site_type)

  aq_data <- left_join(aq_data, meta_data, by = c("code", "site"))

  return(aq_data)
}

#' Function to filter annual/DAQI stats using
#' @param missing_site Input should be `missing(site)`
#' @param site,pollutant,to_narrow Inherits from parent function
#' @noRd
filter_annual_stats <- function(aq_data, missing_site, site, pollutant, to_narrow, data_type){
  # if site isn't missing, filter by sites
  if (!missing_site) {
    aq_data <- aq_data[tolower(aq_data$code) %in% tolower(site),]
  }

  # if pollutant isn't "all", filter pollutants
  if (any(pollutant != "all")) {
    polls <- paste(c("uka_code", "code", "site", "date", "pollutant", pollutant), collapse = "|")
    if (data_type != "daqi") {
      if (to_narrow) {
        aq_data <- aq_data[grepl(polls, aq_data$species, ignore.case = TRUE),]
      } else {
        aq_data <- aq_data[grepl(polls, names(aq_data), ignore.case = TRUE)]
      }
    } else {
      aq_data <- aq_data[grepl(polls, aq_data$pollutant, ignore.case = TRUE),]
    }
  }

  # return output
  return(aq_data)
}
