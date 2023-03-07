#' Import data from the Welsh Air Quality Network
#'
#' @inheritParams importAURN
#' @param site Site code of the site to import e.g. \dQuote{card} is Cardiff
#'   Centre. Several sites can be imported with \code{site = c("card", "car04")}
#'   --- to import Cardiff Centre and Castle Street, for example.
#' @family import functions
#' @export
importWAQN <-
  function(site = "card",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    if (data_type %in% c("annual", "monthly")) {
      files <- paste0(
        "https://airquality.gov.wales/sites/default/files/openair/R_data/summary_",
        data_type,
        "_WAQ_",
        year,
        ".rds"
      )

      if (progress)
        progress <- "Importing Statistics"
      aq_data <- purrr::map(
        files,
        readSummaryData,
        data_type = data_type,
        to_narrow = to_narrow,
        hc = FALSE,
        .progress = progress
      ) %>%
        purrr::list_rbind()

      # add meta data?
      if (meta) {
        aq_data <- add_meta(source = "waqn", aq_data)
      }
    } else {
      aq_data <- importUKAQ(
        site = site,
        year = year,
        data_type,
        pollutant = pollutant,
        meta = meta,
        ratified = ratified,
        to_narrow = to_narrow,
        source = "waqn",
        progress = progress
      )
    }
    
    # check to see if met data needed
    if (meteo == FALSE) {
      
      aq_data <- aq_data %>% 
        select(-any_of(c("ws", "wd", "air_temp")))
      
    }

    return(as_tibble(aq_data))
  }
