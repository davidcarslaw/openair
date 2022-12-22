#' Import data from the Scottish Air Quality Network
#'
#' @inheritParams importAURN
#' @param site Site code of the site to import e.g. \dQuote{gla4} is Glasgow
#'   Kerbside. Several sites can be imported with \code{site = c("gla4",
#'   "gla5")} --- to import Glasgow Kerbside and Anderston, for example.
#' @family import functions
#' @export
importSAQN <-
  function(site = "gla4",
           year = 2009,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           ratified = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    if (data_type %in% c("annual", "monthly")) {
      files <- paste0(
        "https://www.scottishairquality.scot/openair/R_data/summary_",
        data_type,
        "_SCOT_",
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
        aq_data <- add_meta(source = "saqn", aq_data)
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
        source = "saqn",
        progress = progress
      )
    }

    return(as_tibble(aq_data))
  }
