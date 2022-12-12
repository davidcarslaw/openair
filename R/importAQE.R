#' Import data from the Air Quality England
#'
#' @inheritParams importAURN
#' @param site Site code of the site to import e.g. \dQuote{yk13} is York
#'   Heworth Green. Several sites can be imported with \code{site = c("yk13",
#'   "yk8")} --- to import York Heworth Green and Holgate, for example.
#' @family import functions
#' @export
importAQE <- function(site = "yk13", year = 2018, data_type = "hourly",
                      pollutant = "all",
                      meta = FALSE, ratified = FALSE,
                      to_narrow = FALSE) {
  if (data_type %in% c("annual", "monthly")) {
    files <- paste0(
      "https://airqualityengland.co.uk/assets/openair/R_data/summary_",
      data_type, "_AQE_", year, ".rds"
    )


    aq_data <- map_df(files, readSummaryData,
      data_type = data_type,
      to_narrow = to_narrow,
      hc = FALSE
    )

    # add meta data?
    if (meta) {
      aq_data <- add_meta(source = "aqe", aq_data)
    }
  } else {
    aq_data <- importUKAQ(
      site = site, year = year, data_type,
      pollutant = pollutant,
      meta = meta, ratified = ratified,
      to_narrow = to_narrow,
      source = "aqe"
    )
  }

  return(as_tibble(aq_data))
}
