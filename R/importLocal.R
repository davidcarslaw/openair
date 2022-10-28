##' English locally managed networks data import for openair
##'
##' @keywords methods
##' @describeIn importAURN Import data from locally managed AQ networks in
##'   England.
##' @export
##'
importLocal <-
  function(site = "ad1",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           to_narrow = FALSE) {
    # Warn about QC/QA every 8 hrs
    rlang::warn(
      c(
        "i" = "This data is associated with locally managed air quality network sites in England.",
        "!" = "These sites are not part of the AURN national network, and therefore may not have the same level of quality control applied to them."
      ),
      .frequency = "regularly",
      .frequency_id = "lmam"
    )

    if (data_type %in% c("annual", "monthly")) {
      files <-
        paste0(
          "https://uk-air.defra.gov.uk/openair/LMAM/R_data/summary_",
          data_type,
          "_LMAM_",
          year,
          ".rds"
        )

      # read data
      aq_data <- map_df(
        files,
        readSummaryData,
        data_type = data_type,
        to_narrow = to_narrow,
        hc = FALSE
      )

      # add meta data?
      if (meta) {
        aq_data <- add_meta(source = "local", aq_data)
      }
    } else {
      # force uppercase
      site <- toupper(site)

      # get pcodes for file paths
      pcodes <-
        importMeta("local", all = TRUE) %>%
        dplyr::distinct(.data$site, .keep_all = TRUE) %>%
        select("code", "pcode")

      # get sites and pcodes
      site_pcodes <-
        data.frame(code = site) %>%
        merge(pcodes)

      # map over sites and pcodes
      # needed because sites may come from different pcodes
      aq_data <-
        map2_dfr(
          .x = site_pcodes$code,
          .y = site_pcodes$pcode,
          .f = ~ importUKAQ(
            site = .x,
            year = year,
            data_type,
            pollutant = pollutant,
            meta = meta,
            ratified = FALSE,
            to_narrow = to_narrow,
            source = "local",
            lmam_subfolder = .y
          )
        )
    }

    return(as_tibble(aq_data))
  }
