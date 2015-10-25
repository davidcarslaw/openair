##' Import pre-calculated airbase statistics
##'
##' The airbase database makes available detailed pre-calculated
##' statsitics for all species, sites and years. The
##' \code{airbaseStats} function provides these data.
##'
##' The data are imported depending on the statistic chosen --- see
##' the list of available statistics in the \code{statistics}
##' option. Further options are available to refine the selection
##' including a \emph{single} pollutant, year(s), site type(s), data
##' capture percentage threshold and airbase site code.
##'
##' Once the data have been imported it is then possible to subset the
##' data in various ways e.g. by extracting a particular species with
##' a defined data capture threshold.
##'
##'The following pollutants are available:
##'
##' \itemize{
##'\item (CH3)2-CH-CH2-CH2-CH3
##'  \item (CH3)3-C-CH2-CH-(CH3)2
##'  \item 1,2,3-C6H3(CH3)3
##'  \item 1,2,4-C6H3(CH3)3
##'  \item 1,3,5-C6H3(CH3)3
##'  \item 3-methylpentane
##'  \item Acenaphthene
##'  \item Acenaphtylene
##'  \item Al
##'  \item Anthracene
##'  \item As
##'  \item As in PM10
##'  \item As in PM2.5
##'  \item As in TSP
##'  \item BS
##'  \item BaP
##'  \item BaP in PM10
##'  \item BaP in PM2.5
##'  \item Benzo(a)anthracene
##'  \item Benzo(a)anthracene in PM10
##'  \item Benzo(b)fluoranthene
##'  \item Benzo(b)fluoranthene in PM10
##'  \item Benzo(b+j+k)fluoranthenes
##'  \item Benzo(b,j,k)fluoranthene
##'  \item Benzo(b,j,k)fluorantheneInPM1
##'  \item Benzo(e)pyrene
##'  \item Benzo(ghi)perylene
##'  \item Benzo(j)fluoranthene
##'  \item Benzo(j)fluoranthene in PM10
##'  \item Benzo(k)fluoranthene
##'  \item Benzo(k)fluoranthene in PM10
##'  \item Black Carbon
##'  \item C2Cl4
##'  \item C2H6
##'  \item C6H14
##'  \item C6H5-C2H5
##'  \item C6H5-CH3
##'  \item C6H5-CH=CH2
##'  \item C6H6
##'  \item C7H16
##'  \item C8H18
##'  \item CH2=CH-C(CH3)=CH2
##'  \item CH2=CH-CH3
##'  \item CH2=CH-CH=CH2
##'  \item CH4
##'  \item CHCl=CCl2
##'  \item CO
##'  \item CO2
##'  \item CS2
##'  \item Ca++
##'  \item Ca2+ in PM10
##'  \item Ca2+ in PM2.5
##'  \item Cd
##'  \item Cd in PM10
##'  \item Cd in PM2.5
##'  \item Cd in TSP
##'  \item Chrysene
##'  \item Chrysene + Triphenylene
##'  \item Cl-
##'  \item Cl- in PM10
##'  \item Cl- in PM2.5
##'  \item Co
##'  \item Cr
##'  \item Cu
##'  \item Cyclo-hexane
##'  \item Dibenzo(ah)anthracene
##'  \item Dibenzo(ah)anthracene in PM10
##'  \item EC in PM10
##'  \item EC in PM2.5
##'  \item Fe
##'  \item Fluoranthene
##'  \item Fluorene
##'  \item H+
##'  \item H2C=CH-CH2-CH2-CH3
##'  \item H2C=CH-CH2-CH3
##'  \item H2C=CH2
##'  \item H2S
##'  \item H3C-(CH2)3-CH3
##'  \item H3C-CH(CH3)2
##'  \item H3C-CH2-CH(CH3)2
##'  \item H3C-CH2-CH2-CH3
##'  \item H3C-CH2-CH3
##'  \item H3C-HC=CH-CH2-CH3
##'  \item HC C2-C6(excl. AROM. & CHLH)
##'  \item HC=CH
##'  \item HCHO
##'  \item HCl
##'  \item HF
##'  \item HNO3+NO3
##'  \item Hg
##'  \item Hg in PM10
##'  \item Hg-reactive
##'  \item Hg0
##'  \item Hg0 + Hg-reactive
##'  \item Indeno-(1,2,3-cd)pyrene
##'  \item Indeno-(1,2,3-cd)pyrene in PM
##'  \item K+
##'  \item K+ in PM10
##'  \item K+ in PM2.5
##'  \item Mg++
##'  \item Mg2+ in PM10
##'  \item Mg2+ in PM2.5
##'  \item Mn
##'  \item N-DEP
##'  \item N2O
##'  \item NH3
##'  \item NH3+NH4
##'  \item NH4
##'  \item NH4+
##'  \item NH4+ in PM10
##'  \item NH4+ in PM2.5
##'  \item NO
##'  \item NO2
##'  \item NO3
##'  \item NO3-
##'  \item NO3- in PM10
##'  \item NO3- in PM2.5
##'  \item NOX
##'  \item Na+
##'  \item Na+  in PM10
##'  \item Na+  in PM2.5
##'  \item Naphtalene
##'  \item Ni
##'  \item Ni in PM10
##'  \item Ni in PM2.5
##'  \item Ni in TSP
##'  \item O3
##'  \item OC in PM10
##'  \item OC in PM2.5
##'  \item PAH
##'  \item PAN
##'  \item PCB-105
##'  \item PCB-114
##'  \item PCB-118
##'  \item PCB-138
##'  \item PCB-141
##'  \item PCB-149
##'  \item PCB-153
##'  \item PCB-156
##'  \item PCB-157
##'  \item PCB-167
##'  \item PCB-170
##'  \item PCB-18
##'  \item PCB-180
##'  \item PCB-183
##'  \item PCB-187
##'  \item PCB-189
##'  \item PCB-194
##'  \item PCB-28
##'  \item PCB-31
##'  \item PCB-52
##'  \item PCB-74
##'  \item PCB-99
##'  \item PCB_123
##'  \item PM1
##'  \item PM10
##'  \item PM2.5
##'  \item Pb
##'  \item Pb in PM10
##'  \item Pb in PM2.5
##'  \item Pb in TSP
##'  \item Phenanthrene
##'  \item Pyrene
##'  \item Radioactivity
##'  \item S-DEP
##'  \item SA
##'  \item SO2
##'  \item SO2 + SO4--
##'  \item SO4 (H2SO4 aerosols) (SO4--)
##'  \item SO4--
##'  \item SO42-  in PM10
##'  \item SO42- in PM2.5
##'  \item SPM
##'  \item Se
##'  \item T-VOC
##'  \item THC (NM)
##'  \item V
##'  \item Vanadium
##'  \item Zn
##'  \item Zn in PM2.5
##'  \item cis-H3C-CH=CH-CH3
##'  \item k
##'  \item m,p-C6H4(CH3)2
##'  \item o-C6H4-(CH3)2
##'  \item pH
##'  \item precip_amount
##'  \item precip_amount_off
##'  \item trans-H3C-CH=CH-CH3
##'
##' }
##'
##' @title Import pre-calculated airbase statistics
##' @param statistic A \emph{single} choice from \dQuote{P50},
##' \dQuote{Mean}, \dQuote{P95}, \dQuote{P98}, \dQuote{Max},
##' \dQuote{Max36}, \dQuote{Max8}, \dQuote{Days.c.50.},
##' \dQuote{Max26}, \dQuote{Days.c.120.},\dQuote{SOMO35},
##' \dQuote{AOT40}, \dQuote{Max19}, \dQuote{Hours.c.200.},
##' \dQuote{Max4}, \dQuote{Days.c.125.}, \dQuote{P99_9},
##' \dQuote{Max25}, \dQuote{Hours.c.350.}.
##' @param pollutant The \emph{single} name of a pollutant to extract;
##' can be upper or lower case but must match one of those below.
##' @param avg.time The averaging time on which the measurements are
##' based. By default the averaging period with most data associated
##' with it is used. For most pollutants the averaging period will be
##' \dQuote{day}. Other common options are \dQuote{hour} and
##' \dQuote{week}.
##' @param code The airbase site code(s) to select. Can be upper or lower case.
##' @param site.type The type of site(s) to select.
##' @param year The year(s) to select e.g. \code{year = 2000:2012} to
##' select years 2010, 2011 and 2012.
##' @param data.cap The data capture threshold to use (\%). By default
##' all data are selected.
##' @param add Additional fields to add to the returned data frame. By
##' default the country, site type, latitude and logitude are
##' returned. Other useful options include \dQuote{city},
##' \dQuote{site} (site name), \dQuote{EMEP_station} and
##' \dQuote{altitude}.
##' @return A data frame of airbase sites with the statsitics chosen
##' and for all species.
##' @return Returns a data frame with POSIXct date, together with many
##' other airbase fields.
##' @export
##' @author David Carslaw
airbaseStats <- function(statistic = "Mean",
                         pollutant = "no2",
                         avg.time = "auto",
                         code = NULL,
                         site.type = c("background", "traffic", "industrial", "unknown"),
                         year = 1969:2012,
                         data.cap = 0,
                         add = c("country", "lat", "lon", "site.type", "site", "city")) {

    code <- lat <- lon <- site.info <- NULL

    ## checks to see if options exist
    stat.name <- match.arg(statistic, c("P50", "Mean", "P95", "P98", "Max", "Max36",
                                        "Max8", "Days.c.50.", "Max26", "Days.c.120.",
                                        "SOMO35", "AOT40", "Max19", "Hours.c.200.",
                                        "Max4", "Days.c.125.", "P99_9", "Max25",
                                        "Hours.c.350."))

    site.type <- sapply(toupper(site.type), function (x)
                        match.arg(x, toupper(c("background", "traffic", "industrial", "unknown"))))

    avg.time <- sapply(toupper(avg.time), function (x)
                        match.arg(x, toupper(c("auto", "2day", "2month", "2week", "3day",
                                               "3month", "4week", "day", "dymax", "hour",
                                               "month", "var", "week", "year"))))

    if (data.cap < 0 | data.cap > 100) {
        message("Data capture % should be between 0 and 100, setting to 90")
        data.cap <- 90
    }

    fileName <- paste0("http://www.erg.kcl.ac.uk/downloads/Policy_Reports/airbase/",
                       stat.name, ".RData")

    con <- url(fileName)
    load(con) ## brings in data frame dat
    close(con)

    ## pollutant
    dat <- dat[toupper(dat$component_caption) %in% toupper(pollutant) , ]

    ## site code
    if (!is.null(code)) {
        dat <- dat[dat$code %in% toupper(code), ]
    }

    ## data capture
    dat <- dat[dat$data.cap >= data.cap, ]

    ## average time basis
    if (avg.time == "AUTO") {## pick the most populus

        avg <- table(dat$statistics_average_group)
        id <- avg[which.max(avg)]

        dat <- dat[dat$statistics_average_group == names(id), ]
        message(paste0("Using averging time based on ", names(id)))

    } else { ## user selected averaging basis
        dat <- dat[dat$statistics_average_group == avg.time]
    }

    if (length(add) > 0 ) {

        ## add other fields
        fileName <- "http://www.erg.kcl.ac.uk/downloads/Policy_Reports/airbase/site.info.RData"

        con <- url(fileName)
        load(con) ## brings in data frame site.info
        close(con)

        site.info <- site.info[, c("code", add)] ## just the fields needed

        dat <- merge(dat, site.info, by = "code", all = TRUE)
        id <- which(is.na(dat$component_code)) ## remove missing component codes
        dat <- dat[-id, ]

    }
    
    ## site type
    dat <- dat[toupper(dat$site.type) %in% toupper(site.type) , ]

    ## year
    dat <- dat[dat$year %in% year, ]
    
    ## rename some fields
    id <- which(names(dat) == "value")
    names(dat)[id] <- pollutant

    id <- which(names(dat) == "component_caption")
    names(dat)[id] <- "pollutant"

    dat

}



