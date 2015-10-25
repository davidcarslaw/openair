##' Get information about airbase sites and instruments
##'
##' This function compiles key information about airbase sites
##' including the site type, species measured and the instruments and
##' techniques used. By default the function returns one row per site
##' with basic information concerning the site e.g. its type, latitude
##' and longitude.
##'
##' More comprehensive information is returned if \code{instrument =
##' TRUE}. In this case the species measured at the site together with
##' the instruments used will be returned.
##'
##' @title Get information about airbase sites and instruments
##' @param code Site code(s) of the sites to be imported. Can be upper or lower case.
##' @param instrument Should species/instrument details also be
##' returned. When \code{FALSE} only one row per site is returned with
##' other information such as site type, latitude and logitude. When
##' \code{TRUE} details of the individual species and instruments used
##' is also returned.
##' @export
##' @return Returns a data frame containing key information about airbase site(s).
##' @seealso \code{\link{importAirbase}},
##' \code{\link{airbaseFindCode}}, \code{\link{airbaseStats}}
##' @author David Carslaw
airbaseInfo <- function(code = "gb0620a", instrument = FALSE) {

    ## get rid of R check annoyances
    meas.config <- NULL

    code <- toupper(code)

    load(url("http://www.erg.kcl.ac.uk/downloads/Policy_Reports/airbase/meas.config.RData"))

    dat <- meas.config[meas.config$code %in% code, ]

    if (instrument) {
        dat
    } else {
        id <- which(!duplicated(dat$code))
        dat <- dat[id, c("code", "site", "country", "city", "site.type", "lat", "lon", "altitude")]
        dat
    }
    
}
