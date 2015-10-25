##' Function to simplify and combine airbase data imports
##'
##' Airbase keeps a detailed account of the instruments used for
##' measuring different species. For the same species several
##' different instruments can be used over time. For example, for NOx
##' a broken instrument could be replaced by a new one. In airbase
##' these two instruments will results in two separate time series
##' identified through the airbase measurement_european_group_code
##' (with numbers like 100, 101, 102). When data are imported using
##' the \code{importAirbase} function the different measurement codes
##' are retained in the pollutant names if more than one code is used,
##' resulting in names like NOX|101, NOX|102 and so on.
##'
##' For \sQuote{simple} species such as NOx there is generally no
##' reason to keep two separate time series and it would seem
##' reasonable to combine them into a single species NOX. This is what
##' the \code{airbaseSplice} function does. It will combine multiple
##' measurements of the same species into a new single species. In
##' cases where there is overlap, more recent instrument measurements
##' take precedence over older instrument measurements.
##'
##' In other cases it can be sensible to keep the different
##' measurements of the same species separate. An example of such a
##' case is for PM10 where, for example, moving from TEOM to FDMS
##' measurements produces time series that cannot (or should not) be
##' easily be merged.
##'
##' Rather than combine all pollutant measurements of the same species
##' by default it was thought important to retain this information to
##' allow users more flexibility.
##'
##' If \code{drop = TRUE} then the original columns are not
##' retained. For example, given original columns NOX|101, NOX|102 the
##' new data frame will have only one column NOX. Conversely, if
##' \code{drop = FALSE} then the final data frame will have three
##' species: NOX|101, NOX|102 and a new combined field NOX.
##' 
##' @title Function to simplify and combine airbase data imports
##' @param dat Data (a data frame) that has been imported using the
##' \code{importAirbase} function.
##' @param drop Should the original columns be dropped or kept. The
##' default is to remove (drop) them.
##' @export
##' @return A simplified data frame.
##' @author David Carslaw
airbaseSplice <- function(dat, drop = TRUE) {
    ## strip out pollutant name
    poll.nm <- names(dat)
    poll.nm <- lapply(poll.nm, function (x) strsplit(x, split = "\\|")[[1]][1])
    poll.nm <- do.call(c, poll.nm)

    all.ids <- NULL ## ids of columns to remove if drop = TRUE
    ## if pollutant name repeats then combine
    for (j in seq_along(poll.nm)) {

        ## means that there is more than one instrument and need to combine
        if (length(which(poll.nm == poll.nm[j])) != 1) {
            
            id <- which(poll.nm == poll.nm[j])

            ## if no data, ignore
            id2 <- sapply(id, function (x) all(is.na(dat[, x])))
            id <- id[which(id2 == FALSE)]
            
            new.nm <- poll.nm[id[1]] ## just use first for new name

            ## will combine by adding latest measurements last
            ## do this in case of overlap
            res <- sapply(id, function (x) max(na.omit(dat[, c(1, x)])$date))
            res <- data.frame(max.date = res, id = id)

            ## sort lowest to highest
            res <- sortDataFrame(res, key = "max.date")

            for (k in 1:nrow(res)) {
                if (k == 1) {
                    dat[ , new.nm] <- dat[, res$id[k]] ## make new variable
                } else {
                    ids <- which(!is.na(dat[, res$id[k]]))
                    dat[ids, new.nm] <- dat[ids, res$id[k]]
                }               
            }
            
            all.ids <- c(all.ids, id)
        }           
    }

    if (drop & length(all.ids) > 0) dat <- dat[, -all.ids] ## remove original columns
    dat  
}
