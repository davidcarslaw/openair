##' Import data from the UK Automatic Urban and Rural Network (AURN)
##'
##' Function for importing hourly mean UK Automatic Urban and Rural Network
##' (AURN) air quality archive data files for use with the \code{openair}
##' package. Files are imported from a remote server operated by AEA that
##' provides air quality data files as R data objects.
##'
##' The \code{importAURN} function has been written to make it easy to import
##' data from the UK AURN. AEA have provided .RData files (R workspaces) of all
##' individual sites and years for the AURN. These files are updated on a daily
##' basis. This approach requires a link to the Internet to work.
##'
##' For an up to date list of available sites that can be imported, see \code{importMeta}.
##'
##' There are several advantages over the web portal approach where .csv files
##' are downloaded. First, it is quick to select a range of sites, pollutants
##' and periods (see examples below). Second, storing the data as .RData
##' objects is very efficient as they are about four times smaller than .csv
##' files --- which means the data downloads quickly and saves bandwidth.
##' Third, the function completely avoids any need for data manipulation or
##' setting time formats, time zones etc. Finally, it is easy to import many
##' years of data beyond the current limit of about 64,000 lines. The final
##' point makes it possible to download several long time series in one go. The
##' function also has the advantage that the proper site name is imported and
##' used in \code{openair} functions.
##'
##' The site codes and pollutant names can be upper or lower case. The function
##' will issue a warning when data less than six months old is downloaded,
##' which may not be ratified.
##'
##' The data are imported by stacking sites on top of one another and will have
##' field names \code{site}, \code{code} (the site code) and \code{pollutant}.
##' Sometimes it is useful to have columns of site data. This can be done using
##' the \code{reshape} function --- see examples below.
##'
##' All units are expressed in mass terms for gaseous species (ug/m3
##' for NO, NO2, NOx (as NO2), SO2 and hydrocarbons; and mg/m3 for
##' CO). PM10 concentrations are provided in gravimetric units of
##' ug/m3 or scaled to be comparable with these units. Over the years
##' a variety of instruments have been used to measure particulate
##' matter and the technical issues of measuring PM10 are complex. In
##' recent years the measurements rely on FDMS (Filter Dynamics
##' Measurement System), which is able to measure the volatile
##' component of PM. In cases where the FDMS system is in use there
##' will be a separate volatile component recorded as 'v10' and
##' non-volatile component 'nv10', which is already included in the
##' absolute PM10 measurement. Prior to the use of FDMS the
##' measurements used TEOM (Tapered Element Oscillating. Microbalance)
##' and these concentrations have been multiplied by 1.3 to provide an
##' estimate of the total mass including the volatile fraction.
##'
##' The few BAM (Beta-Attenuation Monitor) instruments that have been
##' incorporated into the network throughout its history have been scaled by
##' 1.3 if they have a heated inlet (to account for loss of volatile particles)
##' and 0.83 if they do not have a heated inlet. The few TEOM instruments in
##' the network after 2008 have been scaled using VCM (Volatile Correction
##' Model) values to account for the loss of volatile particles. The object of
##' all these scaling processes is to provide a reasonable degree of comparison
##' between data sets and with the reference method and to produce a consistent
##' data record over the operational period of the network, however there may
##' be some discontinuity in the time series associated with instrument
##' changes.
##'
##' No corrections have been made to the PM2.5 data. The volatile component of
##' FDMS PM2.5 (where available) is shown in the 'v2.5' column.
##'
##'
##' @param site Site code of the AURN site to import e.g. "my1" is Marylebone
##'   Road. Several sites can be imported with \code{site = c("my1", "nott")}
##'   --- to import Marylebone Road and Nottingham for example.
##' @param year Year or years to import. To import a sequence of years from
##'   1990 to 2000 use \code{year = 1990:2000}. To import several specfic years
##'   use \code{year = c(1990, 1995, 2000)} for example.
##' @param pollutant Pollutants to import. If omitted will import all
##'   pollutants ffrom a site. To import only NOx and NO2 for example use
##'   \code{pollutant = c("nox", "no2")}.
##' @param hc A few sites have hydrocarbon measurements available and setting
##'   \code{hc = TRUE} will ensure hydrocarbon data are imported. The default
##'   is however not to as most users will not be interested in using
##'   hydrocarbon data and the resulting data frames are considerably larger.
##' @param meta Should meta data be returned? If \code{TRUE} the site type, latitude and longitude are returned.
##' @param verbose Should the function give messages when downloading files?
##'   Default is \code{FALSE}.
##'
##' @export
##' @importFrom utils download.file
##' @return Returns a data frame of hourly mean values with date in POSIXct
##'   class and time zone GMT.
##' @author David Carslaw
##' @seealso \code{\link{importKCL}}, \code{\link{importADMS}},
##'   \code{\link{importSAQN}}
##' @keywords methods
##' @examples
##'
##'
##' ## import all pollutants from Marylebone Rd from 1990:2009
##' \dontrun{mary <- importAURN(site = "my1", year = 2000:2009)}
##'
##' ## import nox, no2, o3 from Marylebone Road and Nottingham Centre for 2000
##' \dontrun{thedata <- importAURN(site = c("my1", "nott"), year = 2000,
##' pollutant = c("nox", "no2", "o3"))}
##'
##' ## import over 20 years of Mace Head O3 data!
##' \dontrun{o3 <- importAURN(site = "mh", year = 1987:2009)}
##'
##' ## import hydrocarbon (and other) data from Marylebone Road
##' \dontrun{mary <- importAURN(site = "my1", year =1998, hc = TRUE)}
##'
##' ## reshape the data so that each column represents a pollutant/site
##' \dontrun{
##' require(reshape2)
##' thedata <- importAURN(site = c("nott", "kc1"), year = 2008,
##' pollutant = "o3")
##' thedata <- melt(thedata, measure.vars = "o3")
##' thedata <- dcast(thedata, ... ~ variable + site + code)
##' ## thedata now has columns  o3_Nottingham Centre_NOTT o3_London N. Kensington_KC1
##'
##' }
##'
##'
importAURN <- function(site = "my1", year = 2009, pollutant = "all",
                       hc = FALSE, meta = FALSE,
                       verbose = FALSE) {

  # For file name matching, needs to be exact
  site <- toupper(site)

  # Create file name vector
  files <- lapply(site, function(x) paste(x, "_", year, sep = ""))
  files <- do.call(c, files)

  # Donload and load data
  thedata <- plyr::ldply(files, loadData, verbose)

  # Return if no data
  if (nrow(thedata) == 0) return() ## no data

  ## suppress warnings for now - unequal factors, harmless

  if (is.null(thedata)) {
    stop("No data to import - check site codes and year.", call. = FALSE)
  }

  thedata$site <- factor(thedata$site, levels = unique(thedata$site))

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
      "date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
      "v10", "v2.5", "nv10", "nv2.5", "ws", "wd", "code", "site"
    )

    thedata <- thedata[, which(names(thedata) %in% theNames)]
  }

  ## if particular pollutants have been selected
  if (pollutant != "all") {
    thedata <- thedata[, c("date", pollutant, "site", "code")]
  }


  ## warning about recent, possibly unratified data
  timeDiff <- difftime(Sys.time(), max(thedata$date), units = "days")
  if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go", call. = FALSE)
  }

  ## make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"

  # make sure class is correct for lubridate
  class(thedata$date) <- c("POSIXct", "POSIXt")

  if (meta) {
    meta <- importMeta(source = "aurn")
    # suppress warnings about factors
    thedata <- suppressWarnings(inner_join(thedata, meta, by = c("code", "site")))
  }

  thedata
}



# Define downloading and loading function
# No export
loadData <- function(x, verbose) {
  tryCatch({

    # Download file to temp directory
    # need to do this because of https, certificate problems
    tmp <- tempfile()

    # Build the file name
    fileName <- paste(
      "https://uk-air.defra.gov.uk/openair/R_data/", x,
      ".RData",
      sep = ""
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

    # Reasign
    dat <- get(x)

    return(dat)
  }, error = function(ex) {

    # Print a message
    if (verbose) {
      message(x, "does not exist - ignoring that one.")
    }
  })
}
