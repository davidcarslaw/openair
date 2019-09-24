##' Welsh Air Quality Network data import for openair
##'
##' Function for importing hourly mean Welsh Air Quality Network (WAQN)
##' archive data files for use with the \code{openair} package. Files are
##' imported from a remote server operated by Ricardo that provides air quality data
##' files as R data objects.
##'
##' The \code{importWAQN} function has been written to make it easy to import
##' data from the Welsh Air Quality Network (WAQN) ---
##' \url{https://airquality.gov.wales/}. Ricardo have provided
##' .RData files (R workspaces) of all individual sites and years for the WAQN.
##' These files are updated on a daily basis. This approach requires a link to
##' the Internet to work.
##'
##' For a list of up to date site codes and site information, see
##' \code{\link{importMeta}} and in particular \code{importMeta(source =
##' "waqn")}.
##'
##' The site codes and pollutant names can be upper or lower case. The function
##' will issue a warning when data less than six months old is downloaded, which
##' may not be ratified.
##'
##' The function also returns wind speed (ws) and direction (wd) for more recent
##' years derived from WRF (regional meteorological model).
##'
##' @param site Site code of the WAQN site to import e.g. "card" is Cardiff Centre. 
##' Several sites can be imported with \code{site = c("card", "cae6")}
##'   --- to import Cardiff Centre and Hafod-yr-ynys Roadside for example.
##' @param year Year or years to import. To import a sequence of years from 1990
##'   to 2000 use \code{year = 1990:2000}. To import several specfic years use
##'   \code{year = c(1990, 1995, 2000)} for example.
##' @param pollutant Pollutants to import. If omitted will import all pollutants
##'   from a site. To import only NOx and NO2 for example use \code{pollutant =
##'   c("nox", "no2")}.
##' @param meta Should meta data be returned? If \code{TRUE} the site type,
##'   latitude and longitude are returned.
##' @param to_narrow By default the returned data has a column for each
##'   pollutant/variable. When \code{to_narrow = TRUE} the data are stacked into
##'   a narrow format with a column identifying the pollutant name.  
##' @return Returns a data frame of hourly mean values with date in POSIXct
##'   class and time zone GMT.
##' @author David Carslaw and Trevor Davies 
##' @seealso See \code{\link{importAURN}} for data elsewhere in the UK and
##'   \code{\link{importKCL}} for importing comprehensive data in and around
##'   London.
##' @keywords methods
##' @export
##' @examples
##'
##' ## see what sites are available
##' \dontrun{meta <- importMeta("waqn")}
##'
##' ## import all pollutants from Cardiff Centre
##' \dontrun{cardiff <- importWAQN(site = "card", year = 2010:2018)}
##'
##' ## import all pollutants from two sites for 2018
##' \dontrun{all <- importWAQN(site = site = c("card", "cae6"), year = 2018)}
##'
##' 
importWAQN <- function(site = "card", year = 2018, pollutant = "all", 
                       meta = FALSE,
                       to_narrow = FALSE) {
  site <- toupper(site)


  files <- lapply(site, function(x) paste(x, "_", year, sep = ""))

  files <- do.call(c, files)


  loadData <- function(x) {
    tryCatch({
      fileName <- paste("https://airquality.gov.wales/sites/default/files/openair/R_data/", x, ".RData", sep = "")
      con <- url(fileName)
      load(con, envir = .GlobalEnv)
      x
    },
    error = function(ex) {
      cat(x, "does not exist - ignoring that one.\n")
    },
    finally = {
        close(con)
    }
    )
  }

  thedata <- lapply(files, loadData)

  # Return if no data
  if (length(thedata) == 0) return() ## no data

  theObjs <- unlist(thedata)
  ## note unlist will drop NULLs from non-existant sites/years
  mylist <- lapply(theObjs, get)

  thedata <- suppressWarnings(do.call(bind_rows, mylist))
  if (is.null(thedata) || nrow(thedata) == 0) stop("No data to import - check site codes and year.", call. = FALSE)

  thedata$site <- factor(thedata$site, levels = unique(thedata$site))

  ## change names
  names(thedata) <- tolower(names(thedata))

  ## change nox as no2
  id <- which(names(thedata) %in% "noxasno2")
  if (length(id) == 1) names(thedata)[id] <- "nox"

  ## set class to integer for post-processing convenience
  if ("nox" %in% names(thedata)) class(thedata$nox) <- "integer"
  if ("no" %in% names(thedata)) class(thedata$no) <- "integer"
  if ("no2" %in% names(thedata)) class(thedata$no2) <- "integer"
  if ("o3" %in% names(thedata)) class(thedata$o3) <- "integer"
  if ("so2" %in% names(thedata)) class(thedata$so2) <- "integer"
  if ("pm10" %in% names(thedata)) class(thedata$pm10) <- "integer"
  if ("pm2.5" %in% names(thedata)) class(thedata$pm2.5) <- "integer"
  if ("v10" %in% names(thedata)) class(thedata$v10) <- "integer"
  if ("v2.5" %in% names(thedata)) class(thedata$v2.5) <- "integer"


  ## should hydrocarbons be imported?

  ## no hydrocarbons - therefore select conventional pollutants
  theNames <- c(
    "date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
    "v10", "v2.5", "ws", "wd", "code", "site"
  )

  thedata <- thedata[, which(names(thedata) %in% theNames)]


  ## if particular pollutants have been selected
  if (!missing(pollutant) && pollutant != "all") {
    thedata <- thedata[, c("date", pollutant, "site", "code")]
  }

  rm(list = theObjs, pos = 1)

  ## warning about recent, possibly unratified data
  timeDiff <- difftime(Sys.time(), max(thedata$date), units = "days")
  if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")
  }

  ## make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"
  
  if (meta) {
    meta_data <- importMeta(source = "waqn")
    # suppress warnings about factors
    thedata <- suppressWarnings(inner_join(thedata, meta_data, by = c("code", "site")))
  }
  
  if (to_narrow) {
    
    if (meta) {
      
      thedata <- pivot_longer(thedata, -c(date, site, code, latitude, longitude, site.type), 
                              names_to = "pollutant") %>% 
        arrange(site, code, pollutant, date)
      
    } else {
      
      thedata <- pivot_longer(thedata, -c(date, site, code), names_to = "pollutant") %>% 
        arrange(site, code, pollutant, date)
      
    }
  }

  as_tibble(thedata)
}
