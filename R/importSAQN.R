##' Scottish Air Quality Network data import for openair
##'
##' Function for importing hourly mean Scottish Air Quality Network (SAQN)
##' archive data files for use with the \code{openair} package. Files are
##' imported from a remote server operated by AEA that provides air quality
##' data files as R data objects.
##'
##' The \code{importSAQN} function has been written to make it easy to import
##' data from the Scottish Air Quality Network (SAQN) ---
##' \url{http://www.scottishairquality.co.uk/index.php}. AEA have provided
##' .RData files (R workspaces) of all individual sites and years for the SAQN.
##' These files are updated on a daily basis. This approach requires a link to
##' the Internet to work.
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
##' All units are expressed in mass terms for gaseous species (ug/m3 for NO,
##' NO2, NOx (as NO2), SO2; and mg/m3 for CO). PM10 concentrations are provided
##' in gravimetric units of ug/m3 or scaled to be comparable with these units.
##' Over the years a variety of instruments have been used to measure
##' particulate matter and the technical issues of measuring PM10 are complex.
##' In recent years the measurements rely on FDMS (Filter Dynamics Measurement
##' System), which is able to measure the volatile component of PM. In cases
##' where the FDMS system is in use there will be a separate volatile component
##' recorded as 'v10', which is already included in the absolute PM10
##' measurement. Prior to the use of FDMS the measurements used TEOM (Tapered
##' Element Oscillating. Microbalance) and these concentrations have been
##' multiplied by 1.3 to provide an estimate of the total mass including the
##' volatile fraction.
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
##' No corrections have been made to teh PM2.5 data. The volatile component of
##' FDMS PM2.5 (where available) is shown in the 'v2.5' column.
##'
##' While the function is being developed, the following site codes should help
##' with selection. \itemize{ \item | ABD | Aberdeen Errol Place | URBAN
##' BACKGROUND | \item | ABD0 | Aberdeen Market Street 2 | ROADSIDE | \item |
##' ABD1 | Aberdeen Anderson Dr | ROADSIDE | \item | ABD2 | Aberdeen Market St
##' | ROADSIDE | \item | ABD3 | Aberdeen Union Street Roadside | ROADSIDE |
##' \item | ABD8 | Aberdeen Wellington Road | ROADSIDE | \item | ACTH |
##' Auchencorth Moss | RURAL | \item | AD1 | Aberdeen King Street | ROADSIDE |
##' \item | ALOA | Alloa | ROADSIDE | \item | AYR | South Ayrshire Ayr High St
##' | ROADSIDE | \item | BRX | West Lothian Broxburn | ROADSIDE | \item | BUSH
##' | Bush Estate | RURAL | \item | CUMN | East Ayrshire New Cumnock | URBAN
##' BACKGROUND | \item | CUPA | Fife Cupar | ROADSIDE | \item | DUMF | Dumfries
##' | ROADSIDE | \item | DUN1 | Dundee Mains Loan | URBAN BACKGROUND | \item |
##' DUN3 | Dundee Union Street | URBAN CENTRE | \item | DUN4 | Dundee Broughty
##' Ferry Road | URBAN INDUSTRIAL | \item | DUN5 | Dundee Seagate | KERBSIDE |
##' \item | DUN6 | Dundee Lochee Road | KERBSIDE | \item | DUN7 | Dundee
##' Whitehall Street | ROADSIDE | \item | DUNF | Fife Dunfermline | ROADSIDE |
##' \item | ED | Edinburgh Centre | URBAN CENTRE | \item | ED1 | Edinburgh St
##' John's Road | KERBSIDE | \item | ED2 | Edinburgh Med. Sch. | URBAN
##' BACKGROUND | \item | ED3 | Edinburgh St Leonards | URBAN BACKGROUND | \item
##' | ED4 | Edinburgh Roseburn | ROADSIDE | \item | ED5 | Edinburgh Gorgie Road
##' | ROADSIDE | \item | ED6 | Edinburgh Haymarket | ROADSIDE | \item | ED7 |
##' Edinburgh Queen Street | ROADSIDE | \item | ED8 | Edinburgh Salamander St |
##' ROADSIDE | \item | EDB1 | East Dunbartonshire Bishopbriggs | ROADSIDE |
##' \item | EDB2 | East Dunbartonshire Bearsden | ROADSIDE | \item | EDB3 |
##' East Dunbartonshire Kirkintilloch | ROADSIDE | \item | EK | East Kilbride |
##' SUBURBAN | \item | EK0 | South Lanarkshire East Kilbride | ROADSIDE | \item
##' | ESK | Eskdalemuir | RURAL | \item | FAL2 | Falkirk Park St | ROADSIDE |
##' \item | FAL3 | Falkirk Hope St | ROADSIDE | \item | FAL5 | Falkirk Haggs |
##' ROADSIDE | \item | FAL6 | Falkirk West Bridge Street | ROADSIDE | \item |
##' FALK | Falkirk Grangemouth MC | URBAN BACKGROUND | \item | FFAR | Angus
##' Forfar | ROADSIDE | \item | FINI | East Ayrshire Kilmarnock John Finnie St
##' | ROADSIDE | \item | FW | Fort William | SUBURBAN | \item | GL1 | Glasgow
##' Abercromby Street | ROADSIDE | \item | GL2 | Glasgow Nithsdale Road |
##' ROADSIDE | \item | GL3 | Glasgow Broomhill | ROADSIDE | \item | GLA |
##' Glasgow City Chambers | URBAN BACKGROUND | \item | GLA3 | Glasgow Centre |
##' URBAN CENTRE | \item | GLA4 | Glasgow Kerbside | KERBSIDE | \item | GLA5 |
##' Glasgow Anderston | URBAN BACKGROUND | \item | GLA6 | Glasgow Byres Road |
##' ROADSIDE | \item | GLA7 | Glasgow Waulkmillglen Reservoir | RURAL | \item |
##' GLA8 | Glasgow Battlefield Road | ROADSIDE | \item | GLAS | Glasgow Hope St
##' | KERBSIDE | \item | GRA2 | Grangemouth Moray | URBAN BACKGROUND | \item |
##' GRAN | Grangemouth | URBAN INDUSTRIAL | \item | INV2 | Inverness | ROADSIDE
##' | \item | IRV | North Ayrshire Irvine High St | KERBSIDE | \item | KILM |
##' East Ayrshire Kilmarnock | URBAN BACKGROUND | \item | KINC | Fife
##' Kincardine | ROADSIDE | \item | KIR | Fife Kirkcaldy | ROADSIDE | \item |
##' LER3 | Lerwick Staney Hill | URBAN BACKGROUND | \item | LERW | Lerwick |
##' RURAL | \item | LING | West Lothian Linlithgow High Street | ROADSIDE |
##' \item | MID1 | Midlothian Pathhead | KERBSIDE | \item | MID2 | Midlothian
##' Dalkeith | ROADSIDE | \item | MUSS | East Lothian Musselburgh N High St |
##' ROADSIDE | \item | NL1 | N Lanarkshire Coatbridge Whifflet | URBAN
##' BACKGROUND | \item | NL2 | N Lanarkshire Coatbridge Ellis St | ROADSIDE |
##' \item | NL3 | N Lanarkshire Chapelhall | ROADSIDE | \item | NL4 | N
##' Lanarkshire Croy | ROADSIDE | \item | NL5 | N Lanarkshire Harthill |
##' ROADSIDE | \item | NL6 | N Lanarkshire Motherwell | ROADSIDE | \item | NL7
##' | N Lanarkshire Shawhead Coatbridge | ROADSIDE | \item | NL8 | N
##' Lanarkshire Harthill West | URBAN BACKGROUND | \item | NL9 | N Lanarkshire
##' Moodiesburn | ROADSIDE | \item | PAI2 | Paisley Glasgow Airport | AIRPORT |
##' \item | PAI3 | Paisley Gordon Street | ROADSIDE | \item | PAIS | Paisley
##' Central Road | ROADSIDE | \item | PEEB | Peebles | SUBURBAN | \item | PET1
##' | Perth Crieff | ROADSIDE | \item | PET2 | Perth Atholl Street | ROADSIDE |
##' \item | PETH | Perth High Street | ROADSIDE | \item | ROSY | Fife Rosyth |
##' ROADSIDE | \item | SHED | East Renfrewshire Sheddens | ROADSIDE | \item |
##' STRL | Stirling Craig's Roundabout | ROADSIDE | \item | SV | Strath Vaich |
##' RURAL | \item | TARB | South Ayrshire Tarbolton | ROADSIDE | \item | UPH |
##' West Lothian Uphall | ROADSIDE | \item | WDB1 | West Dunbartonshire John
##' Knox St | AIRPORT | \item | WDB2 | West Dunbartonshire Balloch | ROADSIDE |
##' \item | WDB3 | West Dunbartonshire Clydebank | ROADSIDE | \item | WDB4 |
##' West Dunbartonshire Glasgow Road | ROADSIDE | \item | WHIT | West Lothian
##' Whitburn | URBAN BACKGROUND | }
##'
##' @param site Site code of the SAQN site to import e.g. "gla4" is Glasgow
##'   Kerbside. Several sites can be imported with \code{site = c("gla4",
##'   "ed")} --- to import Glasgow Kerbside and Edinbrugh Centre for example.
##' @param year Year or years to import. To import a sequence of years from
##'   1990 to 2000 use \code{year = 1990:2000}. To import several specfic years
##'   use \code{year = c(1990, 1995, 2000)} for example.
##' @param pollutant Pollutants to import. If omitted will import all
##'   pollutants ffrom a site. To import only NOx and NO2 for example use
##'   \code{pollutant = c("nox", "no2")}.
##' @return Returns a data frame of hourly mean values with date in POSIXct
##'   class and time zone GMT.
##' @author David Carslaw and Trevor Davies (AEA)
##' @seealso See \code{\link{importAURN}} for data elsewhere in the UK and
##'   \code{\link{importKCL}} for importing comprehensive data in and around
##'   London.
##' @keywords methods
##' @export
##' @examples
##'
##'
##' ## import all pollutants from Glasgow Roadside
##' \dontrun{glas <- importSAQN(site = "gla4", year = 2000:2009)}
##'
##' ## import all pollutants from Lerwick rural site (O3)
##' \dontrun{ler <- importSAQN(site = "lerw", year = 2005:2010)}
##'
##' ## import all pollutants from Glasgow/Dundee Centre for 2009
##' \dontrun{all <- importSAQN(site = c("gla3", "dun3"), year = 2009)}
##'
##'
importSAQN <- function(site = "gla4", year = 2009, pollutant = "all") {
    site <- toupper(site)


    files <- lapply(site, function (x) paste(x, "_", year, sep = ""))

    files <- do.call(c, files)


    loadData <- function(x) {
        tryCatch({
            fileName <- paste("http://www.scottishairquality.co.uk/openair/R_data/", x, ".RData", sep = "")
            con <- url(fileName)
            load(con, envir = .GlobalEnv)
            close(con)
            x
        },
                 error = function(ex) {cat(x, "does not exist - ignoring that one.\n")})

    }

    thedata <- lapply(files, loadData)

    theObjs <- unlist(thedata)
    ## note unlist will drop NULLs from non-existant sites/years
    mylist <- lapply(theObjs, get)

    thedata <- do.call(bind_rows, mylist)
    if (is.null(thedata)) stop("No data to import - check site codes and year.", call. = FALSE)

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
    theNames <- c("date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
                  "v10", "v2.5", "ws", "code", "site")

    thedata <- thedata[,  which(names(thedata) %in% theNames)]


    ## if particular pollutants have been selected
    if (!missing(pollutant)) thedata <- thedata[, c("date", pollutant, "site", "code")]

    rm(list = theObjs, pos = 1)

    ## warning about recent, possibly unratified data
    timeDiff <- difftime(Sys.time(),  max(thedata$date), units='days')
    if (timeDiff < 180) {
        warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")}

    ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"

    thedata
}
