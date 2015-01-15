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
##' No corrections have been made to teh PM2.5 data. The volatile component of
##' FDMS PM2.5 (where available) is shown in the 'v2.5' column.
##'
##' While the function is being developed, the following site codes should help
##' with selection.
##'
##' \itemize{
##'\item A3 | London A3 Roadside | Urban traffic
##'  \item ABD | Aberdeen | Urban Background
##'  \item ABD7 | Aberdeen Union Street Roadside | Urban traffic
##'  \item ACTH | Auchencorth Moss | Rural Background
##'  \item AH | Aston Hill | Rural Background
##'  \item ARM6 | Armagh Roadside | Urban traffic
##'  \item BALM | Ballymena | Urban Background
##'  \item BAR2 | Barnsley 12 | Urban Background
##'  \item BAR3 | Barnsley Gawber | Urban Background
##'  \item BARN | Barnsley | Urban Background
##'  \item BATH | Bath Roadside | Urban traffic
##'  \item BEL | Belfast East | Urban Background
##'  \item BEL2 | Belfast Centre | Urban Background
##'  \item BEL4 | Belfast Clara St | Suburban Background
##'  \item BEX | London Bexley | Suburban Background
##'  \item BHAM | Birmingham Kerbside | Urban traffic
##'  \item BIL | Billingham | Urban Industrial
##'  \item BIR | Bircotes | Urban Background
##'  \item BIR1 | Birmingham Tyburn | Urban Background
##'  \item BIR2 | Birmingham East | Urban Background
##'  \item BIRM | Birmingham Centre | Urban Background
##'  \item BIRT | Birmingham Tyburn Roadside | Urban traffic
##'  \item BLAC | Blackpool | Urban Background
##'  \item BLC2 | Blackpool Marton | Urban Background
##'  \item BLCB | Blackburn Darwen Roadside | Urban traffic
##'  \item BOLT | Bolton | Urban Background
##'  \item BORN | Bournemouth | Urban Background
##'  \item BOT | Bottesford | Rural Background
##'  \item BRAD | Bradford Centre | Urban Background
##'  \item BREN | London Brent | Urban Background
##'  \item BRI | London Bridge Place | Urban Background
##'  \item BRIS | Bristol Centre | Urban Background
##'  \item BRIT | Brighton Roadside | Urban traffic
##'  \item BRN | Brentford Roadside | Urban traffic
##'  \item BRS2 | Bristol Old Market | Urban traffic
##'  \item BRS8 | Bristol St Paul's | Urban Background
##'  \item BRT3 | Brighton Preston Park | Urban Background
##'  \item BURY | Bury Roadside | Urban traffic
##'  \item BUSH | Bush Estate | Rural Background
##'  \item BY1 | Bromley Roadside | Urban traffic
##'  \item BY2 | London Bromley | Urban traffic
##'  \item CA1 | Camden Kerbside | Urban traffic
##'  \item CAM | Cambridge Roadside | Urban traffic
##'  \item CAMB | Cambridge | Urban traffic
##'  \item CAN | London Canvey | Urban Industrial
##'  \item CANT | Canterbury | Urban Background
##'  \item CAR | Cardiff Kerbside | Urban traffic
##'  \item CARD | Cardiff Centre | Urban Background
##'  \item CARL | Carlisle Roadside | Urban traffic
##'  \item CHAT | Chatham Roadside | Urban traffic
##'  \item CHIL | Chilworth | Suburban Background
##'  \item CHP | Chepstow A48 | Urban traffic
##'  \item CHS6 | Chesterfield | Urban Background
##'  \item CHS7 | Chesterfield Roadside | Urban traffic
##'  \item CLL | Central London | Urban Background
##'  \item CLL2 | London Bloomsbury | Urban Background
##'  \item COV2 | Coventry Centre | Urban Background
##'  \item COV3 | Coventry Memorial Park | Urban Background
##'  \item CRD | London Cromwell Road | Urban traffic
##'  \item CRD2 | London Cromwell Road 2 | Urban traffic
##'  \item CWMB | Cwmbran | Urban Background
##'  \item DERY | Derry | Urban Background
##'  \item DUMB | Dumbarton Roadside | Urban traffic
##'  \item DUMF | Dumfries | Urban traffic
##'  \item EAGL | Stockton-on-Tees Eaglescliffe | Urban traffic
##'  \item EB | Eastbourne | Urban Background
##'  \item ECCL | Salford Eccles | Urban Industrial
##'  \item ED | Edinburgh Centre | Urban Background
##'  \item ED3 | Edinburgh St Leonards | Urban Background
##'  \item EK | East Kilbride | Suburban Background
##'  \item ESK | Eskdalemuir | Rural Background
##'  \item EX | Exeter Roadside | Urban traffic
##'  \item FEA | Featherstone | Urban Background
##'  \item FW | Fort William | Suburban Background
##'  \item GDF | Great Dun Fell | Rural Background
##'  \item GLA | Glasgow City Chambers | Urban Background
##'  \item GLA3 | Glasgow Centre | Urban Background
##'  \item GLA4 | Glasgow Kerbside | Urban traffic
##'  \item GLAS | Glasgow Hope St | Urban traffic
##'  \item GLAZ | Glazebury | Rural Background
##'  \item GRA2 | Grangemouth Moray | Rural Background
##'  \item GRAN | Grangemouth | Urban Industrial
##'  \item HAR | Harwell | Rural Background
##'  \item HARR | London Harrow | Suburban Background
##'  \item HG1 | Haringey Roadside | Urban traffic
##'  \item HG2 | London Haringey | Urban Background
##'  \item HIL | London Hillingdon | Urban Background
##'  \item HK4 | London Hackney | Urban Background
##'  \item HM | High Muffles | Rural Background
##'  \item HOPE | Stanford-le-Hope Roadside | Urban traffic
##'  \item HORE | Horley | Suburban Background
##'  \item HORS | London Westminster | Urban Background
##'  \item HOVE | Hove Roadside | Urban traffic
##'  \item HR3 | London Harrow Stanmore | Urban Background
##'  \item HRL | London Harlington | Urban Industrial
##'  \item HS1 | Hounslow Roadside | Urban traffic
##'  \item HUL2 | Hull Freetown | Urban Background
##'  \item HULL | Hull Centre | Urban Background
##'  \item INV2 | Inverness | Urban traffic
##'  \item ISL | London Islington | Urban Background
##'  \item KC1 | London N. Kensington | Urban Background
##'  \item LB | Ladybower | Rural Background
##'  \item LDS | Leeds Potternewton | Urban Background
##'  \item LEAM | Leamington Spa | Urban Background
##'  \item LED6 | Leeds Headingley Kerbside | Urban traffic
##'  \item LEED | Leeds Centre | Urban Background
##'  \item LEIC | Leicester Centre | Urban Background
##'  \item LEOM | Leominster | Suburban Background
##'  \item LERW | Lerwick | Suburban Background
##'  \item LH | Lullington Heath | Rural Background
##'  \item LINC | Lincoln Roadside | Urban traffic
##'  \item LIVR | Liverpool Centre | Urban Background
##'  \item LN | Lough Navar | Rural Background
##'  \item LON6 | London Eltham | Suburban Background
##'  \item LON7 | London Eltham (HC) | Urban Background
##'  \item LV6 | Liverpool Queen's Drive Roadside | Urban traffic
##'  \item LVP | Liverpool Speke | Urban Background
##'  \item LW1 | London Lewisham | Urban Background
##'  \item MACK | Charlton Mackrell | Rural Background
##'  \item MAN | Manchester Town Hall | Urban Background
##'  \item MAN3 | Manchester Piccadilly | Urban Background
##'  \item MAN4 | Manchester South | Suburban Background
##'  \item MH | Mace Head | Rural Background
##'  \item MID | Middlesbrough | Urban Background
##'  \item MKTH | Market Harborough | Rural Background
##'  \item MOLD | Mold | Suburban Background
##'  \item MY1 | London Marylebone Road | Urban traffic
##'  \item NCA3 | Newcastle Cradlewell Roadside | Urban traffic
##'  \item NEWC | Newcastle Centre | Urban Background
##'  \item NO10 | Norwich Forum Roadside | Urban traffic
##'  \item NO12 | Norwich Lakenfields | Urban Background
##'  \item NOR1 | Norwich Roadside | Urban traffic
##'  \item NOR2 | Norwich Centre | Urban Background
##'  \item NOTT | Nottingham Centre | Urban Background
##'  \item NPT3 | Newport | Urban Background
##'  \item NTON | Northampton | Urban Background
##'  \item OLDB | Sandwell Oldbury | Urban Background
##'  \item OSY | St Osyth | Rural Background
##'  \item OX | Oxford Centre Roadside | Urban traffic
##'  \item OX8 | Oxford St Ebbes | Urban Background
##'  \item PEEB | Peebles | Urban Background
##'  \item PEMB | Narberth | Rural Background
##'  \item PLYM | Plymouth Centre | Urban Background
##'  \item PMTH | Portsmouth | Urban Background
##'  \item PRES | Preston | Urban Background
##'  \item PT | Port Talbot | Urban Industrial
##'  \item PT4 | Port Talbot Margam | Urban Industrial
##'  \item REA1 | Reading New Town | Urban Background
##'  \item READ | Reading | Urban Background
##'  \item REDC | Redcar | Suburban Background
##'  \item ROCH | Rochester Stoke | Rural Background
##'  \item ROTH | Rotherham Centre | Urban Background
##'  \item RUGE | Rugeley | Urban Background
##'  \item SALT | Saltash Roadside | Urban traffic
##'  \item SCN2 | Scunthorpe Town | Urban Industrial
##'  \item SCUN | Scunthorpe | Urban Industrial
##'  \item SDY | Sandy Roadside | Urban traffic
##'  \item SEND | Southend-on-Sea | Urban Background
##'  \item SHE | Sheffield Tinsley | Urban Background
##'  \item SHE2 | Sheffield Centre | Urban Background
##'  \item SIB | Sibton | Rural Background
##'  \item SK1 | London Southwark | Urban Background
##'  \item SK2 | Southwark Roadside | Urban traffic
##'  \item SK5 | Southwark A2 Old Kent Road | Urban traffic
##'  \item SOM | Somerton | Rural Background
##'  \item SOUT | Southampton Centre | Urban Background
##'  \item STE | Stevenage | Suburban Background
##'  \item STEW | Stewartby | Urban Industrial
##'  \item STK4 | Stockport Shaw Heath | Urban Background
##'  \item STOC | Stockport | Urban Background
##'  \item STOK | Stoke-on-Trent Centre | Urban Background
##'  \item STOR | Storrington Roadside | Urban traffic
##'  \item SUN2 | Sunderland Silksworth | Urban Background
##'  \item SUND | Sunderland | Urban Background
##'  \item SUT1 | Sutton Roadside | Urban traffic
##'  \item SUT3 | London Sutton | Suburban Background
##'  \item SV | Strath Vaich | Rural Background
##'  \item SWA1 | Swansea Roadside | Urban traffic
##'  \item SWAN | Swansea | Urban Background
##'  \item TED | London Teddington | Urban Background
##'  \item TH2 | Tower Hamlets Roadside | Urban traffic
##'  \item THUR | Thurrock | Urban Background
##'  \item TRAN | Wirral Tranmere | Urban Background
##'  \item WA2 | London Wandsworth | Urban Background
##'  \item WAL | Walsall Alumwell | Urban Background
##'  \item WAL2 | Walsall Willenhall | Urban Background
##'  \item WAR | Warrington | Urban Background
##'  \item WBRO | Sandwell West Bromwich | Urban Background
##'  \item WC | Wharleycroft | Rural Background
##'  \item WEYB | Weybourne | Rural Background
##'  \item WFEN | Wicken Fen | Rural Background
##'  \item WIG3 | Wigan Leigh | Urban Background
##'  \item WIG5 | Wigan Centre | Urban Background
##'  \item WL | West London | Urban Background
##'  \item WOLV | Wolverhampton Centre | Urban Background
##'  \item WRAY | Wray | Rural Background
##'  \item WREX | Wrexham | Urban traffic
##'  \item YARM | Stockton-on-Tees Yarm | Urban traffic
##'  \item YK10 | York Bootham | Urban Background
##'  \item YK11 | York Fishergate | Urban traffic
##'  \item YW | Yarner Wood | Rural Background
##' }
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
##' @export
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
##' \dontrun{thedata <- importAURN(site = c("nott", "kc1"), year = 2008,
##' pollutant = "o3")
##' thedata <- melt(thedata, measure.vars = "o3")
##' thedata <- dcast(thedata, ... ~ variable + site + code)
##' ## thedata now has columns  o3_Nottingham Centre_NOTT o3_London N. Kensington_KC1
##' ## now can export as a csv file:
##' write.csv(thedata, file = "~/temp/thedata.csv")
##'
##' }
##'
##'
importAURN <- function(site = "my1", year = 2009, pollutant = "all", hc = FALSE) {
    site <- toupper(site)


    files <- lapply(site, function (x) paste(x, "_", year, sep = ""))

    files <- do.call(c, files)


    loadData <- function(x) {
        tryCatch({
             fileName <- paste("http://uk-air.defra.gov.uk/openair/R_data/", x, ".RData", sep = "")
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

    thedata <- do.call(rbind.fill, mylist)
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
    if ("nv10" %in% names(thedata)) class(thedata$v10) <- "integer"
    if ("nv2.5" %in% names(thedata)) class(thedata$v2.5) <- "integer"


    ## should hydrocarbons be imported?
    if (hc) {
        thedata <- thedata
         } else {
             ## no hydrocarbons - therefore select conventional pollutants
             theNames <- c("date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
                           "v10", "v2.5", "nv10", "nv2.5", "ws", "wd", "code", "site")

             thedata <- thedata[,  which(names(thedata) %in% theNames)]
         }

     ## if particular pollutants have been selected
    if (pollutant != "all") thedata <- thedata[, c("date", pollutant, "site", "code")]

    rm(list = theObjs, pos = 1)

    ## warning about recent, possibly unratified data
    timeDiff <- difftime(Sys.time(),  max(thedata$date), units='days')
    if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")}

     ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"

    thedata
}
