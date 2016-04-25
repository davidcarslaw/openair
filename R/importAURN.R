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
##' \tabular{lllrrlll}{
##' CODE \tab SITE NAME \tab SITE TYPE \tab LATITUDE \tab LONGITUDE \tab REGION \tab START DATE \tab END DATE \cr
##' ABD  \tab Aberdeen                          \tab urban_background \tab -
##'   2.094278 \tab 57.15736 \tab North East Scotland     \tab 1999 - 09 - 18 \tab NA\cr
##' ABD7 \tab Aberdeen Union Street Roadside    \tab roadside         \tab -
##'   2.106472 \tab 57.14455 \tab North East Scotland     \tab 2008 - 01 - 01 \tab NA\cr
##' ABD8 \tab Aberdeen Wellington Road          \tab urban            \tab -
##'   2.094198 \tab 57.13389 \tab North East Scotland     \tab 2016 - 02 - 09 \tab NA\cr
##' ARM6 \tab Armagh Roadside                   \tab roadside         \tab -
##'   6.654558 \tab 54.35373 \tab Northern Ireland        \tab 2009 - 01 - 01 \tab NA\cr
##' BALM \tab Ballymena Ballykeel               \tab urban_background \tab -
##'   6.250873 \tab 54.86160 \tab Northern Ireland        \tab 2010 - 01 - 01 \tab NA\cr
##' BAR2 \tab Barnsley 12                       \tab urban_background \tab -
##'   1.485153 \tab 53.55593 \tab Yorkshire &
##'   Humberside  \tab 1994 - 03 - 21 \tab 2012 - 07 - 17\cr
##' BAR3 \tab Barnsley Gawber                   \tab urban_background \tab -
##'   1.510436 \tab 53.56292 \tab Yorkshire &
##'   Humberside  \tab 1997 - 07 - 07 \tab NA\cr
##' BPLE \tab Barnstaple A39                    \tab NA               \tab -
##'   4.041924 \tab 51.07479 \tab South West              \tab 2013 - 11 - 14 \tab NA\cr
##' BATH \tab Bath Roadside                     \tab roadside         \tab -
##'   2.354155 \tab 51.39113 \tab South West              \tab 1996 - 11 - 18 \tab NA\cr
##' BEL2 \tab Belfast Centre                    \tab urban_centre     \tab -
##'   5.928833 \tab 54.59965 \tab Northern Ireland        \tab 1992 - 03 - 08 \tab NA\cr
##' BEL4 \tab Belfast Clara St                  \tab suburban         \tab -
##'   5.895460 \tab 54.59126 \tab Northern Ireland        \tab 1998 - 06 - 01 \tab 2007 -
##'   09 - 30\cr
##' BEL  \tab Belfast East                      \tab urban_background \tab -
##'   5.901667 \tab 54.59653 \tab Northern Ireland        \tab 1989 - 09 - 06 \tab 2007 -
##'   09 - 30\cr
##' BEL1 \tab Belfast Stockman's Lane           \tab roadside         \tab -5.974944 \tab 54.57259 \tab Northern Ireland        \tab 2014-06-07 \tab NA\cr
##' BIL  \tab Billingham                        \tab urban_industrial \tab -1.275039 \tab 54.60537 \tab North East              \tab 1987-01-01 \tab NA\cr
##' AGRN \tab Birmingham Acocks Green           \tab urban_background \tab -1.829999 \tab 52.43717 \tab West Midlands           \tab 2011-03-18 \tab NA\cr
##' BIRM \tab Birmingham Centre                 \tab urban_centre     \tab -1.908078 \tab 52.47972 \tab West Midlands           \tab 1992-03-18 \tab 2009-01-14\cr
##' BIR2 \tab Birmingham East                   \tab urban_background \tab -1.831498 \tab 52.49763 \tab West Midlands           \tab 1993-12-01 \tab 2004-08-04\cr
##' BIR1 \tab Birmingham Tyburn                 \tab urban_background \tab -1.830583 \tab 52.51172 \tab West Midlands           \tab 2004-08-16 \tab 2007-09-30\cr
##' BIRT \tab Birmingham Tyburn Roadside        \tab roadside         \tab -1.830861 \tab 52.51219 \tab West Midlands           \tab 2009-02-11 \tab NA\cr
##' BLAR \tab Blackburn Accrington Road         \tab NA               \tab -2.452724 \tab 53.74775 \tab North West & Merseyside \tab 2014-05-12 \tab NA\cr
##' BLCB \tab Blackburn Darwen Roadside         \tab roadside         \tab -2.483815 \tab 53.71550 \tab North West & Merseyside \tab 2009-06-15 \tab 2014-03-17\cr
##' BLAC \tab Blackpool                         \tab urban_background \tab -3.029283 \tab 53.79046 \tab North West & Merseyside \tab 2000-08-08 \tab 2004-11-11\cr
##' BLC2 \tab Blackpool Marton                  \tab urban_background \tab -3.007175 \tab 53.80489 \tab North West & Merseyside \tab 2005-06-14 \tab 2007-09-30\cr
##' BOLT \tab Bolton                            \tab urban_background \tab -2.439583 \tab 53.57232 \tab North West & Merseyside \tab 1997-02-03 \tab 2008-06-30\cr
##' BORN \tab Bournemouth                       \tab urban_background \tab -1.826744 \tab 50.73957 \tab South West              \tab 2001-03-05 \tab NA\cr
##' BRAD \tab Bradford Centre                   \tab urban_centre     \tab -1.748694 \tab 53.79339 \tab Yorkshire & Humberside  \tab 1997-11-28 \tab 2007-09-30\cr
##' BDMA \tab Bradford Mayo Avenue              \tab urban            \tab -1.759774 \tab 53.77125 \tab Yorkshire & Humberside  \tab 2015-04-24 \tab NA\cr
##' BRN  \tab Brentford Roadside                \tab roadside         \tab -0.310121 \tab 51.48945 \tab Greater London          \tab 2003-06-20 \tab 2007-09-30\cr
##' BRT3 \tab Brighton Preston Park             \tab urban_background \tab -0.147572 \tab 50.84084 \tab South East              \tab 2004-11-03 \tab NA\cr
##' BRIT \tab Brighton Roadside                 \tab roadside         \tab -0.137281 \tab 50.82354 \tab South East              \tab 1998-02-10 \tab 2010-12-31\cr
##' BRT2 \tab Brighton Roadside PM10            \tab roadside         \tab -0.136924 \tab 50.82350 \tab South East              \tab 2003-02-28 \tab 2008-05-29\cr
##' BRIS \tab Bristol Centre                    \tab urban_centre     \tab -2.585622 \tab 51.45718 \tab South West              \tab 1993-01-04 \tab 2005-09-15\cr
##' BRS2 \tab Bristol Old Market                \tab roadside         \tab -2.583519 \tab 51.45603 \tab South West              \tab 1996-07-01 \tab 2012-08-31\cr
##' BRS8 \tab Bristol St Paul's                 \tab urban_background \tab -
##'   2.584482 \tab 51.46284 \tab South West              \tab 2006 - 06 - 15 \tab NA\cr
##' BY1  \tab Bromley Roadside                  \tab roadside         \tab  0.020128 \tab 51.40710 \tab Greater London          \tab 1997 -
##'   05 - 02 \tab 1998 - 07 - 06\cr
##' BURY \tab Bury Roadside                     \tab roadside         \tab -
##'   2.289611 \tab 53.53911 \tab North West &
##'   Merseyside \tab 1997 - 01 - 20 \tab 2007 - 09 - 30\cr
##' BURW \tab Bury Whitefield Roadside          \tab NA               \tab -
##'   2.293772 \tab 53.55903 \tab North West &
##'   Merseyside \tab 2015 - 01 - 01 \tab 2015 - 01 - 01\cr
##' CAM  \tab Cambridge Roadside                \tab roadside         \tab  0.124456 \tab 52.20237 \tab Eastern                 \tab 1999 -
##'   06 - 26 \tab NA\cr
##' CA1  \tab Camden Kerbside                   \tab kerbside         \tab -
##'   0.175269 \tab 51.54421 \tab Greater London          \tab 1996 - 05 - 16 \tab NA\cr
##' CANR \tab Cannock Watling Street            \tab roadside         \tab -
##'   2.030882 \tab 52.67477 \tab West Midlands           \tab 2014 - 12 - 01 \tab 2014 -
##'   12 - 01\cr
##' CANT \tab Canterbury                        \tab urban_background \tab  1.098061 \tab 51.27399 \tab South East              \tab 2001 -
##'   02 - 01 \tab NA\cr
##' CARD \tab Cardiff Centre                    \tab urban_centre     \tab -
##'   3.176250 \tab 51.48178 \tab South Wales             \tab 1992 - 05 - 12 \tab NA\cr
##' CARL \tab Carlisle Roadside                 \tab roadside         \tab -
##'   2.945307 \tab 54.89483 \tab North West &
##'   Merseyside \tab 2008 - 02 - 14 \tab NA\cr
##' CHAT \tab Chatham Roadside                  \tab roadside         \tab  0.547970 \tab 51.37426 \tab South East              \tab 2010 -
##'   07 - 01 \tab NA\cr
##' CHP  \tab Chepstow A48                      \tab roadside         \tab -
##'   2.678731 \tab 51.63809 \tab South Wales             \tab 2008 - 01 - 01 \tab NA\cr
##' CHS6 \tab Chesterfield                      \tab urban_background \tab -
##'   1.433611 \tab 53.23058 \tab East Midlands           \tab 2008 - 03 - 13 \tab 2014 -
##'   05 - 20\cr
##' CHLG \tab Chesterfield Loundsley Green      \tab urban            \tab -
##'   1.454946 \tab 53.24413 \tab East Midlands           \tab 2015 - 03 - 01 \tab NA\cr
##' CHS7 \tab Chesterfield Roadside             \tab roadside         \tab -
##'   1.456944 \tab 53.23172 \tab East Midlands           \tab 2008 - 03 - 11 \tab NA\cr
##' COAL \tab Coventry Allesley                 \tab NA               \tab -
##'   1.560228 \tab 52.41156 \tab West Midlands           \tab 2014 - 06 - 17 \tab NA\cr
##' COV2 \tab Coventry Centre                   \tab urban_centre     \tab -
##'   1.522133 \tab 52.41345 \tab West Midlands           \tab 1997 - 02 - 18 \tab 2000 -
##'   12 - 31\cr
##' COV3 \tab Coventry Memorial Park            \tab urban_background \tab -
##'   1.519612 \tab 52.39440 \tab West Midlands           \tab 2001 - 02 - 26 \tab 2014 -
##'   06 - 10\cr
##' CWMB \tab Cwmbran                           \tab urban_background \tab -
##'   3.006953 \tab 51.65380 \tab South Wales             \tab 2001 - 07 - 20 \tab NA\cr
##' DERY \tab Derry                             \tab urban_background \tab -
##'   7.329115 \tab 55.00122 \tab Northern Ireland        \tab 1997 - 04 - 29 \tab 2016 -
##'   02 - 29\cr
##' DERR \tab Derry Rosemount                   \tab urban_background \tab -
##'   7.331179 \tab 55.00282 \tab Northern Ireland        \tab 2016 - 03 - 21 \tab NA\cr
##' DCST \tab Doncaster A630 Cleveland Street   \tab urban            \tab -
##'   1.138073 \tab 53.51887 \tab Yorkshire &
##'   Humberside  \tab 2015 - 05 - 07 \tab NA\cr
##' DUMB \tab Dumbarton Roadside                \tab roadside         \tab -
##'   4.559730 \tab 55.94320 \tab Central Scotland        \tab 2010 - 09 - 01 \tab NA\cr
##' DUMF \tab Dumfries                          \tab roadside         \tab -
##'   3.614233 \tab 55.07003 \tab Scottish Borders        \tab 2001 - 03 - 01 \tab NA\cr
##' EA8  \tab Ealing Horn Lane                  \tab NA               \tab -
##'   0.265617 \tab 51.51895 \tab Greater London          \tab 2014 - 05 - 21 \tab NA\cr
##' EB   \tab Eastbourne                        \tab urban_background \tab  0.271611 \tab 50.80578 \tab South East              \tab 2009 -
##'   07 - 01 \tab NA\cr
##' ED   \tab Edinburgh Centre                  \tab urban_centre     \tab -
##'   3.195775 \tab 55.95197 \tab Central Scotland        \tab 1992 - 10 - 04 \tab 2003 -
##'   10 - 13\cr
##' ED3  \tab Edinburgh St Leonards             \tab urban_background \tab -
##'   3.182186 \tab 55.94559 \tab Central Scotland        \tab 2003 - 11 - 24 \tab NA\cr
##' EX   \tab Exeter Roadside                   \tab roadside         \tab -
##'   3.532465 \tab 50.72508 \tab South West              \tab 1996 - 07 - 02 \tab NA\cr
##' FW   \tab Fort William                      \tab suburban         \tab -
##'   5.101102 \tab 56.82266 \tab Highland                \tab 2006 - 06 - 22 \tab NA\cr
##' GLA3 \tab Glasgow Centre                    \tab urban_centre     \tab -
##'   4.255161 \tab 55.85773 \tab Central Scotland        \tab 1996 - 07 - 26 \tab 2012 -
##'   08 - 16\cr
##' GLA  \tab Glasgow City Chambers             \tab urban_background \tab -
##'   4.245959 \tab 55.86041 \tab Central Scotland        \tab 1987 - 01 - 06 \tab 2011 -
##'   03 - 16\cr
##' GGWR \tab Glasgow Great Western Road        \tab NA               \tab -
##'   4.270936 \tab 55.87204 \tab Central Scotland        \tab 2014 - 06 - 05 \tab NA\cr
##' GHSR \tab Glasgow High Street               \tab roadside         \tab -
##'   4.238214 \tab 55.86094 \tab Central Scotland        \tab 2015 - 01 - 27 \tab NA\cr
##' GLA4 \tab Glasgow Kerbside                  \tab kerbside         \tab -
##'   4.258889 \tab 55.85917 \tab Central Scotland        \tab 1997 - 03 - 10 \tab NA\cr
##' GLKP \tab Glasgow Townhead                  \tab NA               \tab -
##'   4.243631 \tab 55.86578 \tab Central Scotland        \tab 2013 - 10 - 07 \tab NA\cr
##' GRAN \tab Grangemouth                       \tab urban_industrial \tab -
##'   3.704399 \tab 56.01032 \tab Central Scotland        \tab 2001 - 01 - 01 \tab NA\cr
##' GRA2 \tab Grangemouth Moray                 \tab urban_background \tab -
##'   3.710833 \tab 56.01314 \tab Central Scotland        \tab 2009 - 06 - 01 \tab NA\cr
##' CAE6 \tab Hafod - yr - ynys Roadside            \tab NA               \tab -
##'   3.133508 \tab 51.68058 \tab South Wales             \tab 2014 - 10 - 14 \tab NA\cr
##' HG1  \tab Haringey Roadside                 \tab roadside         \tab -
##'   0.068218 \tab 51.59930 \tab Greater London          \tab 1996 - 05 - 16 \tab NA\cr
##' HONI \tab Honiton                           \tab urban_background \tab -
##'   3.196702 \tab 50.79229 \tab South West              \tab 2012 - 06 - 21 \tab NA\cr
##' HORE \tab Horley                            \tab urban_background \tab -
##'   0.167734 \tab 51.16586 \tab South East              \tab 2007 - 11 - 21 \tab NA\cr
##' HS1  \tab Hounslow Roadside                 \tab roadside         \tab -
##'   0.308975 \tab 51.48965 \tab Greater London          \tab 1997 - 09 - 16 \tab 2002 -
##'   11 - 16\cr
##' HOVE \tab Hove Roadside                     \tab roadside         \tab -
##'   0.170294 \tab 50.82778 \tab South East              \tab 1997 - 09 - 16 \tab 2007 -
##'   09 - 30\cr
##' HULL \tab Hull Centre                       \tab urban_centre     \tab -
##'   0.338322 \tab 53.74479 \tab Yorkshire &
##'   Humberside  \tab 1994 - 01 - 04 \tab 2002 - 01 - 17\cr
##' HUL2 \tab Hull Freetown                     \tab urban_centre     \tab -
##'   0.341222 \tab 53.74878 \tab Yorkshire &
##'   Humberside  \tab 2002 - 11 - 06 \tab 2012 - 09 - 17\cr
##' HULR \tab Hull Holderness Road              \tab urban            \tab -
##'   0.305749 \tab 53.75897 \tab Yorkshire &
##'   Humberside  \tab 2015 - 01 - 15 \tab NA\cr
##' INV2 \tab Inverness                         \tab roadside         \tab -
##'   4.241451 \tab 57.48131 \tab Highland                \tab 2001 - 07 - 17 \tab NA\cr
##' LEAM \tab Leamington Spa                    \tab urban_background \tab -
##'   1.533119 \tab 52.28881 \tab West Midlands           \tab 1996 - 07 - 26 \tab NA\cr
##' LEAR \tab Leamington Spa Rugby Road         \tab roadside         \tab -
##'   1.542911 \tab 52.29488 \tab West Midlands           \tab 2012 - 06 - 01 \tab NA\cr
##' LEED \tab Leeds Centre                      \tab urban_centre     \tab -
##'   1.546472 \tab 53.80378 \tab Yorkshire &
##'   Humberside  \tab 1993 - 01 - 04 \tab NA\cr
##' LED6 \tab Leeds Headingley Kerbside         \tab kerbside         \tab -
##'   1.576361 \tab 53.81997 \tab Yorkshire &
##'   Humberside  \tab 2008 - 02 - 17 \tab NA\cr
##' LEIR \tab Leicester A594 Roadside           \tab urban            \tab -
##'   1.124228 \tab 52.63868 \tab East Midlands           \tab 2015 - 05 - 01 \tab NA\cr
##' LEIC \tab Leicester Centre                  \tab urban_centre     \tab -
##'   1.133006 \tab 52.63135 \tab East Midlands           \tab 1994 - 01 - 04 \tab 2013 -
##'   09 - 23\cr
##' LECU \tab Leicester University              \tab NA               \tab -
##'   1.127311 \tab 52.61982 \tab East Midlands           \tab 2013 - 10 - 01 \tab NA\cr
##' LEOM \tab Leominster                        \tab suburban         \tab -
##'   2.736665 \tab 52.22174 \tab West Midlands           \tab 2005 - 07 - 18 \tab NA\cr
##' LIN3 \tab Lincoln Canwick Rd.               \tab roadside         \tab -
##'   0.534189 \tab 53.22137 \tab East Midlands           \tab 2011 - 07 - 27 \tab NA\cr
##' LINC \tab Lincoln Roadside                  \tab roadside         \tab -
##'   0.537895 \tab 53.22889 \tab East Midlands           \tab 1997 - 05 - 06 \tab 1999 -
##'   12 - 22\cr
##' LIVR \tab Liverpool Centre                  \tab urban_centre     \tab -
##'   2.980249 \tab 53.40845 \tab North West &
##'   Merseyside \tab 1993 - 04 - 23 \tab 2002 - 09 - 23\cr
##' LV6  \tab Liverpool Queen's Drive Roadside  \tab roadside         \tab -2.962500 \tab 53.44694 \tab North West & Merseyside \tab 2008-01-01 \tab NA\cr
##' LVP  \tab Liverpool Speke                   \tab urban_background \tab -2.844333 \tab 53.34633 \tab North West & Merseyside \tab 2003-05-21 \tab 2012-08-21\cr
##' A3   \tab London A3 Roadside                \tab roadside         \tab -0.291853 \tab 51.37348 \tab Greater London          \tab 1997-03-20 \tab 2007-09-30\cr
##' BEX  \tab London Bexley                     \tab suburban         \tab  0.184806 \tab 51.46603 \tab Greater London          \tab 1994-05-01 \tab 2007-09-30\cr
##' CLL2 \tab London Bloomsbury                 \tab urban_centre     \tab -0.125889 \tab 51.52229 \tab Greater London          \tab 1992-01-23 \tab NA\cr
##' BREN \tab London Brent                      \tab urban_background \tab -0.276223 \tab 51.58977 \tab Greater London          \tab 1996-01-26 \tab 2007-09-30\cr
##' BRI  \tab London Bridge Place               \tab urban_background \tab -0.141655 \tab 51.49521 \tab Greater London          \tab 1990-07-03 \tab 1999-11-30\cr
##' BY2  \tab London Bromley                    \tab roadside         \tab  0.018869 \tab 51.40555 \tab Greater London          \tab 1998-08-11 \tab 2007-09-30\cr
##' CRD  \tab London Cromwell Road              \tab kerbside         \tab -0.180564 \tab 51.49492 \tab Greater London          \tab 1973-02-22 \tab 1974-07-11\cr
##' CRD2 \tab London Cromwell Road 2            \tab roadside         \tab -0.178709 \tab 51.49548 \tab Greater London          \tab 1998-05-20 \tab 2012-10-03\cr
##' LON6 \tab London Eltham                     \tab suburban         \tab  0.070766 \tab 51.45258 \tab Greater London          \tab 1996-04-01 \tab NA\cr
##' HK4  \tab London Hackney                    \tab urban_centre     \tab -0.056592 \tab 51.55877 \tab Greater London          \tab 1997-01-06 \tab 2007-09-30\cr
##' HG2  \tab London Haringey                   \tab urban_centre     \tab -0.126486 \tab 51.58603 \tab Greater London          \tab 1996-05-16 \tab 2012-11-09\cr
##' HG4  \tab London Haringey Priory Park South \tab urban_centre     \tab -0.125254 \tab 51.58413 \tab Greater London          \tab 2012-10-26 \tab NA\cr
##' HRL  \tab London Harlington                 \tab airport          \tab -0.441614 \tab 51.48879 \tab Greater London          \tab 2004-01-01 \tab NA\cr
##' HR3  \tab London Harrow Stanmore            \tab urban_background \tab -0.298777 \tab 51.61733 \tab Greater London          \tab 2008-12-16 \tab NA\cr
##' HIL  \tab London Hillingdon                 \tab urban_background \tab -0.460861 \tab 51.49633 \tab Greater London          \tab 1996-08-02 \tab NA\cr
##' LW1  \tab London Lewisham                   \tab urban_centre     \tab -0.020139 \tab 51.44541 \tab Greater London          \tab 1997-04-16 \tab 2007-09-30\cr
##' MY1  \tab London Marylebone Road            \tab kerbside         \tab -0.154611 \tab 51.52253 \tab Greater London          \tab 1997-07-17 \tab NA\cr
##' KC1  \tab London N. Kensington              \tab urban_background \tab -0.213492 \tab 51.52105 \tab Greater London          \tab 1996-04-01 \tab NA\cr
##' SK1  \tab London Southwark                  \tab urban_centre     \tab -0.096667 \tab 51.49055 \tab Greater London          \tab 1997-02-28 \tab 2007-09-30\cr
##' SUT3 \tab London Sutton                     \tab suburban         \tab -0.165489 \tab 51.36789 \tab Greater London          \tab 1996-04-01 \tab 2002-05-02\cr
##' TED2 \tab London Teddington Bushy Park      \tab NA               \tab -0.345606 \tab 51.42529 \tab Greater London          \tab 2013-08-09 \tab NA\cr
##' WA2  \tab London Wandsworth                 \tab urban_centre     \tab -0.191164 \tab 51.45696 \tab Greater London          \tab 1996-04-01 \tab 2007-09-30\cr
##' HORS \tab London Westminster                \tab urban_background \tab -0.131931 \tab 51.49467 \tab Greater London          \tab 2001-07-17 \tab 2013-12-31\cr
##' LUTR \tab Luton A505 Roadside               \tab urban            \tab -0.462110 \tab 51.89229 \tab Eastern                 \tab 2015-03-01 \tab NA\cr
##' MAN3 \tab Manchester Piccadilly             \tab urban_centre     \tab -2.237881 \tab 53.48152 \tab North West & Merseyside \tab 1995-12-18 \tab NA\cr
##' MAHG \tab Manchester Sharston               \tab suburban         \tab -2.239218 \tab 53.37131 \tab North West & Merseyside \tab 2016-01-27 \tab NA\cr
##' MAN4 \tab Manchester South                  \tab suburban         \tab -2.243280 \tab 53.36903 \tab North West & Merseyside \tab 1996-12-06 \tab 2016-01-25\cr
##' MAN  \tab Manchester Town Hall              \tab urban_background \tab -2.244800 \tab 53.47850 \tab North West & Merseyside \tab 1987-01-22 \tab 2007-09-30\cr
##' MID  \tab Middlesbrough                     \tab urban_background \tab -1.220874 \tab 54.56930 \tab North East              \tab 1995-04-21 \tab NA\cr
##' MOLD \tab Mold                              \tab suburban         \tab -3.144889 \tab 53.16231 \tab North Wales             \tab 2009-12-02 \tab 2013-12-31\cr
##' NEWC \tab Newcastle Centre                  \tab urban_centre     \tab -1.610528 \tab 54.97825 \tab North East              \tab 1992-03-08 \tab NA\cr
##' NCA3 \tab Newcastle Cradlewell Roadside     \tab roadside         \tab -1.595362 \tab 54.98640 \tab North East              \tab 2008-03-10 \tab NA\cr
##' NPT3 \tab Newport                           \tab urban_background \tab -2.977281 \tab 51.60120 \tab South Wales             \tab 2008-01-01 \tab NA\cr
##' NTON \tab Northampton                       \tab urban_background \tab -0.885933 \tab 52.27349 \tab East Midlands           \tab 2001-05-23 \tab 2012-07-09\cr
##' NTN3 \tab Northampton Kingsthorpe           \tab urban_background \tab -0.879898 \tab 52.27189 \tab East Midlands           \tab 2012-07-09 \tab NA\cr
##' NTO2 \tab Northampton PM10                  \tab urban_background \tab -0.885933 \tab 52.27349 \tab East Midlands           \tab 2001-04-05 \tab 2007-09-30\cr
##' NOR2 \tab Norwich Centre                    \tab urban_centre     \tab  1.295019 \tab 52.63203 \tab Eastern                 \tab 1997-07-24 \tab 2008-05-13\cr
##' NO10 \tab Norwich Forum Roadside            \tab roadside         \tab  1.291714 \tab 52.62817 \tab Eastern                 \tab 2005-04-08 \tab 2007-09-30\cr
##' NO12 \tab Norwich Lakenfields               \tab urban_background \tab  1.301976 \tab 52.61419 \tab Eastern                 \tab 2009-09-01 \tab NA\cr
##' NOR1 \tab Norwich Roadside                  \tab roadside         \tab  1.299064 \tab 52.62200 \tab Eastern                 \tab 1997-06-21 \tab 2005-02-14\cr
##' NOTT \tab Nottingham Centre                 \tab urban_centre     \tab -1.146447 \tab 52.95473 \tab East Midlands           \tab 1996-09-02 \tab NA\cr
##' NWBV \tab Nottingham Western Boulevard      \tab NA               \tab -1.188851 \tab 52.96938 \tab East Midlands           \tab 2016-03-01 \tab NA\cr
##' BOLD \tab Oldbury Birmingham Road           \tab kerbside         \tab -2.003497 \tab 52.50244 \tab West Midlands           \tab 2014-09-01 \tab NA\cr
##' OX   \tab Oxford Centre Roadside            \tab roadside         \tab -1.257463 \tab 51.75174 \tab South East              \tab 1996-04-15 \tab NA\cr
##' OX8  \tab Oxford St Ebbes                   \tab urban_background \tab -1.260278 \tab 51.74481 \tab South East              \tab 2008-01-01 \tab NA\cr
##' PEEB \tab Peebles                           \tab urban_background \tab -3.196527 \tab 55.65747 \tab Scottish Borders        \tab 2009-11-06 \tab NA\cr
##' PLYM \tab Plymouth Centre                   \tab urban_centre     \tab -4.142361 \tab 50.37167 \tab South West              \tab 1997-09-29 \tab NA\cr
##' PMTH \tab Portsmouth                        \tab urban_background \tab -1.068583 \tab 50.82881 \tab South East              \tab 2001-01-01 \tab NA\cr
##' PT   \tab Port Talbot                       \tab urban_industrial \tab -3.761690 \tab 51.57980 \tab South Wales             \tab 1997-01-09 \tab 2007-07-24\cr
##' PT4  \tab Port Talbot Margam                \tab urban_industrial \tab -3.770822 \tab 51.58395 \tab South Wales             \tab 2007-07-24 \tab NA\cr
##' PRES \tab Preston                           \tab urban_background \tab -2.680353 \tab 53.76559 \tab North West & Merseyside \tab 2000-06-06 \tab NA\cr
##' READ \tab Reading                           \tab urban_background \tab -0.955180 \tab 51.45352 \tab South East              \tab 1997-07-17 \tab 2003-02-06\cr
##' REA5 \tab Reading London Rd.                \tab NA               \tab -0.940382 \tab 51.45490 \tab South East              \tab 2016-03-04 \tab NA\cr
##' REA1 \tab Reading New Town                  \tab urban_background \tab -0.944067 \tab 51.45309 \tab South East              \tab 2003-10-17 \tab 2007-09-30\cr
##' REDC \tab Redcar                            \tab suburban         \tab -1.073300 \tab 54.61073 \tab North East              \tab 1997-06-25 \tab 2007-09-30\cr
##' ROTH \tab Rotherham Centre                  \tab urban_centre     \tab -1.354444 \tab 53.43186 \tab Yorkshire & Humberside  \tab 1997-06-20 \tab 2007-09-30\cr
##' ECCL \tab Salford Eccles                    \tab urban_industrial \tab -2.334139 \tab 53.48481 \tab North West & Merseyside \tab 1997-03-20 \tab 2013-12-31\cr
##' SASH \tab Saltash Callington Road           \tab NA               \tab -4.227678 \tab 50.41146 \tab South West              \tab 2013-01-01 \tab NA\cr
##' SALT \tab Saltash Roadside                  \tab roadside         \tab -4.230300 \tab 50.41310 \tab South West              \tab 2008-07-30 \tab 2010-08-31\cr
##' OLDB \tab Sandwell Oldbury                  \tab urban_background \tab -2.017629 \tab 52.50431 \tab West Midlands           \tab 1997-06-27 \tab 1998-09-21\cr
##' WBRO \tab Sandwell West Bromwich            \tab urban_background \tab -1.995556 \tab 52.52062 \tab West Midlands           \tab 1998-11-04 \tab 2011-12-31\cr
##' SDY  \tab Sandy Roadside                    \tab roadside         \tab -0.300306 \tab 52.13242 \tab Eastern                 \tab 2008-07-28 \tab NA\cr
##' SCUN \tab Scunthorpe                        \tab urban_industrial \tab -0.633015 \tab 53.58499 \tab Yorkshire & Humberside  \tab 1997-12-15 \tab 2004-03-18\cr
##' SCN2 \tab Scunthorpe Town                   \tab urban_industrial \tab -0.636811 \tab 53.58634 \tab Yorkshire & Humberside  \tab 2004-06-06 \tab NA\cr
##' CW   \tab Shaw Crompton Way                 \tab NA               \tab -2.093786 \tab 53.57928 \tab North West & Merseyside \tab 2014-04-01 \tab NA\cr
##' SHE2 \tab Sheffield Centre                  \tab urban_centre     \tab -1.473306 \tab 53.37772 \tab Yorkshire & Humberside  \tab 1995-12-22 \tab 2013-08-30\cr
##' SHDG \tab Sheffield Devonshire Green        \tab NA               \tab -1.478096 \tab 53.37862 \tab Yorkshire & Humberside  \tab 2013-10-31 \tab NA\cr
##' SHE  \tab Sheffield Tinsley                 \tab urban_background \tab -1.396139 \tab 53.41058 \tab Yorkshire & Humberside  \tab 1990-11-28 \tab NA\cr
##' SA33 \tab Southampton A33                   \tab NA               \tab -1.463484 \tab 50.92027 \tab South East              \tab 2016-01-01 \tab NA\cr
##' SOUT \tab Southampton Centre                \tab urban_centre     \tab -1.395778 \tab 50.90814 \tab South East              \tab 1994-01-04 \tab NA\cr
##' SEND \tab Southend-on-Sea                   \tab urban_background \tab  0.678408 \tab 51.54421 \tab Eastern                 \tab 2000-07-24 \tab NA\cr
##' SK5  \tab Southwark A2 Old Kent Road        \tab roadside         \tab -0.059550 \tab 51.48050 \tab Greater London          \tab 2011-01-01 \tab NA\cr
##' SK2  \tab Southwark Roadside                \tab roadside         \tab -0.062300 \tab 51.48199 \tab Greater London          \tab 1997-04-01 \tab 2006-02-21\cr
##' HOPE \tab Stanford-le-Hope Roadside         \tab roadside         \tab  0.439548 \tab 51.51817 \tab Eastern                 \tab 2008-01-22 \tab 2012-12-31\cr
##' STEW \tab Stewartby                         \tab urban_industrial \tab -0.511111 \tab 52.07194 \tab Eastern                 \tab 2007-11-26 \tab 2008-12-31\cr
##' STOC \tab Stockport                         \tab urban_background \tab -2.158200 \tab 53.40994 \tab North West & Merseyside \tab 1996-11-25 \tab 2002-10-03\cr
##' STK4 \tab Stockport Shaw Heath              \tab urban_background \tab -2.161111 \tab 53.40306 \tab North West & Merseyside \tab 2002-10-09 \tab 2007-09-30\cr
##' SOTR \tab Stockton-on-Tees A1305 Roadside   \tab roadside         \tab -1.315900 \tab 54.56582 \tab North East              \tab 2015-01-01 \tab NA\cr
##' EAGL \tab Stockton-on-Tees Eaglescliffe     \tab roadside         \tab -1.358547 \tab 54.51667 \tab North East              \tab 2009-01-21 \tab NA\cr
##' YARM \tab Stockton-on-Tees Yarm             \tab roadside         \tab -1.354319 \tab 54.50918 \tab North East              \tab 2001-01-01 \tab 2008-09-02\cr
##' STKR \tab Stoke-on-Trent A50 Roadside       \tab roadside         \tab -2.111898 \tab 52.98044 \tab West Midlands           \tab 2015-05-22 \tab NA\cr
##' STOK \tab Stoke-on-Trent Centre             \tab urban_centre     \tab -2.175133 \tab 53.02821 \tab West Midlands           \tab 1997-03-11 \tab NA\cr
##' STOR \tab Storrington Roadside              \tab roadside         \tab -0.449548 \tab 50.91693 \tab South East              \tab 2009-08-01 \tab NA\cr
##' SUND \tab Sunderland                        \tab urban_background \tab -1.380081 \tab 54.90611 \tab North East              \tab 1992-10-06 \tab 2007-09-30\cr
##' SUN2 \tab Sunderland Silksworth             \tab urban_background \tab -1.406878 \tab 54.88361 \tab North East              \tab 2004-12-09 \tab NA\cr
##' SUNR \tab Sunderland Wessington Way         \tab urban            \tab -1.408391 \tab 54.91839 \tab North East              \tab 2015-01-01 \tab NA\cr
##' SUT1 \tab Sutton Roadside                   \tab roadside         \tab -0.182789 \tab 51.36636 \tab Greater London          \tab 1996-04-01 \tab 2002-05-02\cr
##' SWAN \tab Swansea                           \tab urban_centre     \tab -3.943329 \tab 51.62114 \tab South Wales             \tab 1994-12-01 \tab 2006-08-07\cr
##' SWA1 \tab Swansea Roadside                  \tab roadside         \tab -3.947374 \tab 51.63270 \tab South Wales             \tab 2006-09-20 \tab 2007-09-30\cr
##' THUR \tab Thurrock                          \tab urban_background \tab  0.317969 \tab 51.47707 \tab Eastern                 \tab 1996-09-01 \tab NA\cr
##' TH2  \tab Tower Hamlets Roadside            \tab roadside         \tab -0.042155 \tab 51.52253 \tab Greater London          \tab 1996-04-01 \tab NA\cr
##' WAL  \tab Walsall Alumwell                  \tab urban_background \tab -2.010483 \tab 52.58167 \tab West Midlands           \tab 1987-03-05 \tab 2007-09-30\cr
##' WAL2 \tab Walsall Willenhall                \tab urban_background \tab -2.033144 \tab 52.60821 \tab West Midlands           \tab 1997-04-29 \tab 2010-02-03\cr
##' WAL4 \tab Walsall Woodlands                 \tab urban_background \tab -2.030523 \tab 52.60562 \tab West Midlands           \tab 2012-06-19 \tab NA\cr
##' WAR  \tab Warrington                        \tab urban_background \tab -2.615358 \tab 53.38928 \tab North West & Merseyside \tab 2008-10-21 \tab NA\cr
##' WL   \tab West London                       \tab urban_background \tab -0.200361 \tab 51.49380 \tab Greater London          \tab 1987-01-01 \tab 2007-09-30\cr
##' WSMR \tab Widnes Milton Road                \tab roadside         \tab -2.731680 \tab 53.36539 \tab North West & Merseyside \tab 2015-01-01 \tab NA\cr
##' WIG5 \tab Wigan Centre                      \tab urban_background \tab -2.638139 \tab 53.54914 \tab North West & Merseyside \tab 2004-10-08 \tab 2007-09-30\cr
##' WIG3 \tab Wigan Leigh                       \tab urban_background \tab -2.506899 \tab 53.49422 \tab North West & Merseyside \tab 2001-01-01 \tab 2004-09-28\cr
##' TRAN \tab Wirral Tranmere                   \tab urban_background \tab -3.022722 \tab 53.37287 \tab North West & Merseyside \tab 2000-05-14 \tab NA\cr
##' WOLV \tab Wolverhampton Centre              \tab urban_centre     \tab -2.129008 \tab 52.58818 \tab West Midlands           \tab 1995-12-19 \tab 2007-09-30\cr
##' WREX \tab Wrexham                           \tab roadside         \tab -3.002778 \tab 53.04222 \tab North Wales             \tab 2002-03-01 \tab NA\cr
##' YK10 \tab York Bootham                      \tab urban_background \tab -1.086514 \tab 53.96751 \tab Yorkshire & Humberside  \tab 2008-01-01 \tab NA\cr
##' YK11 \tab York Fishergate                   \tab roadside         \tab -1.075861 \tab 53.95189 \tab Yorkshire & Humberside  \tab 2008-01-01 \tab NA
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
importAURN <- function(site = "my1", year = 2009, pollutant = "all", hc = FALSE) {
    site <- toupper(site)


    files <- lapply(site, function (x) paste(x, "_", year, sep = ""))

    files <- do.call(c, files)


    loadData <- function(x) {
        tryCatch({
             fileName <- paste("http://uk-air.defra.gov.uk/openair/R_data/", x, ".RData", sep = "")
             con <- url(fileName, method = "libcurl")
             load(con)
             
             close(con)
             
             dat <- get(x)
             
             return(dat)
             },
                  error = function(ex) {cat(x, "does not exist - ignoring that one.\n")})

     }


    thedata <- plyr::ldply(files, loadData)
    
    if (nrow(thedata) == 0) return() ## no data

    ## suppress warnings for now - unequal factors, harmless
 
    if (is.null(thedata)) stop("No data to import - check site codes and year.", call. = FALSE)

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
             theNames <- c("date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
                           "v10", "v2.5", "nv10", "nv2.5", "ws", "wd", "code", "site")

             thedata <- thedata[,  which(names(thedata) %in% theNames)]
         }

     ## if particular pollutants have been selected
    if (pollutant != "all") thedata <- thedata[, c("date", pollutant, "site", "code")]


    ## warning about recent, possibly unratified data
    timeDiff <- difftime(Sys.time(),  max(thedata$date), units='days')
    if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")}

     ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"

    thedata
}
