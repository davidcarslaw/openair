##' Example data for openair
##'
##' The mydata dataset is provided as an example dataset as part of the openair
##' package. The dataset contains hourly measurements of air pollutant
##' concentrations, wind speed and wind direction collected at the Marylebone
##' (London) air quality monitoring supersite between 1st January 1998 and 23rd
##' June 2005.
##'
##' \code{mydata} is supplied with the \code{openair} package as an example
##' dataset for use with documented examples.
##'
##' @name mydata
##' @docType data
##' @format Data frame with 65533 observations (rows) on the following 10
##'   variables: \describe{ \item{list("date")}{Observation date/time stamp in
##'   year-month-day hour:minute:second format (POSIXct). }
##'   \item{list("ws")}{Wind speed, in m/s, as numeric vector.}
##'   \item{list("wd")}{Wind direction, in degrees from North, as a numeric
##'   vector.} \item{list("nox")}{Oxides of nitrogen concentration, in ppb, as
##'   a numeric vector.} \item{list("no2")}{Nitrogen dioxide concentration, in
##'   ppb, as a numeric vector.} \item{list("o3")}{Ozone concentration, in ppb,
##'   as a numeric vector.} \item{list("pm10")}{Particulate PM10 fraction
##'   measurement, in ug/m3 (raw TEOM), as a numeric vector.}
##'   \item{list("so2")}{Sulfur dioxide concentration, in ppb, as a numeric
##'   vector.} \item{list("co")}{Carbon monoxide concentration, in ppm, as a
##'   numeric vector.} \item{list("pm25")}{Particulate PM2.5 fraction
##'   measurement, in ug/m3, as a numeric vector.} }
##' @note \code{openair} functions generally require data frames with a field
##'   "date" that can be in either \code{POSIXct} or \code{Date} format but
##'   should be GMT time zone. This can be hourly data or higher resolution
##'   data.
##' @source \code{mydata} was compiled from data archived in the London Air
##'   Quality Archive.  See \url{http://www.londonair.org.uk} for site details.
##'
##' The same data is also provide in \code{'.csv'} format via the openair
##'   project web site \url{http://www.openair-project.org}.
##' @keywords datasets
##' @examples
##'
##'
##' #basic structure
##' head(mydata)
##'
##'
NULL
