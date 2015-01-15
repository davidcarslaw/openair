##' Trajectory level plots with conditioning
##'
##' This function plots gridded back trajectories. This function
##' requires that data are imported using the \code{importTraj}
##' function.
##'
##'
##' An alternative way of showing the trajectories compared with
##' plotting trajectory lines is to bin the points into
##' latitude/longitude intervals. For these purposes \code{trajLevel}
##' should be used. There are several trajectory statistics that can
##' be plotted as gridded surfaces. First, \code{statistic} can be set
##' to \dQuote{frequency} to show the number of back trajectory points
##' in a grid square. Grid squares are by default at 1 degree
##' intervals, controlled by \code{lat.inc} and \code{lon.inc}. Such
##' plots are useful for showing the frequency of air mass
##' locations. Note that it is also possible to set \code{method =
##' "hexbin"} for plotting frequencies (not concentrations), which
##' will produce a plot by hexagonal binning.
##'
##' If \code{statistic = "difference"} the trajectories associated
##' with a concentration greater than \code{percentile} are compared
##' with the the full set of trajectories to understand the
##' differences in freqeuncies of the origin of air masses of the
##' highest concentration trajectories compared with the trajectories
##' on average. The comparsion is made by comparing the percentage
##' change in gridded frequencies. For example, such a plot could show
##' that the top 10\% of concentrations of PM10 tend to orginate from
##' air-mass origins to the east.
##'
##' If \code{statistic = "pscf"} then the Potential Source
##' Contribution Function is plotted. The PSCF calculates the
##' probability that a source is located at latitude \eqn{i} and
##' longitude \eqn{j} (Pekney et al., 2006).The basis of PSCF is that
##' if a source is located at (i,j), an air parcel back trajectory
##' passing through that location indicates that material from the
##' source can be collected and transported along the trajectory to
##' the receptor site. PSCF solves \deqn{PSCF = m_{ij}/n_{ij}} where
##' \eqn{n_{ij}} is the number of times that the trajectories passed
##' through the cell (i,j) and \eqn{m_{ij}} is the number of times
##' that a source concentration was high when the trajectories passed
##' through the cell (i,j). The criterion for de-termining
##' \eqn{m_{ij}} is controlled by \code{percentile}, which by default
##' is 90. Note also that cells with few data have a weighting factor
##' applied to reduce their effect.
##'
##' A limitation of the PSCF method is that grid cells can have the
##' same PSCF value when sample concentrations are either only
##' slightly higher or much higher than the criterion. As a result, it
##' can be difficult to distinguish moderate sources from strong
##' ones. Seibert et al. (1994) computed concentration fields to
##' identify source areas of pollutants. The Concentration Weighted
##' Trajectory (CWT) approach considers the concentration of a species
##' together with its residence time in a grid cell. The CWT approach
##' has been shown to yield similar results to the PSCF approach. The
##' openair manual has more details and examples of these approaches.
##'
##' A further useful refinement is to smooth the resulting surface,
##' which is possible by setting \code{smooth = TRUE}.
##'
##' @note This function is under active development and is likely to change
##'
##' @param mydata Data frame, the result of importing a trajectory
##' file using \code{importTraj}
##' @param lon Column containing the longitude, as a decimal.
##' @param lat Column containing the latitude, as a decimal.
##' @param pollutant Pollutant to be plotted. By default the
##' trajectory height is used.
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. "season", "year",
##' "weekday" and so on. For example, \code{type = "season"} will
##' produce four plots --- one for each season.
##'
##' It is also possible to choose \code{type} as another variable in
##' the data frame. If that variable is numeric, then the data will be
##' split into four quantiles (if possible) and labelled
##' accordingly. If type is an existing character or factor variable,
##' then those categories/levels will be used directly. This offers
##' great flexibility for understanding the variation of different
##' variables and how they depend on one another.
##'
##' \code{type} can be up length two e.g. \code{type = c("season",
##' "weekday")} will produce a 2x2 plot split by season and day of the
##' week. Note, when two types are provided the first forms the
##' columns and the second the rows.
##'
##' @param smooth Should the trajectory surface be smoothed?
##' @param statistic For \code{trajLevel}. By default the function
##' will plot the trajectory frequencies.
##'
##' For \code{trajLevel}, the argument \code{method = "hexbin"} can be
##' used. In this case hexagonal binning of the trajectory
##' \emph{points} (i.e. a point every three hours along each back
##' trajectory). The plot then shows the trajectory frequencies uses
##' hexagonal binning. This is an alternative way of viewing
##' trajectory frequencies compared with \code{statistic =
##' "frequency"}.
##'
##' There are also various ways of plotting concentrations.
##'
##' It is also possible to set \code{statistic = "difference"}. In
##' this case trajectories where the associated concentration is
##' greater than \code{percentile} are compared with the the full set
##' of trajectories to understand the differences in freqeuncies of
##' the origin of air masses. The comparsion is made by comparing the
##' percentage change in gridded frequencies. For example, such a plot
##' could show that the top 10\% of concentrations of PM10 tend to
##' orginate from air-mass origins to the east.
##'
##' If \code{statistic = "pscf"} then a Potential Source Contribution
##' Function map is produced. If \code{statistic = "cwt"} then
##' concentration weighted trajectories are plotted.
##'
##' If \code{statistic = "cwt"} then the Concentration Weighted
##' Trajectory approach is used. See details.
##' @param percentile For \code{trajLevel}. The percentile
##' concentration of \code{pollutant} against which the all
##' trajectories are compared.
##' @param map Should a base map be drawn? If \code{TRUE} the world
##' base map from the \code{maps} package is used.
##' @param lon.inc The longitude-interval to be used for binning data
##' for \code{trajLevel}.
##' @param lat.inc The latitude-interval to be used for binning data
##' when \code{trajLevel}.
##' @param min.bin For \code{trajLevel} the minimum number of unique
##' points in a grid cell. Counts below \code{min.bin} are set as
##' missing. For \code{trajLevel} gridded outputs.
##' @param map.fill Should the base map be a filled polygon? Default
##' is to fill countries.
##' @param map.res The resolution of the base map. By default the
##' function uses the \sQuote{world} map from the \code{maps}
##' package. If \code{map.res = "hires"} then the (much) more detailed
##' base map \sQuote{worldHires} from the \code{mapdata} package is
##' used.
##' @param map.cols If \code{map.fill = TRUE} \code{map.cols} controls
##' the fill colour. Examples include \code{map.fill = "grey40"} and
##' \code{map.fill = openColours("default", 10)}. The latter colours
##' the countries and can help differentiate them.
##' @param map.alpha The transpency level of the filled map which
##' takes values from 0 (full transparency) to 1 (full
##' opacity). Setting it below 1 can help view trajectories,
##' trajectory surfaces etc. \emph{and} a filled base map.
##' @param projection The map projection to be used. Different map
##' projections are possible through the \code{mapproj}
##' package. See \code{?mapproj} for extensive details and information
##' on setting other parameters and orientation (see below).
##' @param parameters From the \code{mapproj} package. Optional
##' numeric vector of parameters for use with the projection
##' argument. This argument is optional only in the sense that certain
##' projections do not require additional parameters. If a projection
##' does not require additional parameters then set to null
##' i.e. \code{parameters = NULL}.
##' @param orientation From the \code{mapproj} package. An optional
##' vector c(latitude, longitude, rotation) which describes where the
##' "North Pole" should be when computing the projection. Normally
##' this is c(90, 0), which is appropriate for cylindrical and conic
##' projections. For a planar projection, you should set it to the
##' desired point of tangency. The third value is a clockwise rotation
##' (in degrees), which defaults to the midrange of the longitude
##' coordinates in the map.
##' @param grid.col The colour of the map grid to be used. To remove
##' the grid set \code{grid.col = "transparent"}.
##' @param ... other arguments are passed to \code{cutData} and
##' \code{scatterPlot}. This provides access to arguments used in both
##' these functions and functions that they in turn pass arguments on
##' to. For example, \code{plotTraj} passes the argument \code{cex} on
##' to \code{scatterPlot} which in turn passes it on to the
##' \code{lattice} function \code{xyplot} where it is applied to set
##' the plot symbol size.
##' @export
##' @return NULL
##' @seealso \code{\link{importTraj}} to import trajectory data from the King's
##' College server and \code{\link{trajPlot}} for plotting back trajectory lines.
##' @author David Carslaw
##' @references
##'
##' Pekney, N. J., Davidson, C. I., Zhou, L., & Hopke,
##' P. K. (2006). Application of PSCF and CPF to PMF-Modeled Sources
##' of PM 2.5 in Pittsburgh. Aerosol Science and Technology, 40(10),
##' 952-961.
##'
##' Seibert, P., Kromp-Kolb, H., Baltensperger, U., Jost, D.,
##' 1994. Trajectory analysis of high-alpine air pollution data. NATO
##' Challenges of Modern Society 18, 595-595.
##'
##' Xie, Y., & Berkowitz, C. M. (2007). The use of conditional
##' probability functions and potential source contribution functions
##' to identify source regions and advection pathways of hydrocarbon
##' emissions in Houston, Texas. Atmospheric Environment, 41(28),
##' 5831-5847.
##' @examples
##'
##' # show a simple case with no pollutant i.e. just the trajectories
##' # let's check to see where the trajectories were coming from when
##' # Heathrow Airport was closed due to the Icelandic volcanic eruption
##' # 15--21 April 2010.
##' # import trajectories for London and plot
##' \dontrun{
##' lond <- importTraj("london", 2010)
##' }
##' # more examples to follow linking with concentration measurements...
##'
##' # import some measurements from KC1 - London
##' \dontrun{
##' kc1 <- importAURN("kc1", year = 2010)
##' # now merge with trajectory data by 'date'
##' lond <- merge(lond, kc1, by = "date")
##'
##' # trajectory plot, no smoothing - and limit lat/lon area of interest
##' # use PSCF
##' trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20),
##' pollutant = "pm10", statistic = "pscf")
##'
##' # can smooth surface, suing CWT approach:
##' trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20),
##' pollutant = "pm2.5", statistic = "cwt",  smooth = TRUE)
##'
##' # plot by season:
##' trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm2.5",
##' statistic = "pscf", type = "season")
##' }
trajLevel <- function(mydata, lon = "lon", lat = "lat",
                      pollutant = "height", type = "default", smooth = FALSE,
                      statistic = "frequency", percentile = 90,
                      map = TRUE, lon.inc = 1.0, lat.inc = 1.0, min.bin = 1,
                      map.fill = TRUE, map.res = "default", map.cols = "grey40",
                      map.alpha = 0.3, projection = "lambert",
                      parameters = c(51, 51), orientation = c(90, 0, 0),
                      grid.col = "deepskyblue", ...)  {

    ## mydata can be a list of several trajectory files; in which case combine them
    ## before averaging

    ## variables needed in trajectory plots
    vars <- c("date", "lat", "lon", "hour.inc", pollutant)
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    ## extra.args
    extra.args <- list(...)

    statistic <- tolower(statistic)

    if(!"ylab" %in% names(extra.args))
        extra.args$ylab <- ""

    if(!"xlab" %in% names(extra.args))
        extra.args$xlab <- ""

     if(!"main" %in% names(extra.args))
        extra.args$main <- ""

    if(!"key.header" %in% names(extra.args)) {
        if (statistic == "frequency") extra.args$key.header <- "% trajectories"
        if (statistic == "pscf") extra.args$key.header <- "PSCF \nprobability"
        if (statistic == "difference") extra.args$key.header <- quickText(paste("gridded differences", "\n(", percentile, "th percentile)", sep = ""))
    }

     if(!"key.footer" %in% names(extra.args))
         extra.args$key.footer <- ""

    extra.args$trajStat <- statistic

    if (!"method" %in% names(extra.args)) {
        method <- "traj"
    } else {
        method <- extra.args$method
        statistic = "XX" ## i.e. it wont touch the data
    }

    if (method == "density") stop ("Use trajPlot with method = 'density' instead")

    if (is.list(mydata)) mydata <- rbind.fill(mydata)

    mydata <- cutData(mydata, type, ...)

    ## bin data
    if (method == "traj") {
        mydata$ygrid <- round_any(mydata[ , lat], lat.inc)
        mydata$xgrid <- round_any(mydata[ , lon], lon.inc)
    } else {
        mydata$ygrid <- mydata[ , lat]
        mydata$xgrid <- mydata[ , lon]
    }

    rhs <- c("xgrid", "ygrid", type)
    rhs <- paste(rhs, collapse = "+")
    mydata <- mydata[ , c("date", "xgrid", "ygrid", type, pollutant)]
    ids <- which(names(mydata) %in% c("xgrid", "ygrid", type))

    ## plot mean concentration - CWT method
    if (statistic %in% c("cwt", "median")) {

        counts <-  aggregate(mydata[ , -ids], mydata[ , ids],
                             function (x)  length(x))

        if (statistic == "cwt") stat.name <- "mean" else stat.name <- "median"

        ## need dates for later processing e.g. for type = "season"
        dates <- aggregate(mydata[ , -ids], mydata[ , ids], function (x) head(x, 1))
        dates <- dates$date

        mydata <- aggregate(mydata[ , -ids], mydata[ , ids], get(stat.name), na.rm = TRUE)
        mydata$count <- counts$date

        mydata$date <- dates

        mydata <- subset(mydata, count >= min.bin)

        ## adjust at edges

        id <- which(mydata$count > 20 & mydata$count <= 80)
        mydata[id, pollutant] <- mydata[id, pollutant] * 0.7

        id <- which(mydata$count > 10 & mydata$count <= 20)
        mydata[id, pollutant] <- mydata[id, pollutant] * 0.42

        id <- which(mydata$count <= 10)
        mydata[id, pollutant] <- mydata[id, pollutant] * 0.05
        attr(mydata$date, "tzone") <- "GMT"  ## avoid warning messages about TZ
    }

    ## plot trajectory frequecies
    if (statistic == "frequency") {
        ## count % of times a cell contains a trajectory
        ## counts by conditioning variable not total

        ## need dates for later processing e.g. for type = "season"
        dates <- aggregate(mydata[ , -ids], mydata[ , ids], function (x) head(x, 1))
        dates <- dates$date

        mydata <- aggregate(mydata[ , -ids], mydata[ , ids],
                            function (x) length(unique(x)))


        mydata$count <-  mydata[, pollutant] #mydata[, "date"] #counts$date
        mydata$date <- dates

        mydata[, pollutant] <- ave(mydata$count, mydata[, type], FUN = function (x) 100 * x / max(x))


        attr(mydata$date, "tzone") <- "GMT"  ## avoid warning messages about TZ

    }

    ## Poential Source Contribution Function
    if (statistic == "pscf") {
         ## count % of times a cell contains a trajectory
        n1 <- length(unique(mydata$date))
        dat1 <- aggregate(mydata[ , -ids], mydata[ , ids],
                          function (x) length(x))
        dat1[, pollutant] <- dat1[, "date"]
        dat1 <- subset(dat1, select = -date)

        ## select top X percent
        Q90 <- quantile(mydata[, pollutant], probs = percentile / 100, na.rm = TRUE)

        ## now select trajectories with conc > percentile
        dat2 <- subset(mydata, get(pollutant) > Q90)
        n2 <- length(unique(dat2$date))
        ## number in each bin
        counts <-  aggregate(dat2[ , -ids], dat2[ , ids],
                             function (x)  length(x))

        ## need dates for later processing e.g. for type = "season"
        dates <- aggregate(dat2[ , -ids], dat2[ , ids], function (x) head(x, 1))
        dates <- dates$date

        dat2 <- aggregate(dat2[ , -ids], dat2[ , ids],
                          function (x) length(x))
        dat2[, pollutant] <- dat2[, "date"]
        dat2$count <- counts$date
        dat2$date <- dates
        attr(dat2$date, "tzone") <- "GMT"  ## avoid warning messages about TZ
        dat2 <- subset(dat2, count >= min.bin)

        ## differences
        mydata <- merge(dat1, dat2, by = c("xgrid", "ygrid", type))
        pol1 <- paste(pollutant, ".x", sep = "")
        pol2 <- paste(pollutant, ".y", sep = "")
        mydata[, pollutant] <-  mydata[, pol2] / mydata[, pol1]

        ## adjust at edges
        n <- mean(mydata$count)
        id <- which(mydata$count > n & mydata$count <= 2 * n)
        mydata[id, pollutant] <- mydata[id, pollutant] * 0.75

        id <- which(mydata$count > (n / 2) & mydata$count <= n)
        mydata[id, pollutant] <- mydata[id, pollutant] * 0.5

        id <- which(mydata$count <= (n / 2))
        mydata[id, pollutant] <- mydata[id, pollutant] * 0.15


    }

    ## plot trajectory frequecy differences e.g. top 10% concs cf. mean
    if (statistic == "difference") {
        ## count % of times a cell contains a trajectory
        n1 <- length(unique(mydata$date))
        dat1 <- aggregate(mydata[ , -ids], mydata[ , ids],
                          function (x) 100 * length(unique(x)) / n1)
        dat1[, pollutant] <- dat1[, "date"]
        dat1 <- subset(dat1, select = -date)

        ## select top X percent
        Q90 <- quantile(mydata[, pollutant], probs = percentile / 100, na.rm = TRUE)

        ## now select trajectories with conc > percentile
        dat2 <- subset(mydata, get(pollutant) > Q90)
        n2 <- length(unique(dat2$date))
        ## number in each bin
        counts <-  aggregate(dat2[ , -ids], dat2[ , ids],
                             function (x)  length(unique(x)))

        ## need dates for later processing e.g. for type = "season"
        dates <- aggregate(dat2[ , -ids], dat2[ , ids], function (x) head(x, 1))
        dates <- dates$date

        dat2 <- aggregate(dat2[ , -ids], dat2[ , ids],
                          function (x) 100 * length(unique(x)) / n2)
        dat2[, pollutant] <- dat2[, "date"]
        dat2$count <- counts$date
        dat2$date <- dates
        attr(dat2$date, "tzone") <- "GMT"  ## avoid warning messages about TZ
        dat2 <- subset(dat2, count >= min.bin)

        ## differences
        mydata <- merge(dat1, dat2, by = c("xgrid", "ygrid", type))
        pol1 <- paste(pollutant, ".x", sep = "")
        pol2 <- paste(pollutant, ".y", sep = "")
        mydata[, pollutant] <-  mydata[, pol2] - mydata[, pol1]

    }


    ## change x/y names to gridded values
    lon <- "xgrid"
    lat <- "ygrid"

    ## the plot
    scatterPlot.args <- list(mydata, x = lon, y = lat, z = pollutant,
                             type = type, method = method, smooth = smooth,
                             map = map, x.inc = lon.inc, y.inc = lat.inc,
                             map.fill = map.fill, map.res = map.res,
                             map.cols = map.cols, map.alpha = map.alpha, traj = TRUE,
                             projection = projection,
                             parameters = parameters, orientation = orientation,
                             grid.col = grid.col)

    ## reset for extra.args
    scatterPlot.args <- listUpdate(scatterPlot.args, extra.args)

    ## plot
    do.call(scatterPlot, scatterPlot.args)

}

