##' Calculate clusters for back tracectories
##'
##' This function carries out cluster analysis of HYSPLIT back
##' trajectories. The function is specifically designed to work with
##' the trajectories imported using the \code{openair}
##' \code{importTraj} function, which provides pre-calculated back
##' trajectories at specific receptor locations.
##'
##' Two main methods are available to cluster the back trajectories
##' using two different calculations of the distance matrix. The
##' default is to use the standard Euclidian distance between each
##' pair of trajectories. Also available is an angle-based distance
##' matrix based on Sirois and Bottenheim (1995). The latter method is
##' useful when the interest is the direction of the trajectories in
##' clustering.
##'
##' The distance matrix calculations are made in C++ for speed. For
##' data sets of up to 1 year both methods should be relatively fast,
##' although the \code{method = "Angle"} does tend to take much longer
##' to calculate. Further details of these methods are given in the
##' openair manual.
##' @param traj An openair trajectory data frame resulting from the
##' use of \code{importTraj}.
##' @param method Method used to calculate the distance matrix for the
##' back trajectories. There are two methods available: \dQuote{Euclid} and
##' \dQuote{Angle}.
##' @param n.cluster Number of clusters to calculate.
##' @param plot Should a plot be produced?
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. \dQuote{season}, \dQuote{year},
##' \dQuote{weekday} and so on. For example, \code{type = "season"} will
##' produce four plots --- one for each season. Note that the cluster
##' calculations are separately made of each level of "type".
##' @param cols Colours to be used for plotting. Options include
##' \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet}
##' and \code{RColorBrewer} colours --- see the \code{openair}
##' \code{openColours} function for more details. For user defined the
##' user can supply a list of colour names recognised by R (type
##' \code{colours()} to see the full list). An example would be
##' \code{cols = c("yellow", "green", "blue")}
##' @param split.after For \code{type} other than \dQuote{default}
##' e.g. \dQuote{season}, the trajectories can either be calculated for each
##' level of \code{type} independently or extracted after the cluster
##' calculations have been applied to the whole data set.
##' @param map.fill Should the base map be a filled polygon? Default
##' is to fill countries.
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
##' package. See\code{?mapproj} for extensive details and information
##' on setting other parameters and orientation (see below).
##' @param parameters From the \code{mapproj} package. Optional
##' numeric vector of parameters for use with the projection
##' argument. This argument is optional only in the sense that certain
##' projections do not require additional parameters. If a projection
##' does require additional parameters, these must be given in the
##' parameters argument.
##' @param orientation From the \code{mapproj} package. An optional
##' vector c(latitude,longitude,rotation) which describes where the
##' "North Pole" should be when computing the projection. Normally
##' this is c(90,0), which is appropriate for cylindrical and conic
##' projections. For a planar projection, you should set it to the
##' desired point of tangency. The third value is a clockwise rotation
##' (in degrees), which defaults to the midrange of the longitude
##' coordinates in the map.
##' @param ... Other graphical parameters passed onto
##' \code{lattice:levelplot} and \code{cutData}. Similarly, common
##' axis and title labelling options (such as \code{xlab},
##' \code{ylab}, \code{main}) are passed to \code{levelplot} via
##' \code{quickText} to handle routine formatting.
##' @export
##' @useDynLib openair
##' @import cluster
##' @return Returns original data frame with a new (factor) variable
##' \code{cluster} giving the calculated cluster.
##' @seealso \code{\link{importTraj}}, \code{\link{trajPlot}}, \code{\link{trajLevel}}
##' @author David Carslaw
##' @references
##'
##' Sirois, A. and Bottenheim, J.W., 1995. Use of backward
##' trajectories to interpret the 5-year record of PAN and O3 ambient
##' air concentrations at Kejimkujik National Park, Nova
##' Scotia. Journal of Geophysical Research, 100: 2867-2881.
##' @keywords methods
##' @examples
##' \dontrun{
##' ## import trajectories
##' traj <- importTraj(site = "london", year = 2009)
##' ## calculate clusters
##' traj <- trajCluster(traj, n.clusters = 5)
##' head(traj) ## note new variable 'cluster'
##' ## use different distance matrix calculation, and calculate by season
##' traj <- trajCluster(traj, method = "Angle", type = "season", n.clusters = 4)
##' }
trajCluster <- function(traj, method = "Euclid", n.cluster = 5, plot = TRUE, type = "default",
                        cols = "Set1", split.after = FALSE, map.fill = TRUE,
                        map.cols = "grey40", map.alpha = 0.4,
                        projection = "lambert",
                        parameters = c(51, 51), orientation = c(90, 0, 0), ...) {

    if (tolower(method) == "euclid")  method <- "distEuclid" else method <- "distAngle"


    extra.args <- list(...)

    ## label controls
    extra.args$plot.type <- if ("plot.type" %in% names(extra.args))
        extra.args$plot.type else extra.args$plot.type <- "l"
    extra.args$lwd <- if ("lwd" %in% names(extra.args))
       extra.args$lwd else extra.args$lwd <- 4



    calcTraj <- function(traj) {

        ## make sure ordered correctly
        traj <- traj[order(traj$date, traj$hour.inc), ]

        ## length of back trajectories
        traj$len <- ave(traj$lat, traj$date, FUN = length)

        ## find length of back trajectories
        ## 96-hour back trajectories with origin: length should be 97
        n <- max(abs(traj$hour.inc)) + 1

        traj <- subset(traj, len == n)
        len <- nrow(traj) / n

        ## lat/lon input matrices
        x <- matrix(traj$lon, nrow = n)
        y <- matrix(traj$lat, nrow = n)

        z <- matrix(0, nrow = n, ncol = len)
        res <- matrix(0, nrow = len, ncol = len)

        res <- .Call(method, x, y, res)

        res[is.na(res)] <- 0 ## possible for some to be NA if trajectory does not move between two hours?


        dist.res <- as.dist(res)
        clusters <- pam(dist.res, n.cluster)
        cluster <- rep(clusters$clustering, each = n)
        traj$cluster <- factor(paste("C", cluster, sep = ""))
        traj

    }

    ## this bit decides whether to separately calculate trajectories for each level of type

    if (split.after) {
        traj <- ddply(traj, "default", calcTraj)
        traj <- cutData(traj, type)
    } else {
        traj <- cutData(traj, type)
        traj <- ddply(traj, type, calcTraj)
    }

    if (plot) {
        ## calculate the mean trajectories by cluster
        agg <- aggregate(traj[, c("lat", "lon", "date")], traj[, c("cluster", "hour.inc", type)] ,
                         mean, na.rm = TRUE)

        ## make sure date is in correct format
        class(agg$date) = class(traj$date)
        attr(agg$date, "tzone") <- "GMT"

        plot.args <- list(agg, x = "lon", y ="lat", group = "cluster",
                    col = cols, type = type, map = TRUE, map.fill = map.fill,
                          map.cols = map.cols, map.alpha = map.alpha,
                          projection = projection, parameters = parameters,
                          orientation = orientation, traj = TRUE)

         ## reset for extra.args
        plot.args <- listUpdate(plot.args, extra.args)

        ## plot
        plt <- do.call(scatterPlot, plot.args)

    }

    invisible(traj)

}


