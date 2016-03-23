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
##' 
##' @param traj An openair trajectory data frame resulting from the 
##'   use of \code{importTraj}.
##' @param method Method used to calculate the distance matrix for the
##'   back trajectories. There are two methods available:
##'   \dQuote{Euclid} and \dQuote{Angle}.
##' @param n.cluster Number of clusters to calculate.
##' @param plot Should a plot be produced?
##' @param type \code{type} determines how the data are split i.e.
##'   conditioned, and then plotted. The default is will produce a 
##'   single plot using the entire data. Type can be one of the
##'   built-in types as detailed in \code{cutData} e.g.
##'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For
##'   example, \code{type = "season"} will produce four plots --- one
##'   for each season. Note that the cluster calculations are
##'   separately made of each level of "type".
##' @param cols Colours to be used for plotting. Options include 
##'   \dQuote{default}, \dQuote{increment}, \dQuote{heat},
##'   \dQuote{jet} and \code{RColorBrewer} colours --- see the
##'   \code{openair} \code{openColours} function for more details. For
##'   user defined the user can supply a list of colour names
##'   recognised by R (type \code{colours()} to see the full list). An
##'   example would be \code{cols = c("yellow", "green", "blue")}
##' @param split.after For \code{type} other than \dQuote{default} 
##'   e.g. \dQuote{season}, the trajectories can either be calculated
##'   for each level of \code{type} independently or extracted after
##'   the cluster calculations have been applied to the whole data
##'   set.
##' @param map.fill Should the base map be a filled polygon? Default 
##'   is to fill countries.
##' @param map.cols If \code{map.fill = TRUE} \code{map.cols} controls
##'   the fill colour. Examples include \code{map.fill = "grey40"} and
##'   \code{map.fill = openColours("default", 10)}. The latter colours
##'   the countries and can help differentiate them.
##' @param map.alpha The transpency level of the filled map which 
##'   takes values from 0 (full transparency) to 1 (full opacity).
##'   Setting it below 1 can help view trajectories, trajectory
##'   surfaces etc. \emph{and} a filled base map.
##' @param projection The map projection to be used. Different map 
##'   projections are possible through the \code{mapproj} package.
##'   See\code{?mapproject} for extensive details and information on
##'   setting other parameters and orientation (see below).
##' @param parameters From the \code{mapproj} package. Optional 
##'   numeric vector of parameters for use with the projection 
##'   argument. This argument is optional only in the sense that
##'   certain projections do not require additional parameters. If a
##'   projection does require additional parameters, these must be
##'   given in the parameters argument.
##' @param orientation From the \code{mapproj} package. An optional 
##'   vector c(latitude,longitude,rotation) which describes where the 
##'   "North Pole" should be when computing the projection. Normally 
##'   this is c(90,0), which is appropriate for cylindrical and conic 
##'   projections. For a planar projection, you should set it to the 
##'   desired point of tangency. The third value is a clockwise
##'   rotation (in degrees), which defaults to the midrange of the
##'   longitude coordinates in the map.
##' @param by.type The percentage of the total number of trajectories
##'   is given for all data by default. Setting \code{by.type = TRUE}
##'   will make each panel add up to 100.
##' @param origin If \code{TRUE} a filled circle dot is shown to mark the
##'     receptor point.   
##' @param ... Other graphical parameters passed onto 
##'   \code{lattice:levelplot} and \code{cutData}. Similarly, common 
##'   axis and title labelling options (such as \code{xlab}, 
##'   \code{ylab}, \code{main}) are passed to \code{levelplot} via 
##'   \code{quickText} to handle routine formatting.
##' @export
##' @useDynLib openair
##' @import cluster
##' @return Returns a list with two data components. The first
##'   (\code{data}) contains the orginal data with the cluster
##'   identified. The second (\code{results}) contains the data used
##'   to plot the clustered trajectories.
##' @seealso \code{\link{importTraj}}, \code{\link{trajPlot}},
##'   \code{\link{trajLevel}}
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
##' clust <- trajCluster(traj, n.clusters = 5)
##' head(clust$data) ## note new variable 'cluster'
##' ## use different distance matrix calculation, and calculate by season
##' traj <- trajCluster(traj, method = "Angle", type = "season", n.clusters = 4)
##' }
trajCluster <- function(traj, method = "Euclid", n.cluster = 5, 
                        plot = TRUE, type = "default",
                        cols = "Set1", split.after = FALSE, map.fill = TRUE,
                        map.cols = "grey40", map.alpha = 0.4,
                        projection = "lambert",
                        parameters = c(51, 51), orientation = c(90, 0, 0),
                        by.type = FALSE, origin = TRUE, ...) {
  
  # silence R check
  freq <- hour.inc <- NULL
  
  if (tolower(method) == "euclid")  
    method <- "distEuclid" else method <- "distAngle"
    
    
    Args <- list(...)
    
    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip,
                            fontsize = current.font))
    
    ## label controls
    Args$plot.type <- if ("plot.type" %in% names(Args))
      Args$plot.type else Args$plot.type <- "l"
    Args$lwd <- if ("lwd" %in% names(Args))
      Args$lwd else Args$lwd <- 4
    
    if ("fontsize" %in% names(Args))
      trellis.par.set(fontsize = list(text = Args$fontsize))
    
    calcTraj <- function(traj) {
      
      ## make sure ordered correctly
      traj <- traj[order(traj$date, traj$hour.inc), ]
      
      ## length of back trajectories
      traj <- group_by(traj, date) %>% 
        mutate(len = length(date))
      
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
      traj$cluster <- as.character(paste("C", cluster, sep = ""))
      traj
      
    }
    
    ## this bit decides whether to separately calculate trajectories for each level of type
    
    if (split.after) {
      
      traj <- group_by_(traj, "default") %>%
        do(calcTraj(.))
      traj <- cutData(traj, type)
      
    } else {
      
      traj <- cutData(traj, type)

      traj <- group_by_(traj, type) %>%
        do(calcTraj(.))
      
    }
    
    # trajectory origin
    origin_xy <- head(subset(traj, hour.inc == 0), 1) ## origin
    tmp <- mapproject(x = origin_xy[["lon"]][1],
                      y = origin_xy[["lat"]][1],
                      projection = projection,
                      parameters = parameters,
                      orientation = orientation)
    receptor <- c(tmp$x, tmp$y)
    
    if (plot) {
      ## calculate the mean trajectories by cluster
      
      agg <- select_(traj, "lat", "lon", "date", "cluster", "hour.inc", type) %>% 
        group_by_(., "cluster", "hour.inc", type) %>% 
        summarise_each(funs(mean))
      
      # the data frame we want to return before it is transformed
      resRtn <- agg
      
      ## proportion of total clusters
      
      clusters <- group_by_(traj, type, "cluster") %>% 
        summarise(n = n()) %>% 
        mutate(freq = round(100 * n / sum(n), 1))
      
      ## make each panel add up to 100
      if (by.type) {
        clusters <- group_by_(clusters, type) %>% 
          mutate(freq = 100 * freq / sum(freq)) 
        
        clusters$freq <- round(clusters$freq, 1)
        
      }
      
      ## make sure date is in correct format
      class(agg$date) = class(traj$date)
      attr(agg$date, "tzone") <- "GMT"
      
      ## xlim and ylim set by user
      if (!"xlim" %in% names(Args))
        Args$xlim <- range(agg$lon)
      
      if (!"ylim" %in% names(Args))
        Args$ylim <- range(agg$lat)
      
      ## extent of data (or limits set by user) in degrees
      trajLims <- c(Args$xlim, Args$ylim)
      
      ## need *outline* of boundary for map limits
      Args <- setTrajLims(traj, Args, projection, parameters, orientation)
      
      ## transform data for map projection
      tmp <- mapproject(x = agg[["lon"]],
                        y = agg[["lat"]],
                        projection = projection,
                        parameters = parameters,
                        orientation = orientation)
      agg[["lon"]] <- tmp$x
      agg[["lat"]] <- tmp$y
      
      plot.args <- list(agg, x = "lon", y = "lat", group = "cluster",
                        col = cols, type = type, map = TRUE, map.fill = map.fill,
                        map.cols = map.cols, map.alpha = map.alpha,
                        projection = projection, parameters = parameters,
                        orientation = orientation, traj = TRUE, trajLims = trajLims,
                        clusters = clusters, receptor = receptor, 
                        origin = origin)
      
      ## reset for Args
      plot.args <- listUpdate(plot.args, Args)
      
      ## plot
      plt <- do.call(scatterPlot, plot.args)
      
    }
    
    
    output <- list(plot = plt, data = traj, results = resRtn, call = match.call())
    class(output) <- "openair"
    invisible(output)
    
}


