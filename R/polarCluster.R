##' K-means clustering of bivariate polar plots
##'
##' Function for identifying clusters in bivariate polar plots
##' (\code{polarPlot}); identifying clusters in the original data for
##' subsequent processing.
##'
##' Bivariate polar plots generated using the \code{polarPlot}
##' function provide a very useful graphical technique for identifying
##' and characterising different air pollution sources. While
##' bivariate polar plots provide a useful graphical indication of
##' potential sources, their location and wind-speed or other variable
##' dependence, they do have several limitations. Often, a `feature'
##' will be detected in a plot but the subsequent analysis of data
##' meeting particular wind speed/direction criteria will be based
##' only on the judgement of the investigator concerning the wind
##' speed-direction intervals of interest. Furthermore, the
##' identification of a feature can depend on the choice of the colour
##' scale used, making the process somewhat arbitrary.
##'
##' \code{polarCluster} applies Partition Around Medoids (PAM)
##' clustering techniques to \code{polarPlot} surfaces to help
##' identify potentially interesting features for further
##' analysis. Details of PAM can be found in the \code{cluster}
##' package (a core R package that will be pre-installed on all R
##' systems). PAM clustering is similar to k-means but has several
##' advantages e.g. is more robust to outliers. The clustering is
##' based on the equal contribution assumed from the u and v wind
##' components and the associated concentration. The data are
##' standardized before clustering takes place.
##'
##' The function works best by first trying different numbers of
##' clusters and plotting them. This is achieved by setting
##' \code{n.clusters} to be of length more than 1. For example, if
##' \code{n.clusters = 2:10} then a plot will be output showing the 9
##' cluster levels 2 to 10.
##'
##' Note that clustering is computationally intensive and the function
##' can take a long time to run --- particularly when the number of
##' clusters is increased. For this reason it can be a good idea to
##' run a few clusters first to get a feel for it
##' e.g. \code{n.clusters = 2:5}.
##'
##' Once the number of clusters has been decided, the user can then
##' run \code{polarCluster} to return the original data frame together
##' with a new column \code{cluster}, which gives the cluster number
##' as a character (see example). Note that any rows where the value
##' of \code{pollutant} is \code{NA} are ignored so that the returned
##' data frame may have fewer rows than the original.
##'
##' Note that there are no automatic ways in ensuring the most
##' appropriate number of clusters as this is application
##' dependent. However, there is often a-priori information available
##' on what different features in polar plots correspond
##' to. Nevertheless, the appropriateness of different clusters is
##' best determined by post-processing the data. The Carslaw and
##' Beevers (2012) paper discusses these issues in more detail.
##'
##' Note that unlike most other \code{openair} functions only a single
##' \code{type} \dQuote{default} is allowed.
##'
##' @param mydata A data frame minimally containing \code{wd}, another
##' variable to plot in polar coordinates (the default is a column
##' \dQuote{ws} --- wind speed) and a pollutant. Should also contain
##' \code{date} if plots by time period are required.
##' @param pollutant Mandatory. A pollutant name corresponding to a
##' variable in a data frame should be supplied e.g. \code{pollutant =
##' "nox"}. Only one pollutant can be chosen.
##' @param x Name of variable to plot against wind direction in polar
##' coordinates, the default is wind speed, \dQuote{ws}.
##' @param wd Name of wind direction field.
##' @param n.clusters Number of clusters to use. If \code{n.clusters}
##' is more than length 1, then a \code{lattice} panel plot will be
##' output showing the clusters identified for each one of
##' \code{n.clusters}.
##' @param cols Colours to be used for plotting. Useful options for
##' categorical data are avilable from \code{RColorBrewer} colours ---
##' see the \code{openair} \code{openColours} function for more
##' details. Useful schemes include \dQuote{Accent}, \dQuote{Dark2},
##' \dQuote{Paired}, \dQuote{Pastel1}, \dQuote{Pastel2},
##' \dQuote{Set1}, \dQuote{Set2}, \dQuote{Set3} --- but see
##' ?\code{brewer.pal} for the maximum useful colours in each. For
##' user defined the user can supply a list of colour names recognised
##' by R (type \code{colours()} to see the full list). An example
##' would be \code{cols = c("yellow", "green", "blue")}.
##' @param angle.scale The wind speed scale is by default shown at a
##' 315 degree angle. Sometimes the placement of the scale may
##' interfere with an interesting feature. The user can therefore set
##' \code{angle.scale} to another value (between 0 and 360 degrees) to
##' mitigate such problems. For example \code{angle.scale = 45} will
##' draw the scale heading in a NE direction.
##' @param units The units shown on the polar axis scale.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##' \code{TRUE} titles and axis labels will automatically try and
##' format pollutant names and units properly e.g.  by subscripting
##' the `2' in NO2.
##' @param ... Other graphical parameters passed onto
##' \code{polarPlot}, \code{lattice:levelplot} and
##' \code{cutData}. Common axis and title labelling options (such as
##' \code{xlab}, \code{ylab}, \code{main}) are passed via
##' \code{quickText} to handle routine formatting.
##' @export
##' @import cluster
##' @return As well as generating the plot itself, \code{polarCluster}
##' also returns an object of class ``openair''. The object includes
##' three main components: \code{call}, the command used to generate
##' the plot; \code{data}, the original data frame with a new field
##' \code{cluster} identifying the cluster; and \code{plot}, the plot
##' itself. Note that any rows where the value of \code{pollutant} is
##' \code{NA} are ignored so that the returned data frame may have
##' fewer rows than the original.
##'
##' An openair output can be manipulated using a number of generic
##' operations, including \code{print}, \code{plot} and
##' \code{summary}. 
##'
##' @author David Carslaw
##' @seealso \code{\link{polarPlot}}
##' @references
##'
##' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006).
##' Detecting and quantifying aircraft and other on-airport
##' contributions to ambient nitrogen oxides in the vicinity of a
##' large international airport.  Atmospheric Environment. 40/28 pp
##' 5424-5434.
##'
##' Carslaw, D.C., & Beevers, S.D. (2013). Characterising and
##' understanding emission sources using bivariate polar plots and
##' k-means clustering. Environmental Modelling & Software, 40,
##' 325-329. doi:10.1016/j.envsoft.2012.09.005
##' @examples
##'
##' \dontrun{
##' # load example data from package
##' data(mydata)
##'
##' ## plot 2-8 clusters. Warning! This can take several minutes...
##' 
##' polarCluster(mydata, pollutant = "nox", n.clusters = 2:8)
##' 
##'
##' # basic plot with 6 clusters
##' results <- polarCluster(mydata, pollutant = "nox", n.clusters = 6)
##'
##' ## get results, could read into a new data frame to make it easier to refer to
##' ## e.g. results <- results$data...
##' head(results$data)
##'
##' ## how many points are there in each cluster?
##' table(results$data$cluster)
##'
##' ## plot clusters 3 and 4 as a timeVariation plot using SAME colours as in
##' ## cluster plot
##' timeVariation(subset(results$data, cluster %in% c("3", "4")), pollutant = "nox",
##' group = "cluster", col = openColours("Paired", 6)[c(3, 4)])
##' }
##'
polarCluster <- function(mydata, pollutant = "nox", x = "ws", wd = "wd", n.clusters = 6,
                         cols = "Paired", angle.scale = 315, units = x, auto.text = TRUE, ...) {

    ## avoid R check annoyances
    u = v = z= strip = strip.left = NULL

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip,
                            fontsize = current.font))

    data.orig <- mydata ## keep original data so cluster can be merged with it
    type <- "default"
    vars <- c("wd", x, pollutant)
    vars <- c(vars, "date")

    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    max.x <- ceiling(max(mydata[ , x], na.rm = TRUE))
    min.ws <- floor(min(mydata[[x]], na.rm = TRUE))
    upper <- max.x

    min.scale <- floor(min(mydata[[x]], na.rm = TRUE))

    extra.args <- list(...)

    ## label controls
    extra.args$xlab <- if ("xlab" %in% names(extra.args))
        quickText(extra.args$xlab, auto.text) else quickText("", auto.text)
    extra.args$ylab <- if ("ylab" %in% names(extra.args))
        quickText(extra.args$ylab, auto.text) else quickText("", auto.text)
    extra.args$main <- if ("main" %in% names(extra.args))
        quickText(extra.args$main, auto.text) else quickText("", auto.text)

    if ("fontsize" %in% names(extra.args))
        trellis.par.set(fontsize = list(text = extra.args$fontsize))
    
    ## layout default
    if(!"layout" %in% names(extra.args))
        extra.args$layout <- NULL

    results.grid <- polarPlot(mydata, pollutant = pollutant, x = x, ...)$data

    ## remove missing because we don't want to find clusters for those points
    ## saves a lot on computation
    results.grid <- na.omit(results.grid)
    results.grid <- subset(results.grid, select = c(u, v, z))

    ## sequence of u or v, based on unique values that already exist
    uv.id <- with(results.grid, sort(unique(c(u, v))))

    make.clust <- function(i, results.grid) {
        i <- n.clusters[i]
        dat.orig <- results.grid
        clusters <- pam(results.grid, i, stand = TRUE)
        dat.orig$cluster <- clusters$clustering
        dat.orig$nclust <- paste(i, "clusters")
        dat.orig
    }

    results.grid <- plyr::ldply(seq_along(n.clusters), make.clust, results.grid)

    results.grid$nclust <- ordered(results.grid$nclust, levels = paste(n.clusters, "clusters"))

    ## auto scaling
    nlev <- max(n.clusters) + 1
    breaks <- c(0, 1 : max(n.clusters))
    nlev2 <- length(breaks)
    col <- openColours(cols, (nlev2 - 1))
    col.scale <- breaks

    myform <- formula("cluster ~ u * v | nclust")

    ## find ids of u and v if only one cluster used
    if (length(n.clusters) == 1L) {

        ## find indices in u-v space
        results.grid$u.id <- findInterval(results.grid$u, uv.id)
        results.grid$v.id <- findInterval(results.grid$v, uv.id)

        mydata <- na.omit(mydata)

        mydata <- transform(mydata, u = get(x) * sin(wd * pi / 180),
                            v = get(x) * cos(wd * pi / 180))
        mydata$u.id <- findInterval(mydata$u, uv.id, all.inside = TRUE)
        mydata$v.id <- findInterval(mydata$v, uv.id, all.inside = TRUE)

        ## convert to matrix for direct lookup
        ## need to do this because some data are missing due to exclude.missing in polarPlot
        mat.dim <- max(results.grid[ , c("u.id", "v.id")]) ## size of lookup matrix
        temp <- matrix(NA, ncol = mat.dim, nrow = mat.dim)

        ## matrix of clusters by u.id, v.id with missings
        temp[cbind(results.grid$u.id, results.grid$v.id)] <- results.grid$cluster

        ## match u.id, v.id in mydata to cluster
        mydata$cluster <- as.factor(temp[cbind(mydata$u.id, mydata$v.id)])

        mydata <- mydata[ , c("date", "cluster")] ## just need date/cluster
        mydata <- merge(data.orig, mydata, by = "date")
        results <- mydata
        myform <- formula("cluster ~ u * v")

    }

    ## scaling of 'zeroed' data
    ## scale data by subtracting the min value
    ## this helps with dealing with data with offsets - e.g. negative data
    mydata[ , x] <- mydata[ , x] - min(mydata[ , x], na.rm = TRUE)
    intervals <- pretty(range(mydata[ , x], na.rm = TRUE))

    ## labels for scaling
    labels <- pretty(intervals + min.scale)
    upper <- max(mydata[[x]], na.rm = TRUE)

    ## offset the lines/labels if necessary
    intervals <- intervals + (min(labels) - min.scale)

    ## add zero in the middle if it exists
    if (min.scale != 0){
        labels <- labels[-1]
        intervals <- intervals[-1]
    }



    levelplot.args <- list(x = myform, results.grid, axes = FALSE,
                           as.table = TRUE,
                           col.regions = col,
                           region = TRUE,
                           aspect = 1,
                           at = col.scale,
                           par.strip.text = list(cex = 0.8),
                           scales = list(draw = FALSE),
                           xlim = c(-upper * 1.025, upper * 1.025),
                           ylim = c(-upper * 1.025, upper * 1.025),
                           colorkey = FALSE, #legend = legend,
                           key = list(rectangles = list(col = openColours(cols,
                                                        max(n.clusters)), border = NA),
                           text = list(lab = as.character(1 : max(n.clusters))),
                           space = "right", columns = 1, title = "cluster",
                           cex.title = 1, lines.title = 2),

                           panel = function(x, y, z,subscripts,...) {
                               panel.levelplot(x, y, z,
                                               subscripts,
                                               at = col.scale,
                                               pretty = TRUE,
                                               col.regions = col,
                                               labels = FALSE)

                               angles <- seq(0, 2 * pi, length = 360)

                               sapply(intervals, function(x) llines(x * sin(angles), x * cos(angles),
                                                                    col = "grey", lty = 5))

                               ltext(1.07 * intervals * sin(pi * angle.scale / 180),
                                     1.07 * intervals * cos(pi * angle.scale / 180),
                                     sapply(paste(labels, c("", "", units, rep("", 7))), function(x)
                                            quickText(x, auto.text)) , cex = 0.7, pos = 4)


                               ## add axis line to central polarPlot
                               larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
                               larrows(0, -upper, 0, upper, code = 3, length = 0.1)

                               ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7)
                               ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7)
                               ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7)
                               ltext(upper * 0.95, 0.07 *upper, "E", cex = 0.7)

                           })

    ## reset for extra.args
    levelplot.args <- listUpdate(levelplot.args, extra.args)

    ## plot
    plt <- do.call(levelplot, levelplot.args)

    ## output ################################################################

    if (length(type) == 1L) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))

    ## change cluster output to C1, C2 etc
    mydata$cluster <- paste("C", mydata$cluster, sep = "")

    output <- list(plot = plt, data = results, call = match.call())
    invisible(output)
}
