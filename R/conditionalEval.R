##' Conditional quantile estimates with additional variables for model evaluation
##'
##' This function enhances \code{\link{conditionalQuantile}} by also
##' considering how other variables vary over the same
##' intervals. Conditional quantiles are very useful on their own for
##' model evaluation, but provide no direct information on how other
##' variables change at the same time. For example, a conditional
##' quantile plot of ozone concentrations may show that low
##' concentrations of ozone tend to be under-predicted. However, the
##' cause of the under-prediction can be difficult to
##' determine. However, by considering how well the model predicts
##' other variables over the same intervals, more insight can be
##' gained into the underlying reasons why model performance is poor.
##'
##' The \code{conditionalEval} function provides information on how
##' other variables vary across the same intervals as shown on the
##' conditional quantile plot. There are two types of variable that
##' can be considered by setting the value of \code{statistic}. First,
##' \code{statistic} can be another variable in the data frame. In
##' this case the plot will show the different proportions of
##' \code{statistic} across the range of predictions. For example
##' \code{statistic = "season"} will show for each interval of
##' \code{mod} the proportion of predictions that were spring, summer,
##' autumn or winter. This is useful because if model performance is
##' worse for example at high concentrations of \code{mod} then
##' knowing that these tend to occur during a particular season
##' etc. can be very helpful when trying to understand \emph{why} a
##' model fails. See \code{\link{cutData}} for more details on the
##' types of variable that can be \code{statistic}. Another example
##' would be \code{statistic = "ws"} (if wind speed were available in
##' the data frame), which would then split wind speed into four
##' quantiles and plot the proportions of each.
##'
##' Second, \code{conditionalEval} can simultaneously plot the model
##' performance of other observed/predicted variable \bold{pairs}
##' according to different model evaluation statistics. These
##' statistics derive from the \code{\link{modStats}} function and
##' include \dQuote{MB}, \dQuote{NMB}, \dQuote{r}, \dQuote{COE},
##' \dQuote{MGE}, \dQuote{NMGE}, \dQuote{RMSE} and \dQuote{FAC2}. More
##' than one statistic can be supplied e.g. \code{statistic = c("NMB",
##' "COE")}. Bootstrap samples are taken from the corresponding values
##' of other variables to be plotted and their statistics with 95\%
##' confidence intervals calculated. In this case, the model
##' \emph{performance} of other variables is shown across the same
##' intervals of \code{mod}, rather than just the values of single
##' variables. In this second case the model would need to provide
##' observed/predicted pairs of other variables.
##'
##' For example, a model may provide predictions of NOx and wind speed
##' (for which there are also observations available). The
##' \code{conditionalEval} function will show how well these other
##' variables are predicted for the same intervals of the main
##' variables assessed in the conditional quantile e.g. ozone. In this
##' case, values are supplied to \code{var.obs} (observed values for
##' other variables) and \code{var.mod} (modelled values for other
##' variables). For example, to consider how well the model predicts
##' NOx and wind speed \code{var.obs = c("nox.obs", "ws.obs")} and
##' \code{var.mod = c("nox.mod", "ws.mod")} would be supplied
##' (assuming \code{nox.obs, nox.mod, ws.obs, ws.mod} are present in
##' the data frame). The analysis could show for example, when ozone
##' concentrations are under-predicted, the model may also be shown to
##' over-predict concentrations of NOx at the same time, or
##' under-predict wind speeds. Such information can thus help identify
##' the underlying causes of poor model performance. For example, an
##' under-prediction in wind speed could result in higher surface NOx
##' concentrations and lower ozone concentrations. Similarly if wind
##' speed predictions were good and NOx was over predicted it might
##' suggest an over-estimate of NOx emissions. One or more additional
##' variables can be plotted.
##'
##' A special case is \code{statistic = "cluster"}. In this case a
##' data frame is provided that contains the cluster calculated by
##' \code{\link{trajCluster}} and
##' \code{\link{importTraj}}. Alternatively users could supply their
##' own pre-calculated clusters. These calculations can be very useful
##' in showing whether certain back trajectory clusters are associated
##' with poor (or good) model performance. Note that in the case of
##' \code{statistic = "cluster"} there will be fewer data points used
##' in the analysis compared with the ordinary statistics above
##' because the trajectories are available for every three hours. Also
##' note that \code{statistic = "cluster"} cannot be used together
##' with the ordinary model evaluation statistics such as MB. The
##' output will be a bar chart showing the proportion of each interval
##' of \code{mod} by cluster number.
##'
##' Far more insight can be gained into model performance through
##' conditioning using \code{type}. For example, \code{type =
##' "season"} will plot conditional quantiles and the associated model
##' performance statistics of other variables by each
##' season. \code{type} can also be a factor or character field
##' e.g. representing different models used.
##'
##' See Wilks (2005) for more details of conditional quantile plots.
##'
##' @param mydata A data frame containing the field \code{obs} and \code{mod}
##'   representing observed and modelled values.
##' @param obs The name of the observations in \code{mydata}.
##' @param mod The name of the predictions (modelled values) in \code{mydata}.
##' @param var.obs Other variable observations for which statistics
##' should be calculated. Can be more than length one
##' e.g. \code{var.obs = c("nox.obs", "ws.obs")}.
##' @param var.mod Other variable predictions for which statistics
##' should be calculated. Can be more than length one
##' e.g. \code{var.obs = c("nox.obs", "ws.obs")}.
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
##' @param bins Number of bins used in \code{conditionalQuantile}.
##' @param statistic Statistic(s) to be plotted. Can be \dQuote{MB},
##' \dQuote{NMB}, \dQuote{r}, \dQuote{COE}, \dQuote{MGE},
##' \dQuote{NMGE}, \dQuote{RMSE} and \dQuote{FAC2}, as described in
##' \code{modStats}. When these statistics are chosen, they are
##' calculated from \code{var.mod} and \code{var.mod}.
##'
##' \code{statistic} can also be a value that can be supplied to
##' \code{cutData}. For example, \code{statistic = "season"} will show
##' how model performance varies by season across the distribution of
##' predictions which might highlight that at high concentrations of
##' NOx the model tends to underestimate concentrations and that these
##' periods mostly occur in winter. \code{statistic} can also be
##' another variable in the data frame --- see \code{cutData} for more
##' information. A special case is \code{statistic = "cluster"} if
##' clusters have been calculated using \code{trajCluster}.
##' @param xlab label for the x-axis, by default \code{"predicted value"}.
##' @param ylab label for the y-axis, by default \code{"observed value"}.
##' @param col Colours to be used for plotting the uncertainty bands and median
##'   line. Must be of length 5 or more.
##' @param col.var Colours for the additional variables to be
##' compared. See \code{openColours} for more details.
##' @param var.names Variable names to be shown on plot for plotting
##' \code{var.obs} and \code{var.mod}.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels etc. will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param ... Other graphical parameters passed onto
##' \code{conditionalQuantile} and \code{cutData}. For example,
##' \code{conditionalQuantile} passes the option \code{hemisphere =
##' "southern"} on to \code{cutData} to provide southern (rather than
##' default northern) hemisphere handling of \code{type = "season"}.
##' Similarly, common axis and title labelling options (such as
##' \code{xlab}, \code{ylab}, \code{main}) are passed to \code{xyplot}
##' via \code{quickText} to handle routine formatting.
##' @import latticeExtra
##' @export
##' @author David Carslaw
##' @seealso See \code{\link{conditionalQuantile}} for information on conditional
##' quantiles, \code{\link{modStats}} for model evaluation statistics
##' and the package \code{verification} for comprehensive functions
##' for forecast verification.
##' @references Wilks, D. S., 2005. Statistical Methods in the Atmospheric
##'   Sciences, Volume 91, Second Edition (International Geophysics), 2nd
##'   Edition. Academic Press.
##' @keywords methods
##' @examples
##'
##'
##' ## Examples to follow, or will be shown in the openair manual
##'
conditionalEval <- function(mydata, obs = "obs", mod = "mod",
                            var.obs = "var.obs", var.mod = "var.mod",
                            type = "default",
                            bins = 31,
                            statistic = "MB",
                            xlab = "predicted value",
                            ylab = "statistic",
                            col = brewer.pal(5, "YlOrRd"),
                            col.var = "Set1",
                            var.names = NULL,
                            auto.text = TRUE, ...) {

    Var1 <- NULL; current.strip <- NULL; hour.inc <- NULL; .id <- NULL; Freq <- NULL ## keep CRAN check happy


    ## statistic can be predefined one in modStats, cluster, date-based e.g. "season" or
    ## another field in the data frame
    vars <- NULL
    other <- FALSE ## statistic other than var.obs/var.mod

    ## greyscale handling
    if (length(col.var) == 1 && col.var == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")
    
    ## reset graphic parameters
    on.exit(trellis.par.set(strip.background = current.strip,
                            fontsize = current.font))

    ## statistic is date-based
    if (any(statistic %in% dateTypes)) {
        ## choose only one statistic
        statistic <- statistic[which(statistic %in% dateTypes)][1]
        mydata <- cutData(mydata, type = statistic)
        vars <- c(vars, statistic)
        other <- TRUE ## i.e. statistic other than var.obs/var.mod is present
    }

    ## statistic is based on varible in data frame
    if (any(statistic %in% names(mydata)))  {
        if (!other) {
            statistic <- statistic[which(statistic %in% names(mydata))][1]
            mydata <- cutData(mydata, type = statistic)
            vars <- c(vars, statistic) ## use this statistic
            other <- TRUE
        }
    }

    if ("cluster" %in% statistic) {
        other <- TRUE
        statistic <- "cluster"
    }

    ## various checks
    if (length(var.obs) == 0 | length(var.mod) == 0 & !"cluster" %in% statistic)
        stop ("No variables chosen to analyse")
    if (length(var.obs) != length(var.mod))
        stop ("Number of var.obs does not equal number of var.mod variables")
    if (length(type) > 1)
        stop("Only one type can be used with this function")

    ## don't need var.obs or var.mod if statistic is "other"
    if (other) {var.obs <- NULL; var.mod <- NULL}

    ## extra.args setup
    extra.args <- list(...)



    ## allowable ordinary model evaluation statistics
    the.stats <- c("MB", "NMB", "r", "COE", "MGE", "NMGE", "RMSE", "FAC2")

    ## label controls
    ## (xlab and ylab handled in formals because unique action)
    extra.args$main <- if ("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)

    if ("fontsize" %in% names(extra.args))
        trellis.par.set(fontsize = list(text = extra.args$fontsize))

    ## variables needed
    vars <- c(vars, mod, obs, var.obs, var.mod)

    cluster <- FALSE
    ## if cluster is in data frame then remove any data duplicates
    if (statistic[1] == "cluster") {
        if ("hour.inc" %in% names(mydata)) mydata <- subset(mydata, hour.inc == 0)
        vars <- c(vars, "cluster")
        cluster <- TRUE
    }

    ## ordinary conditional quantile plot
    pltCondQ <- conditionalQuantile(mydata, obs = obs, mod = mod, type = type, bins = bins,
                                    key.position = "bottom", key.columns = 1, layout = c(1, NA), ...)$plot

    if (any(type %in% dateTypes)) vars <- c("date", vars)

    ## check the data
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    mydata <- na.omit(mydata)

    ## for plot limits
    lo <- min(mydata[ , c(mod, obs)])
    hi <- max(mydata[ , c(mod, obs)])

    mydata <- cutData(mydata, type)

    ## function to process ordinary statistics using bootstrap confidence intervals

    procData <- function(mydata, statistic = statistic, var.obs = var.obs, 
                         var.mod = var.mod, ...) {
        ## only numerics if not clustering
        if (!other) mydata <- mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                       drop = FALSE]

        obs <- mydata[[obs]]
        pred <- mydata[[mod]]
        min.d <- min(c(obs, pred))
        max.d <- max(c(obs, pred))
        bins <- seq(floor(min.d), ceiling(max.d), length = bins)

        b <- bins[-length(bins)]
        labs <- b + 0.5 * diff(bins)

        pred.cut <- cut(pred, breaks = bins, include.lowest = TRUE,
                        labels = labs)
        pred.cut[is.na(pred.cut)] <- labs[1]

        ## split by predicted intervals
        res <- split(mydata, pred.cut)

        statFun <- function(x, ...) {

            tmpFun <- function(i, x, ...) {
                x <- x[sample(1:nrow(x), nrow(x), replace = TRUE), ]
                get(statistic)(x, obs = var.obs, mod = var.mod)
            }

            if (nrow(x) > 4) {
                res <- plyr::ldply(1:200, tmpFun, x, ...)

                data.frame(statistic = statistic, group = var.obs, mean = mean(res[[statistic]]),
                           lower = quantile(res[[statistic]], probs = 0.025, na.rm = TRUE),
                           upper = quantile(res[[statistic]], probs = 0.975, na.rm = TRUE))
            }
        }

        if (other) {

             res <- plyr::ldply(res, function (x) as.data.frame(table(x[, statistic])))

            ## calculate proportions by interval
            
             res <- group_by(res, .id) %>%
               mutate( Freq = Freq / sum(Freq)
                       )
             res$statistic <- factor(statistic)

        } else {

            res <- plyr::ldply(res, statFun, statistic = statistic)

        }

        res
    }

    ## treat clusters specfically if present ###############################################

    if (other) {

        clust.results <- group_by_(mydata, type) %>%
          do(procData(., other = other, statistic = statistic))

        clust.results$.id <- as.numeric(clust.results$.id)

        pol.name <- sapply(levels(clust.results[["statistic"]]), function(x) quickText(x, auto.text))
        strip <- strip.custom(factor.levels = pol.name)

        if (type == "default") {

            strip.left <- FALSE

        } else { ## two conditioning variables

            pol.name <- sapply(levels(clust.results[ , type[1]]), function(x) quickText(x, auto.text))
            strip.left <- strip.custom(factor.levels = pol.name)
        }
        ## ###################################################################################

        cols <-  openColours(col.var, length(unique(clust.results$Var1)))
        temp <- "statistic"
        if (type != "default") temp <- paste(c("statistic", type), collapse = "+")
        myform <- formula(paste("Freq ~ .id | ", temp, sep = ""))

        clust.plt <- xyplot(myform , data = clust.results,
                            xlim = c(lo, hi * 1.05),
                            ylim = c(0, 1),
                            ylab = "proportion",
                            xlab = quickText(xlab, auto.text),
                            as.table = TRUE,
                            strip = strip,
                            strip.left = strip.left,
                            groups = Var1,
                            stack = TRUE,
                            col = cols,
                            border = NA,
                            drop.unused.levels = FALSE,
                            horizontal = FALSE,
                            key = list(rectangles = list(col = cols, border = NA),
                            text = list(levels(clust.results$Var1)), space = "bottom",
                            title = statistic, cex.title = 1),
                            par.strip.text = list(cex = 0.8),

                            panel = function (x, ...) {
                                ## set width of boxes depending on x-scale
                                box.width = diff(sort(unique(x)))[1]
                                panel.grid(-1, -1)
                                panel.barchart(x, box.width = box.width, ...)
                            })
    }

    ## go through list of ordinary statistics ##################################################
    statistic <- statistic[which(statistic %in% the.stats)]

    if (length(statistic) > 0) {

        ## go through vars, then statistics
        results <- plyr::ldply(seq_along(var.obs), function (x) {
            plyr::ldply(statistic, function (y) plyr::ddply(mydata, type, procData, statistic = y,
                                                var.obs = var.obs[x], var.mod = var.mod[x]))})
        results$.id <- as.numeric(results$.id)

        ## make sure all infinite values are set to NA
        results[] <- lapply(results, function(x) {replace(x, x == Inf | x == -Inf, NA)})

        ## proper names of labelling #####################################################

        pol.name <- sapply(levels(results[ , "statistic"]), function(x) quickText(x, auto.text))
        strip <- strip.custom(factor.levels = pol.name)

        if (type == "default") {

            strip.left <- FALSE

        } else { ## two conditioning variables

            pol.name <- sapply(levels(results[ , type[1]]), function(x) quickText(x, auto.text))
            strip.left <- strip.custom(factor.levels = pol.name)
        }
        ## #####################################################################################

        ## set up colours
        myColors <- openColours(col.var, length(var.obs))

        if (is.null(var.names)) var.names <- var.obs

        key <- list(lines = list(col = myColors[1:length(var.obs)], lty = extra.args$lty, lwd = 2),
                    text = list(lab = sapply(var.names, function(x) quickText(x, auto.text)), cex = 1),
                    space = "bottom", columns = 2,
                    title = quickText("variable", auto.text), cex.title = 1)

        temp <- "statistic"
        if (type != "default") temp <- paste(c("statistic", type), collapse = "+")

        myform <- formula(paste("mean ~ .id | ", temp, sep = ""))

        p.args <- list(x = myform, data = results, groups = results$group,
                       ylim = dlply(results, .(statistic), function(x) c(min(x$lower, na.rm = TRUE),
                       max(x$upper, na.rm = TRUE))),
                       xlim = c(lo, hi * 1.05),
                       ylab = quickText(ylab, auto.text),
                       xlab = quickText(xlab, auto.text),
                       as.table = TRUE,
                       key = key,
                       aspect = 1,
                       strip = strip,
                       strip.left = strip.left,
                       scales = list(y = list(relation = "free", rot = 0)),

                       par.strip.text = list(cex = 0.8),
                       panel = panel.superpose, ...,
                       panel.groups = function(x, y, group.number, subscripts, ...)
                   {
                       if (group.number == 1) {

                           panel.grid (-1, -1, col = "grey95")
                           if (results$statistic[subscripts][1] %in% c("r", "COE", "FAC2"))
                               panel.abline(h = 1, lty = 5)
                           if (results$statistic[subscripts][1] %in% c("MB", "NMB"))
                               panel.abline(h = 0, lty = 5)
                       }

                       poly.na(x, results$lower[subscripts], x,
                               results$upper[subscripts], group.number, myColors)

                       panel.lines(results$.id[subscripts], results$mean[subscripts],
                                   col.line = myColors[group.number], lwd = 2)

                   })

        ## reset for extra.args
        p.args <- listUpdate(p.args, extra.args)

    }

    if (other) {
        thePlot <- clust.plt
    } else {
        thePlot <- do.call(xyplot, p.args)
    }

    ## how wide to set plots,  base is "2" units
    ## width <- 1.2 / (1.2 + max(length(statistic), 1))
    width <- 0.45
    print(pltCondQ, position = c(0, 0, width, 1), more = TRUE)

    if (type == "default") {
        suppressWarnings(print(thePlot, position = c(width, 0, 1, 1), more = FALSE))
    } else {
        suppressWarnings(print(useOuterStrips(thePlot, strip = strip,
                             strip.left = strip.left), position = c(width, 0, 1, 1), more = FALSE))
    }


    invisible(trellis.last.object())

    if (other) results <- clust.results

    output <- list(plot = list(pltCondQ, trellis.last.object()), data = results, call = match.call())
    class(output) <- "openair"
    invisible(output)
}

