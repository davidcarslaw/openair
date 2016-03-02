#####################
#openair map plotting
#####################
#working
#
#kr 07/07/2011 v

#######################
#file contains
#scripts for:
#######################
#function: GoogleMapsPlot
#function: openairMapManager
#function: panel.GoogleMaps
#function: panel.GoogleMapsRaster
#

######################
#notes
######################
#GoogleMapsPlot
#requires/loads RgoogleMaps (openair suggests)
#RgoogleMaps version 1.1.9.13 (not 10)
#requires png package for png
#
######################
#map.panels TRUE/FALSE
#selects panel.GoogleMapsRaster or panel.GoogleMaps
#structure of both panel...(map)
#one arg, map; no extra args allowed
#no checking that map is valid
#map is modification of RgoogleMaps output
#


#####################
#to do
#####################
#cols, cex, pch handling
#openair panels??
##e.g. panel.bubbleplot,
#could we make this control the key type?
#because the bubble key might want to be different?
#


#####################
#suggests
#####################
#handle more than one pollutant?
#more control of key to allow different key types via same command
#



#####################
#####################
##FUNCTIONS
#####################
#####################


#################################
##GoogleMapsPlot
#################################



##' GoogleMapsPlot
##'
##' [IN DEVELOPMENT] Map plotting for openair data sets.
##'

##'
##' \code{GoogleMapsPlot} is an IN DEVELOPMENT function.
##'
##' It combines a dedicated map layer, e.g.  \code{\link{panel.GoogleMaps}}, or
##' (the default) \code{\link{panel.GoogleMapsRaster}}, and standard
##' \code{\link{lattice}} panels such as \code{\link[lattice]{panel.xyplot}} or
##' \code{\link[lattice]{panel.levelplot}} as a data layer, to produce
##' map-based data visualisations.
##'
##' It provides lattice-style conditioning/handling for
##' \code{RgoogleMaps} outputs.
##'
##' @aliases GoogleMapsPlot panel.GoogleMapsRaster panel.GoogleMaps
##' @param mydata The openair data frame to use to generate the
##'   \code{GoogleMapsPlot} plot.
##' @param latitude,longitude The names of the data series in \code{mydata}
##'   giving the latitudes and longitudes, respectively, of measurements.
##'   If only one latitude longitude pair are supplied, the function applies
##'   a default range to the plot. To override this either set the required range
##'   using \code{xlim} and \code{ylim} (see below) or the map \code{zoom}
##'   level. (Note: The default is equivalent to \code{zoom = 15}.)
##' @param type The type of data conditioning to apply before
##' plotting. The default is will produce a single plot using the
##' entire data. Other type options include \dQuote{hour} (for hour of
##' the day), \dQuote{weekday} (for day of the week) and
##' \dQuote{month} (for month of the year), \dQuote{year},
##' \dQuote{season} (string, summer, autumn or winter) and
##' \dQuote{daylight} (daylight or nighttime hour).  But it is also
##' possible to set \code{type} to the name of another variable in
##' \code{mydata}, in which case the plotted data will be divided into
##' quantiles based on that data series. See \code{cutData} for
##' further details.(NOTE: type conditioning currently allows up to
##' two levels of conditioning, e.g., \code{type = c("weekday",
##' "daylight")}.)
##' @param xlim,ylim The x-axis and y-axis size ranges. By default
##' these sized on the basis of \code{latitude} and \code{longitude},
##' but can be forced as part of the plot call. (NOTE: This are
##' in-development and should be used with care. The RgoogleMaps
##' argument \code{size = c(640, 640)} can be use to force map
##' dimensions to square.)
##' @param pollutant If supplied, the name of a pollutant or variable
##' in \code{mydata} that is to be evaluated at the each measurement
##' point.  Depending on settings, nominally \code{cols} and
##' \code{cex}, the evaluation can be by colour, size or both.
##' @param labels If supplied, either the name of \code{mydata} column/field
##'   containing the labels to be used or a list, containing that field name
##'   (as \code{labels}), and any other label properties, e.g. \code{cex},
##'   \code{col}, etc, required for fine-tuning label appearance.
##' @param cols The colour set to use to colour scaled data. Typically,
##'   \code{cols} is passed to \code{openColours} for evaluation, but can be
##'   forced to one colour using e.g. \code{col = "red"}. The special case
##'   \code{cols = "greyscale"} forces all plot components (the map, the data
##'   layer and the plot strip of \code{type} conditioning) to greyscale for
##'   black and white printing. See \code{?openColours} for more details.
##' @param limits By default, the data colour scale is fitted to the total data
##'   range. However, there are circumstances when the user may wish to set
##'   different ones. In such cases \code{limits} can be set in the form
##'   \code{c(lower, upper)} to modify the colour range.
##' @param cex The size of data points plotted on maps. By default this
##'   \code{NULL} or \code{pollutant} if supplied. If \code{NULL} all points
##'   are plotted an equal size. If \code{pollutant} or the name of another
##'   variable in \code{mydata} this is used by scaled using \code{cex.range}.
##'   If necessary, \code{cex} can also be forced, e.g. \code{cex = 1} to make
##'   all points the same size.
##' @param pch The plot symbol to be used when plotting data. By default this
##'   is a solid circle (\code{pch = 20}), but can be any predefined symbol,
##'   e.g. \code{pch = 1} is the open circle symbol used in most standard R
##'   plots. \code{pch} may also be the name of a variable in \code{mydata} for
##'   local control.
##' @param cex.range The range to rescale \code{cex} values to if \code{cex} is
##'   supplied as a \code{mydata} variable name. This is intended to provide
##'   sensible data point points regardless of the variable value range but may
##'   be require fine-tuning.
##' @param xlab,ylab,main The x-axis, y-axis and main title labels to be added
##'   to the plot. All labels are passed via \code{quickText} to handle
##'   formatting if enabled (\code{auto.text = TRUE}). By default
##'   \code{GoogleMapsPlot} uses \code{latitude} and \code{longitude} names as
##'   xlab and ylab, respectively.
##' @param axes An alternative (short hand) option to add/remove
##'   (\code{TRUE}/\code{FALSE}) all x and y axis annotation and labelling.
##' @param map If supplied, an \code{RgoogleMaps} output, to be used as a
##'   background map. If \code{NULL} (as in default), a map is produced using
##'   the \code{RgoogleMaps-package} function \code{MapBackground}, the supplied
##'   \code{latitude} and \code{longitude} ranges, and any additional
##'   \code{RgoogleMaps-package} arguments supplied as part of the plot call.  (Note:
##'   the \code{map} object currently used in \code{panel...} functions is a
##'   modified form of this output, details to be confirmed.)
##' @param map.raster Should the map be plotted as a raster object? The default
##'   \code{TRUE} uses \code{panel.GoogleMapsRaster} to produce the map layer,
##'   while the alternative (\code{FALSE}) uses \code{panel.GoogleMaps}. (NOTE:
##'   The raster version is typically much faster but may not be available for
##'   all computer systems.)
##' @param map.cols Like \code{cols} a colour scale, but, if supplied, used to
##'   recolour the map layer before plotting. (NOTE: If set, this will override
##'   \code{cols = "greyscale"}.)
##' @param aspect The aspect ratio of the plot. If \code{NULL} (default), this
##'   is calculated by the function based on the data and \code{xlim} and
##'   \code{ylim} ranges.
##' @param as.table \code{as.table} is a \code{lattice} option that controls
##'   the order in which multiple panels are displayed. The default
##'   (\code{TRUE}) produces layouts similar to other openair plot.
##' @param plot.type The method to use to produce the data layer for the plot.
##'   By default (\code{plot.type = "xy"}), this is an x-y style scatter plot,
##'   but can also be other pre-defined options (e.g. "level" for a levelplot)
##'   or a user-defined panel of a similar structire to \code{panel...}
##'   functions in \code{lattice}.
##' @param plot.transparent Data layer transparency control. When enabled, this
##'   forces colours used in the data layer to transparent, and can be a
##'   numeric setting the colour density, from invisible (0) to solid (1), or a
##'   logical (\code{TRUE} applying default 0.5). Note: User-defined colours
##'   (and some panel defaults when supplying specialist functions using e.g.
##'   \code{plot.type = panel...}) may sometimes supersede this option.
##' @param key Fine control for the color scale key. By default (\code{key =
##'   NULL}) the key is generated is a colour range exists, but can be forced
##'   (\code{key = TRUE/FALSE}) or controlled at a higher level (via
##'   \code{drawOpenKey}).
##' @param key.position Location where the scale key should be plotted.
##'   Allowed arguments currently include \code{"top"}, \code{"right"},
##'   \code{"bottom"} and \code{"left"}.
##' @param key.header,key.footer Header and footer labels to add to colour key,
##'   if drawn. If enabled (\code{auto.text = TRUE}), these arguments are
##'   passed to the scale key (\code{drawOpenKey}) via \code{quickText} to
##'   handle formatting.
##' @param auto.text Automatic routine text formatting. \code{auto.text = TRUE}
##'   allows labels (\code{xlab}, \code{ylab}, \code{main}, etc.) to be passed
##'   to the plot via \code{quickText}.  \code{auto.text = FALSE} turns this
##'   option off and passes labels to the plot without modification.
##' @param ... Addition options are passed on to \code{cutData} for \code{type}
##'   handling, \code{MapBackground} in \code{RgoogleMaps} for map layer
##'   production, and \code{xyplot} in \code{lattice} for data layer
##'   production.
##' @import RgoogleMaps
##' @export
##' @return As well as generating the plot itself, \code{GoogleMapsPlot} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- GoogleMapsPlot(mydata)}, this output can be used to
##'   recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. 
##' @note IN DEVELOPMENT: HANDLE WITH CARE.
##'
##'   Users should be aware that Google Maps are flat 2D projections of the
##'   Earth's (curved) surface. Latitude and longitude scales are therefore
##'   locally modified to account for this.
##' @author Karl Ropkins
##' @seealso \code{RgoogleMaps},
##'   \code{\link[lattice]{xyplot}}, \code{\link[lattice]{panel.xyplot}} and
##'   \code{\link[lattice]{panel.levelplot}}
##' @references This function makes extensive use of code developed by others.
##'
##' RgoogleMaps: Markus Loecher and Sense Networks (2011).  RgoogleMaps:
##'   Overlays on Google map tiles in R. R package version 1.1.9.6.
##'   http://CRAN.R-project.org/package=RgoogleMaps
##'
##' lattice: Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization
##'   with R. Springer, New York. ISBN 978-0-387-75968-5
##' @keywords methods
##' @examples
##'
##' #TO BE CONFIRMED
##'
##'
GoogleMapsPlot <- function(mydata,
         latitude = "latitude", longitude = "longitude", type = "default",
         xlim, ylim, pollutant = NULL, labels = NULL, cols = "default",
         limits = c(0,100), cex = pollutant, pch = NULL, cex.range =c(2,10),
         xlab = longitude, ylab = latitude, main = "", axes = TRUE,
         map = NULL, map.raster = TRUE, map.cols = NULL,
         aspect = NULL, as.table = TRUE, plot.type = "xy",
         plot.transparent = FALSE,
         key = NULL, key.position = "right",
         key.header = "", key.footer = pollutant,
         auto.text = TRUE, ...
){


    ## get rid of R check annoyances
    strip = strip.left = NULL
#googleMapsPlot
#openair flavour
#karl 2011-08-04

#to do
#########################
#same type and something else conflict
#

#uses
#RgoogleMaps MapBackground, etc.
 
    
##################
#need to confirm this has not changed
#check depends with Markus
#################
#rgdal/png depending on RgoogleMaps version
#so let RgoogleMaps handle its depends
#

##########################
#column assignment in args
###########################
#below code assumes
#col assignment by col
#######################
#could have characters or numerics?
#to allow assignment by col number
#

    ##########################
    #greyscale handling/setup
    ##########################
    if (length(cols) == 1 && cols == "greyscale") {
        #strip and special greyscale
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
        if(is.null(map.cols))
            map.cols <- "greyscale"
    }

    ##########################
    #misc set ups, checks, etc
    ##########################

    #robust args handling
    extra.args <- list(...)

    if ("fontsize" %in% names(extra.args))
        trellis.par.set(fontsize = list(text = extra.args$fontsize))

    #map panel
    map.panel <- if(map.raster)
                     panel.GoogleMapsRaster else panel.GoogleMaps

    #plot.type
    ##predefined cases
    if(is.character(plot.type) && plot.type == "xy")
        plot.type <- panel.xyplot
    if(is.character(plot.type) && plot.type == "level")
        plot.type <- panel.levelplot

    if(!is.function(plot.type)){
        warning(paste("GoogleMapsPlot did not recognise 'plot.type'",
            "\n\t[applying default]", sep=""), call.=FALSE)
        plot.type <- panel.xyplot
    }

    #pollutant only 1 allowed
    #see suggestions
    if (length(pollutant) > 1){
        warning(paste("GoogleMapsPlot only allows one 'pollutant'",
            "\n\t[ignoring all but first]", sep=""), call.=FALSE)
        pollutant <- pollutant[1]
    }

    #type upto 2 levels
    if(length(type) > 2){
        warning(paste("GoogleMapsPlot allows up to two 'type' values",
            "\n\t[ignoring all but first two]", sep=""), call.=FALSE)
        type <- type[1:2]
    }

    #labels only 1 allowed
    #can be vector or list

    if(is.list(labels)){
        temp <- labels$labels
        label2 <- if(length(temp) > 0)
                        labels$labels else NULL
    } else label2 <- if(length(labels) > 0)
                         labels else NULL

    if(length(label2) > 1)
        warning(paste("GoogleMapsPlot only allows one 'labels' source",
                "\n\t[ignoring all but first]", sep=""), call.=FALSE)
            label2 <- label2[1]

    ############################
    #checkPrep
    ############################
    #get cex, pch, labels if characters
    #using label2 incase list element

    temp <- na.omit(sapply(list(cex, pch, label2), function(x){
                      if("character" %in% is(x)) x else NA}))

    #keep date if about
    temp <- if("date" %in% names(mydata))
                c("date", pollutant, temp) else
                    c(pollutant, temp)
    #all of x, y, temp need to be handled as type here
    mydata <- checkPrep(mydata, temp, type=c(latitude, longitude, type),
                        remove.calm = FALSE)

    ############################
    #axes handling
    ############################
    if(!axes){
        temp <- list(draw = FALSE)
        extra.args$scales <- if("scales" %in% names(extra.args)){
                                 if(is.list(extra.args$scales)){
                                     listUpdate(temp, extra.args$scales)
                                 } else temp
                             } else temp
#############
#this will need changing
#if we move xlab, ylab to ...
#############
        xlab <- ""
        ylab <- ""
    }


    ############################
    #type, cutData handling
    ############################
 #   newdata <- cutData(mydata, type, ...)

    ############################
    #pollutant, cols, etc.
    ############################
    
                                        #z pollutant if set else default
    if (is.null(pollutant)) pollutant <- latitude
    z <-  mydata[[pollutant]]

    #cex.range setup
    if(is.null(cex.range))
        cex.range <- FALSE
    if(is.logical(cex.range) && cex.range)
        cex.range <- c(1,10)
    if(is.numeric(cex.range)){
        temp <- range(cex.range, na.rm = TRUE, finite = TRUE)
        cex.range <- if(length(na.omit(temp)) < 2)
                         FALSE else temp
    }

    #cex default
    if(is.null(cex)){
        cex <- if(is.numeric(cex.range)) mean(cex.range) else 1
    } else {
       if(is.character(cex)){
           cex <- as.numeric(mydata[[cex[1]]])
           temp <- range(cex, na.rm = TRUE, finite = TRUE)
           my.range <- if(length(na.omit(temp)) < 2)
                           FALSE else temp
           if(is.numeric(cex.range)){
               if(my.range[1] == my.range[2]){
                   cex[cex == my.range[1]] <- mean(cex.range)
               } else {
                   temp <- (cex.range[2]-cex.range[1]) / (my.range[2]-my.range[1])
                   cex <- cex.range[1] + (cex - my.range[1]) * temp
               }
           }
       }
    }

    #pch handling
    if(is.null(pch))
        pch <- 20
    if(is.character(pch))
        pch <- as.numeric(mydata[, pch[1]])

    #cols handling

    #belt and braces
    if(is.null(cols))
        cols <- "default"

    #if map.cols and cols same use darker range
    col.range <- if(identical(map.cols, cols))
                     openColours(cols, 156)[56:156] else openColours(cols, 101)

    #make transparent

    if(is.logical(plot.transparent) && plot.transparent)
        plot.transparent <- 0.5

    if(is.numeric(plot.transparent)){
        if(any(plot.transparent < 0) | any(plot.transparent > 1)){
            warning(paste("GoogleMapsPlot could sensibly apply requested 'plot.transparency'",
                          "\n\t[Sugest numeric in range 0 to 1]",
                          "\n\t[resetting value(s) to default, 0.5]",
                    sep=""), call.=FALSE)
            plot.transparent[plot.transparent > 1] <- 0.5
            plot.transparent[plot.transparent < 0] <- 0.5
        }
        col.range <- col2rgb(col.range)
        col.range <- rgb(col.range[1,], col.range[2,], col.range[3,],
                      alpha = plot.transparent * 255, maxColorValue = 255)
    }

    if(missing(limits)){
        breaks <- seq(min(z, na.rm = TRUE), quantile(z, probs = 0.95, na.rm = TRUE), length = 101)
            if(max(breaks, na.rm=TRUE) < max(z, na.rm = TRUE)){
                breaks <- seq(min(z, na.rm = TRUE), quantile(z, probs = 0.95, na.rm = TRUE), length = 100)
                breaks <- c(breaks, max(z, na.rm = TRUE))
        }
    } else {
        breaks <- seq(limits[1], limits[2], length = 101)
    }

    temp <- range(breaks)
    mycols <- ifelse(z <= temp[2] & z >= temp[1], z, NA)
    if(temp[1] == temp[2]){
        mycols[!is.na(mycols)] <- col.range[50]
        breaks <- do.breaks(c(temp[1] - 1, temp[1] + 1), 101)
    } else {
        mycols <- col.range[cut(mycols, c(breaks[1] - 1, breaks), labels = FALSE)]
    }

    ################
    #labels handling
    ################

    #get labels source

#note
#currently make labels even if not there
#then don't plot if labels$labels NULL
#rethink?

    if(!is.null(label2))
        label2 <- mydata[, label2]

    #default label style

    temp <- list(labels = label2, cex = 0.75, col = "red", lwd=2)
    labels <- if(is.list(labels))
                  listUpdate(temp, labels[names(labels) != "labels"]) else temp

    #check for label formatting
    temp <- labels$fun
    if(!is.null(temp) && is.function(temp))
        labels$labels <- temp(labels$labels)

    ################
    #add in drawOpenKey
    ################

    #if key null but scalable data present
    #force key
    if(is.null(key))
        if(length(unique(z))>1)
            key = TRUE

    #make legend using defaults
    legend <- list(col = col.range, at = breaks, space = key.position,
                  auto.text = auto.text, footer = key.footer, header = key.header,
                  height = 1, width = 1.5, fit = "all")
    legend <- makeOpenKeyLegend(key, legend, "trendLevel")

    ###############
    #main map call
    ###############

    #get map not suppled
    if(is.null(map)){

        #get xlims from data/local if not set
        temp.y <- if(missing(ylim))
                      mydata[, latitude] else ylim
        temp.x <- if(missing(xlim))
                      mydata[, longitude] else xlim

        #get names of args that MapBackground can handle
        temp <- unique(c(names(formals(MapBackground)),
                         names(formals(GetMap.bbox)),
                         names(formals(GetMap))))

######test fix

        temp2 <- try(qbbox(lat = temp.y, lon = temp.x), silent = TRUE)
        if(is(temp2)[1] == "try-error")
            stop(paste("\tGoogleMapsPlot could not apply supplied lat, lon combination",
                       "\n\t[check call settings and data source]", sep = ""),
                 call.=FALSE)

my.y <- diff(range(temp2$latR, na.rm=TRUE))
my.x <- diff(range(temp2$lonR, na.rm=TRUE))

#was c(640, 640)
my.size <- if(my.y > my.x)
               c(ceiling((my.x/my.y) * 640), 640) else
               c(640, ceiling((my.y/my.x) * 640))

        #override some RgoogleMaps defaults
        map <- list(lon = temp2$lonR, lat = temp2$latR, destfile = tempfile(),
                     maptype = "terrain", size = my.size)

        #catch all missing x/y dimensions
        if(my.x==0 | my.y==0){
            if(is.null(map$zoom))
                map$zoom <- 15
            map$size <- c(640,640)
        }
        if(any(is.na(map$size)))
            map$size[is.na(map$size)] <- 64
        map$size[map$size < 1] <- 64

        ##update my defaults with relevant ones in call
        map <- listUpdate(map, extra.args, subset.b = temp)

        #use MapBackground and list of allowed args
        map <- try(do.call(GetMap.bbox, map), silent = TRUE)

#get square range map

#temp.2 <- c(min(temp.y, na.rm = TRUE) + (diff(range(temp.y, na.rm=TRUE))/2),
#            min(temp.x, na.rm = TRUE) + (diff(range(temp.x, na.rm=TRUE))/2))
#zoom <- min(MaxZoom(range(temp.y, na.rm=TRUE),
#                    range(temp.x, na.rm=TRUE)),
#            na.rm=TRUE)
#map <- list(center = temp.2, zoom = zoom, destfile = "XtempX.png",
#                     maptype = "terrain")
#map <- listUpdate(map, extra.args, subset.b = temp)
#map$size <- c(640, 640)
#map <- try(do.call(GetMap, map), silent = TRUE)


        if(is(map)[1] == "try-error")
            stop(paste("\tGoogleMapsPlot could not apply supplied lat, lon and RgoogleMap args",
                       "\n\t[check call settings and data source]", sep = ""),
                 call.=FALSE)
    }

    #get xlim, ylim from map if not supplied
    #use map lims to reset borders for plot
    #(is larger than data range
    #and already done by RgoogleMaps!)

#re-enabled as part of test fix

    if(missing(xlim))
        xlim <- c(map$BBOX$ll[2], map$BBOX$ur[2])
    if(missing(ylim))
        ylim <- c(map$BBOX$ll[1], map$BBOX$ur[1])

###############
#new bit
###############
#using rescale to panel dimensions

#x, ylims
temp <- LatLon2XY.centered(map, c(map$BBOX$ll[1], map$BBOX$ur[1]),
                                c(map$BBOX$ll[2], map$BBOX$ur[2]))
xlim <- temp$newX
ylim <- temp$newY

if(is.null(extra.args$aspect))
    extra.args$aspect <- diff(ylim)/diff(xlim)


#latitude, longitude
temp <- LatLon2XY.centered(map, mydata[[latitude]],
                                mydata[[longitude]])
mydata[, longitude] <- temp$newX
mydata[, latitude] <- temp$newY

     map <- openairMapManager(map)

###############
#temp addition
###############
#while testing xlim/ylim
###############

##############
#rationalise the x,ylim?
##############

    map$xlim <- xlim
    map$ylim <- ylim

    ra <- dim(map$myTile)

    ##############
    #recolor map
    #############
    if(!is.null(map.cols)){

        #if map.cols and cols same use lighten map range
        if(identical(map.cols, cols))
            map.cols <- if(length(map.cols) == 1 && map.cols == "greyscale")
                            openColours(c("white", grey(0.65)), 10) else
                            openColours(map.cols, 7)[1:2]

        #make an intensity scale
        temp <- apply(col2rgb(map$myTile), 2, prod)

        #reset cols in frame
        map$myTile <- level.colors(temp, pretty(temp, 200), openColours(map.cols, 200))
        dim(map$myTile) <- ra[1:2]
    }


    #############################
    #plot set up
    #############################

    #xyplot formula
    myform <- paste(latitude, " ~ ", longitude, sep = "")
    if(length(type[type!="default"]) > 0)
        myform <- paste(myform, " | ", paste(type, collapse = "+"), sep = "")
    myform <- formula(myform)


#labels handling

    #labels via quickText
    main <- quickText(main, auto.text)
    xlab <- quickText(xlab, auto.text)
    ylab <- quickText(ylab, auto.text)

#check quickText space addition
#can we drop it?

    if(!is.null(labels$labels))
             labels$labels <- sapply(labels$labels, function(x){
                                                        x <- paste(" ", x, sep = "")
                                                        quickText(x, auto.text)})

#######################
#could think about moving these
#and map making outside
#plot function
#######################

#axis formatting
    gmYscale.components <- function(lim, ...){
        ans <- yscale.components.default(c(map$BBOX$ll[1], map$BBOX$ur[1]), ...)
        ans$num.limit <- map$ylim
        temp <- LatLon2XY.centered(map, ans$left$ticks$at,
                                        map$BBOX$ll[2])
        temp.2 <- LatLon2XY.centered(map, ans$left$ticks$at,
                                          map$BBOX$ur[2])
        ans$left$ticks$at <- temp$newY
        ans$left$labels$at <- temp$newY
        ans$right <- ans$left
        ans$right$ticks$at <- temp.2$newY
        ans$right$labels$at <- temp.2$newY
        ans
    }
    gmXscale.components <- function(lim, ...){
        ans <- xscale.components.default(c(map$BBOX$ll[2], map$BBOX$ur[2]), ...)
        ans$num.limit <- map$xlim
        temp <- LatLon2XY.centered(map, map$BBOX$ll[1],
                                        ans$bottom$ticks$at)
        temp.2 <- LatLon2XY.centered(map, map$BBOX$ur[1],
                                          ans$bottom$ticks$at)
        ans$bottom$ticks$at <- temp$newX
        ans$bottom$labels$at <- temp$newX
        ans$top <- ans$bottom
        ans$top$ticks$at <- temp.2$newX
        ans$top$labels$at <- temp.2$newX
        ans
    }

    ##############################
    #plot
    ##############################

    my.plot <- list(myform, data = mydata, z = z,
                  cex = cex, pch = pch, xlim = xlim, ylim = ylim,
                  col = mycols, aspect = aspect, as.table = as.table,
                  at = breaks, col.regions = col.range,
                  yscale.components = gmYscale.components,
                  xscale.components = gmXscale.components,
                  main = main, xlab = xlab, ylab = ylab, labels = labels,
                  panel = function(x, y, subscripts, at, col.regions, ...){
                                   map.panel(map)
                                   temp <- list(...)
                                   if(!is.null(subscripts)){
                                        temp <- lapply(temp, function(x)
                                        x <- if(length(x)>1) x[subscripts] else x )

                                        labels <- lapply(labels, function(x)
                                        x <- if(length(x)>1) x[subscripts] else x )

                                        subscripts <- 1:length(subscripts)
                                   }
                                   temp <- listUpdate(
                                               list(x = x, y = y, z = temp$z, at = at,
                                                    col.regions = col.regions, subscripts = subscripts),
                                               temp)
                                   do.call(plot.type, temp)


                                   labels <- listUpdate(
                                               list(x = x, y = y, subscripts = subscripts),
                                               labels)
                                   if(!is.null(labels$labels))
                                       do.call(ltext, labels)
                  }, legend = legend)
    my.plot <- listUpdate(my.plot, extra.args)
    plt <- do.call(xyplot, my.plot)

    ##############################
    #update for two levels
    ##############################
    #uses iseOuterStrips in latticeExtra
    if(length(type) > 1)
        plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)

    ##############################
    #openair output
    ##############################
    plot(plt)
    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)
    output <- list(plot = plt, data = list(data = mydata, map = map), call = match.call())
    class(output) <- "openair"
    invisible(output)
}


#################################
#RgoogleMaps version handler
#IN DEVELOPMENT
#################################
#this was introduced on introduction of
#RgoogleMaps 1.1.9.10-13
#rgdal/png dependent change
#at same time the structures map
#structures changed
#

openairMapManager <- function(map){

    #######################
    #native raster handler
    #######################

    if ("nativeRaster" %in% class(map$myTile)) {

        ## do to png native output
        tmpFile <- tempfile()
        png::writePNG(map$myTile, tmpFile)
        map$myTile <- png::readPNG(tmpFile, native = FALSE)
        attr(map$myTile, "type") <- "rgb"

    }

    #set up
    ra <- dim(map$myTile)

    #version test
    tempfun <- function(x, pck = "RgoogleMaps")
                  compareVersion(packageDescription(pck)$Version, x)

    #if RgoogleMaps version between 1.1.5 and 1.1.9.13
    #currently don't know structure
    if(tempfun("1.1.9.13") < 0){
        warning(paste("GoogleMapsPlot may not be able to support this version of 'RgoogleMaps'",
                      "\n\t[You may encounter problems]",
                      "\n\t[If so, suggest updating RgoogleMaps or contacting openair]",
                sep=""), call.=FALSE)

        #NOTE: this assumes
        #imagematrix, png
        #NOT tested for jgp
        map$myTile <- matrix(attr(map$myTile, "COL")[map$myTile],
                             nrow = ra[1], ncol = ra[2]
                      )
        map$myTile <- t(map$myTile)
        map$myTile <- map$myTile[nrow(map$myTile):1,]
        attr(map$myTile, "type") <- "openair"
        return(map)
    }

    if(length(ra) > 2){

        if(ra[3] == 4 & attr(map$myTile, "type") == "rgb"){
            map$myTile <- rgb(map$myTile[, , 1], map$myTile[, , 2],
                              map$myTile[, , 3], map$myTile[, , 4])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }

        if(ra[3] == 3 & attr(map$myTile, "type") == "rgb"){
            map$myTile <- rgb(map$myTile[, , 1], map$myTile[, , 2],
                              map$myTile[, , 3])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }

        if(ra[3] == 1 & attr(map$myTile, "type") == "grey"){
            map$myTile <- grey(map$myTile[, , 1])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }
    }


    if(length(ra) == 2){

        if(is.character(attr(map$myTile, "type")) && attr(map$myTile, "type") == "grey"){
            map$myTile <- grey(map$myTile[,])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }
    }

    warning(paste("GoogleMapsPlot encountered unexpected 'RgoogleMaps' output",
                  "\n\t[You may encounter problems or some options may not be supported]",
                  "\n\t[If so, suggest updating RgoogleMaps or contacting openair]",
            sep=""), call.=FALSE)
    return(map)

}


#################################
##panel.GoogleMapsRaster
#################################
#raster map panel
#

panel.GoogleMapsRaster <- function(map){
    grid.raster(map$myTile,
         x = unit(map$xlim[1], "native"), y = unit(map$ylim[1], "native"),
         width = unit(map$xlim[2] - map$xlim[1], "native"),
         height = unit(map$ylim[2] - map$ylim[1], "native"),
         just = c("left", "bottom")
    )
}



#################################
##panel.GoogleMaps
#################################
#non-raster map panel
#

panel.GoogleMaps <- function(map){

    #there has to be a better way

    #both the rect handling
    #and the map.col generation
    #need thinking about

    if(attr(map$myTile, "type") != "openair")
        map <- openairMapManager(map)

    ra <- dim(map$myTile)
    map.col <- map$myTile

    map.lon <- rep(seq(map$xlim[1], map$xlim[2],
                       length.out = ra[1]),
                   each = ra[2])
    map.lat <- rep(seq(map$ylim[2], map$ylim[1],
                       length.out = ra[2]),
                   time = ra[1])
    width <- (map$xlim[1] - map$xlim[2]) / ra[1]
    height <- (map$ylim[1] - map$ylim[1]) / ra[2]

    panel.rect(x = map.lon, y = map.lat,
               width = width, height = height,
               col = map.col, border = map.col)
}



