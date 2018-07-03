##' openair colours
##'
##' Pre-defined openair colours and definition of user-defined colours
##'
##' This in primarily an internal openair function to make it easy for users to
##' select particular colour schemes, or define their own range of colours of a
##' user-defined length.
##'
##' Each of the pre-defined schemes have merits and their use will depend on a
##' particular situation. For showing incrementing concentrations e.g. high
##' concentrations emphasised, then "default", "heat", "jet" and "increment" are
##' very useful. See also the description of \code{RColorBrewer} schemes for the
##' option \code{scheme}.
##'
##' To colour-code categorical-type problems e.g. colours for different
##' pollutants, "hue" and "brewer1" are useful.
##'
##' When publishing in black and white, "greyscale" is often convenient.  With
##' most openair functions, as well as generating a greyscale colour gradient,
##' it also resets strip background and other coloured text and lines to
##' greyscale values.
##'
##' Failing that, the user can define their own schemes based on R colour names.
##' To see the full list of names, type \code{colors()} into R.
##'
##' @param scheme The pre-defined schemes are "increment", "default", "brewer1",
##'   "heat", "jet", "hue", "greyscale", or a vector of R colour names e.g.
##'   c("green", "blue"). It is also possible to supply colour schemes from the
##'   \code{RColorBrewer} package. This package defines three types of colour
##'   schemes: sequential, diverging or qualitative. See
##'   \url{http://colorbrewer2.org} for more details concerning the orginal work
##'   on which this is based.
##'
##'   Simplified versions of the \code{viridis} colours are also available.
##'   These include "viridis", "plasma", "magma", "inferno" and "cividis".
##'
##'   Sequential colours are useful for ordered data where there is a need to
##'   show a difference between low and high values with colours going from
##'   light to dark. The pre-defined colours that can be supplied are: "Blues",
##'   "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
##'   "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr",
##'   "YlOrRd".
##'
##'   Diverging palettes put equal emphasis on mid-range critical values and
##'   extremes at both ends of the data range. Pre-defined values are: "BrBG",
##'   "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
##'
##'   Qualitative palettes are useful for differentiating between categorical
##'   data types. The pre-defined schemes are "Accent", "Dark2", "Paired",
##'   "Pastel1", "Pastel2", "Set1", "Set2", "Set3".
##'
##'   A colorblind safe pallette "cbPalette" is available based on the work of:
##'   http://jfly.iam.u-tokyo.ac.jp/color/
##'
##'   Note that because of the way these schemes have been developed they only
##'   exist over certain number of colour gradations (typically 3--10) --- see
##'   ?\code{brewer.pal} for actual details. If less than or more than the
##'   required number of colours is supplied then \code{openair} will
##'   interpolate the colours.
##' @param n number of colours required.
##' @export
##' @import RColorBrewer
##' @return Returns colour values - see examples below.
##' @author David Carslaw
##' @references \url{http://colorbrewer2.org}
##' @keywords methods
##' @examples
##'
##' # to return 5 colours from the "jet" scheme:
##' cols <- openColours("jet", 5)
##' cols
##'
##' # to interpolate between named colours e.g. 10 colours from yellow to
##' #  green to red:
##' cols <- openColours(c("yellow", "green", "red"), 10)
##' cols
##'
##' 
openColours <- function(scheme = "default", n = 100) {

  ## pre-defined brewer colour palletes sequential, diverging, qualitative
  brewer.col <- c(
    "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
    "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
    "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral",
    "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"
  )
  ## max colours allowed

  brewer.n <- c(rep(9, 18), rep(9, 9), c(8, 8, 12, 9, 8, 9, 8, 12))

  ## predefined schemes
  schemes <- c("increment", "default", "brewer1", "heat", "jet", "hue", 
               "greyscale", brewer.col, "cbPalette", "viridis", "magma",
               "inferno", "plasma", "cividis")


  ## schemes
  heat <- colorRampPalette(brewer.pal(9, "YlOrRd"), interpolate = "spline")

  viridis <- colorRampPalette(c(
    "#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", 
    "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF"
  ))
  
  inferno <- colorRampPalette(c(
    "#000004FF", "#1B0C42FF", "#4B0C6BFF", "#781C6DFF", "#A52C60FF", "#CF4446FF",
    "#ED6925FF", "#FB9A06FF", "#F7D03CFF", "#FCFFA4FF"
  ))
  
  magma <- colorRampPalette(c(
    "#000004FF", "#180F3EFF", "#451077FF", "#721F81FF", "#9F2F7FFF", "#CD4071FF",
    "#F1605DFF", "#FD9567FF", "#FEC98DFF", "#FCFDBFFF"
  ))
  
  plasma <- colorRampPalette(c(
    "#0D0887FF", "#47039FFF", "#7301A8FF", "#9C179EFF", "#BD3786FF", "#D8576BFF",
    "#ED7953FF", "#FA9E3BFF", "#FDC926FF", "#F0F921FF"
  ))
  
  cividis <- colorRampPalette(c(
    "#00204DFF", "#00336FFF", "#39486BFF", "#575C6DFF", "#707173FF", "#8A8779FF",
    "#A69D75FF", "#C4B56CFF", "#E4CF5BFF", "#FFEA46FF"
  ))
  
  
  jet <- colorRampPalette(c(
    "#00007F", "blue", "#007FFF", "cyan",
    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"
  ))

  default.col <- colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")



  ## for this pallete use specfified number if possible - because it has been thought about...
  brewer1 <- function(n) {
    if (n >= 3 & n <= 9) {
      brewer.pal(n, "Set1")
    } else {
      thefun <- suppressWarnings(colorRampPalette(brewer.pal(9, "Set1"), interpolate = "spline"))
      thefun(n)
    }
  }

  ## for this pallete use specfified number if possible - because it has been thought about...
  find.brewer <- function(thecol, n) {
    n.brew <- brewer.n[scheme == brewer.col]

    if (n >= 3 & n <= n.brew) {
      brewer.pal(n, thecol)
    } else {
      thefun <- suppressWarnings(colorRampPalette(brewer.pal(n.brew, thecol), interpolate = "spline"))
      thefun(n)
    }
  }

  increment <- colorRampPalette(c(
    "#B0FFF1", "#9CFFC7", "#87FF8E", "#A0FF73",
    "#B4FF69", "#CCFF60", "#E7FF56", "#FFF84D", "#FFCB46", "#FF9C40",
    "#FF6939", "#FF3333", "#CC1B62", "#990A7C", "#520066"
  ))

  h <- c(0, 360) + 15
  l <- 65
  c <- 100

  if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
  }

  hue <- grDevices::hcl(
    h = seq(h[1], h[2], length = n),
    c = c,
    l = l
  )

  greyscale <- grey(seq(0.9, 0.1, length = n))

  # The palette with grey:
  cbPalette <- function(n) {
    
    cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
              "#D55E00", "#CC79A7")
    
    if (n >= 1 && n < 9) {
      
      cols <- cols[1:n] 
      
    } else {
    
      warning("Too many colours selected. Should be 1 to 8.")
    }
  }
    
  
  
  ## error catcher
  if (length(scheme) == 1) {
    if (scheme %in% brewer.col) cols <- find.brewer(scheme, n)
    if (scheme == "increment") cols <- increment(n)
    if (scheme == "default") cols <- rev(default.col(n))
    if (scheme == "brewer1") cols <- brewer1(n)
    if (scheme %in% brewer.col) cols <- find.brewer(scheme, n)
    if (scheme == "heat") cols <- heat(n)
    if (scheme == "jet") cols <- jet(n)
    if (scheme == "viridis") cols <- viridis(n)
    if (scheme == "magma") cols <- magma(n)
    if (scheme == "inferno") cols <- inferno(n)
    if (scheme == "plasma") cols <- plasma(n)
    if (scheme == "cividis") cols <- cividis(n)
    if (scheme == "hue") cols <- hue
    if (scheme == "greyscale") cols <- greyscale
    if (scheme == "cbPalette") cols <- cbPalette(n)
  }

  if (!any(scheme %in% schemes)) { # assume user has given own colours
    if (length(scheme) > 1) { ## interpolate
      user.cols <- colorRampPalette(scheme)
      cols <- user.cols(n)
    } else {
      cols <- rep(scheme, n)
    }
  }

  cols
}
