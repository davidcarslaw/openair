#' Pre-defined openair colours and definition of user-defined colours
#'
#' This in primarily an internal openair function to make it easy for users to
#' select particular colour schemes, or define their own range of colours of a
#' user-defined length.
#'
#' @section Schemes:
#'
#'   The following schemes are made available by `openColours()`:
#'
#'   **Sequential Colours:**
#'
#'   * "default", "increment", "brewer1", "heat", "jet", "turbo", "hue",
#'   "greyscale".
#'
#'   * Simplified versions of the `viridis` colours: "viridis", "plasma",
#'   "magma", "inferno", "cividis", and "turbo".
#'
#'   * Simplified versions of the `RColorBrewer` sequential palettes: "Blues", "BuGn",
#'   "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
#'   "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
#'
#'   **Diverging Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` diverging palettes: "BrBG",
#'   "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
#'
#'   **Qualitative Palettes:**
#'
#'   * Simplified versions of the `RColorBrewer` qualitative palettes:
#'   "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3".
#'
#'   * "cbPalette", a colour-blind safe palette based on the work of
#'   <https://www.nature.com/articles/nmeth.1618>
#'
#'   **UK Government Palettes:**
#'
#'   * "daqi" and "daqi.bands", the colours associated with the UK daily air quality index; "daqi" (a palette of 10 colours, corresponding to each
#'   index value) or "daqi.bands" (4 colours, corresponding to each band - Low,
#'   Moderate, High, and Very High). These colours were taken directly from
#'   <https://uk-air.defra.gov.uk/air-pollution/daqi> and may be useful in
#'   figures like [calendarPlot()].
#'
#'   * "gaf.cat", "gaf.focus" and "gaf.seq", colours recommended by the UK Government Analysis function
#'   (<https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>).
#'   "gaf.cat" will return the 'categorical' palette (max 6 colours),
#'   "gaf.focus" the 'focus' palette (max 2 colours), and "gaf.seq" the
#'   'sequential' palette.
#'
#' @section Details:
#'
#'   Because of the way many of the schemes have been developed they only exist
#'   over certain number of colour gradations (typically 3--10) --- see
#'   `?brewer.pal` for actual details. If less than or more than the required
#'   number of colours is supplied then `openair` will interpolate the colours.
#'
#'   Each of the pre-defined schemes have merits and their use will depend on a
#'   particular situation. For showing incrementing concentrations, e.g., high
#'   concentrations emphasised, then "default", "heat", "jet", "turbo", and
#'   "increment" are very useful. See also the description of `RColorBrewer`
#'   schemes for the option `scheme`.
#'
#'   To colour-code categorical-type problems, e.g., colours for different
#'   pollutants, "hue" and "brewer1" are useful.
#'
#'   When publishing in black and white, "greyscale" is often convenient.  With
#'   most openair functions, as well as generating a greyscale colour gradient,
#'   it also resets strip background and other coloured text and lines to
#'   greyscale values.
#'
#'   Failing that, the user can define their own schemes based on R colour
#'   names. To see the full list of names, type [colors()] into R.
#'
#' @param scheme Any one of the pre-defined `openair` schemes (e.g.,
#'   `"increment"`) or a user-defined palette (e.g., `c("red", "orange",
#'   "gold")`). See `?openColours` for a full list of available schemes.
#' @param n number of colours required.
#' @export
#' @return A character vector of hex codes
#' @author David Carslaw
#' @author Jack Davison
#' @references \url{https://colorbrewer2.org/}
#' @references \url{https://uk-air.defra.gov.uk/air-pollution/daqi}
#' @references \url{https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/}
#' @examples
#'
#' # to return 5 colours from the "jet" scheme:
#' cols <- openColours("jet", 5)
#' cols
#'
#' # to interpolate between named colours e.g. 10 colours from yellow to
#' #  green to red:
#' cols <- openColours(c("yellow", "green", "red"), 10)
#' cols
#'
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
  schemes <- c(
    "increment", "default", "brewer1", "heat", "jet", "hue",
    "greyscale", brewer.col, "cbPalette", "viridis", "magma",
    "inferno", "plasma", "cividis", "turbo", "daqi", "daqi.bands",
    "gaf.cat", "gaf.seq", "gaf.focus"
  )

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

  turbo <- colorRampPalette(c(
    "#30123BFF", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF", "#72FE5EFF",
    "#C7EF34FF", "#FABA39FF", "#F66B19FF", "#CB2A04FF", "#7A0403FF"
  ))

  gaf_ramp <- colorRampPalette(c(
    "#12436D", "#2073BC", "#6BACE6"
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
    cols <- c(
      "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
      "#D55E00", "#CC79A7"
    )

    if (n >= 1 && n < 9) {
      cols <- cols[1:n]
    } else {
      cli::cli_abort(
        c(
          "!" = "Too many colours selected for {.code {scheme}}.",
          "i" = "{.code n} should be between 1 and 8."
        ),
        call = NULL
      )
    }
  }

  # Defra's DAQI Colours
  daqi_pal <- function(n, extent) {
    if (extent == "i") {
      cols <- c(
        "#9CFF9C", "#31FF00", "#31CF00", "#FFFF00", "#FFCF00",
        "#FF9A00", "#FF6464", "#FF0000", "#990000", "#CE30FF"
      )
      max <- 10
    } else if (extent == "b") {
      cols <- c("#009900", "#ff9900", "#ff0000", "#990099")
      max <- 4
    }

    if (n >= 1 && n <= max) {
      cols <- cols[1:n]
    } else {
      cli::cli_abort(
        c(
          "!" = "Too many colours selected for {.code {scheme}}.",
          "i" = "{.code n} should be between 1 and {max}."
        ),
        call = NULL
      )
    }
  }

  gaf_pal <- function(n, extent) {
    if (extent == "c") {
      cols <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3D3D3D", "#A285D1")
      max <- 6
    } else if (extent == "f") {
      cols <- c("#BFBFBF", "#12436D")
      max <- 2
    }

    if (n >= 1 && n <= max) {
      cols <- cols[1:n]
    } else {
      cli::cli_abort(
        c(
          "!" = "Too many colours selected for {.code {scheme}}.",
          "i" = "{.code n} should be between 1 and {max}."
        ),
        call = NULL
      )
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
    if (scheme == "turbo") cols <- turbo(n)
    if (scheme == "viridis") cols <- viridis(n)
    if (scheme == "magma") cols <- magma(n)
    if (scheme == "inferno") cols <- inferno(n)
    if (scheme == "plasma") cols <- plasma(n)
    if (scheme == "cividis") cols <- cividis(n)
    if (scheme == "hue") cols <- hue
    if (scheme == "greyscale") cols <- greyscale
    if (scheme == "cbPalette") cols <- cbPalette(n)
    if (scheme == "daqi") cols <- daqi_pal(n, extent = "i")
    if (scheme == "daqi.bands") cols <- daqi_pal(n, extent = "b")
    if (scheme == "gaf.cat") cols <- gaf_pal(n, extent = "c")
    if (scheme == "gaf.focus") cols <- gaf_pal(n, extent = "f")
    if (scheme == "gaf.seq") cols <- gaf_ramp(n)
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
