#' Polar plots considering changes in concentrations between two time periods
#'
#' This function provides a way of showing the differences in concentrations
#' between two time periods as a polar plot. There are several uses of this
#' function, but the most common will be to see how source(s) may have changed
#' between two periods.
#'
#' While the function is primarily intended to compare two time periods at the
#' same location, it can be used for any two data sets that contain the same
#' pollutant. For example, data from two sites that are close to one another, or
#' two co-located instruments.
#'
#' The analysis works by calculating the polar plot surface for the
#' \code{before} and \code{after} periods and then subtracting the \code{before}
#' surface from the \code{after} surface.
#'
#' @param before A data frame that represents the "before" case. See
#'   \code{\link{polarPlot}} for details of different input requirements.
#' @param after A data frame that represents the "after" case. See
#'   \code{\link{polarPlot}} for details of different input requirements.
#' @param pollutant The pollutant to analyse.
#' @param x The variable used for the radial axis (default = "ws").
#' @param limits The colour scale limits e.g. \code{limits = c(-10, 10)}.
#' @param plot Should a plot be produced? \code{FALSE} can be useful when
#'   analysing data to extract plot components and plotting them in other
#'   ways.
#' @inheritDotParams polarPlot -pollutant -x -limits -plot
#' @family polar directional analaysis functions
#' @return an [openair][openair-package] plot.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' before_data <- selectByDate(mydata, year = 2002)
#' after_data <- selectByDate(mydata, year = 2003)
#'
#' polarDiff(before_data, after_data, pollutant = "no2")
#'
#' # with some options
#' polarDiff(before_data, after_data, pollutant = "no2", cols = "RdYlBu", limits = c(-20, 20))
#'
#' }
polarDiff <- function(before, after, pollutant = "nox",
                      x = "ws",
                      limits = NA,
                      plot = TRUE, ...) {

  # extra args setup
  Args <- list(...)

  # check variables exists
  before <- checkPrep(before, c(x, "wd", pollutant),
                      "default", remove.calm = FALSE)

  after <- checkPrep(after, c(x, "wd", pollutant),
                     "default", remove.calm = FALSE)

  # need to pass on use limits only to final plot
  Args$new_limits <- limits
  Args$limits <- NA

  before <- mutate(before, period = "before")
  after <- mutate(after, period = "after")

  all_data <- bind_rows(before, after)

  polar_plt <- polarPlot(all_data,
                      pollutant = pollutant,
                      x = x,
                      type = "period",
                      plot = FALSE,
                      ...)

  polar_data <- pivot_wider(polar_plt$data,
                            id_cols = u:v,
                            names_from = period,
                            values_from = z) %>%
    mutate({{ pollutant }} := after - before,
           {{ x }} := (u ^ 2 + v ^ 2) ^ 0.5,
           wd = 180 * atan2(u, v) / pi,
           wd = ifelse(wd < 0, wd + 360, wd))

  # other arguments
  Args$cols <- if ("cols" %in% names(Args)) {
    Args$cols
  } else {
    c("#002F70", "#3167BB", "#879FDB", "#C8D2F1", "#F6F6F6",
      "#F4C8C8", "#DA8A8B", "#AE4647", "#5F1415")
  }

  lims_adj <- pretty(seq(0, max(abs(polar_data[[pollutant]]), na.rm = TRUE), 5))
  lims_adj <- lims_adj[length(lims_adj) - 1]


  Args$limits <- if (is.na(Args$new_limits[1])) {
    c(-lims_adj, lims_adj)
  } else {
    Args$new_limits
  }


  polarPlot(polar_data, pollutant = pollutant,
            x = x, plot = plot,
            cols = Args$cols,
            limits = Args$limits,
            force.positive = FALSE)

  output <- list(data = polar_data, call = match.call())

  invisible(output)

}

