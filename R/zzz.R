.onLoad <- function(...) {

  ##  packageStartupMessage("\ttype citation(\"openair\") for how to cite openair")
  utils::globalVariables(c("variable", "value", "res", "site_pol", "code",
                           "latitude", "longitude", "site.type",
                           ".", "data", "fit", "grp", "intercept", "out",
                           "seslope", "slope", "weekday", "cuts", "empty",
                           "lat", "lon", "traj_len", "wkday"))
}
