.onLoad <- function(...) {

  ##  packageStartupMessage("\ttype citation(\"openair\") for how to cite openair")
  utils::globalVariables(c("variable", "value", "res", "site_pol", "code",
                           "latitude", "longitude", "site.type", "temp",
                           ".", "data", "fit", "grp", "intercept", "out",
                           "seslope", "slope", "weekday", "cuts", "empty",
                           "lat", "lon", "traj_len", "wkday", "date_end",
                           "validity", "process", "parameter", "date_start",
                           "site", "where", "ratified_to", "site_type", "start_date",
                           "u", "v", "period", "z", "wd", "cluster", ".id", "weights",
                           "NOXasNO2.mean", "NOXasNO2.capture", "uka_code"))
}
