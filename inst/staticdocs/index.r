sd_section(
  "Package overview",
  "General information about the openair package",
  c("openair-package")
)

sd_section(
  "Data",
  "Data that comes with openair",
  c("mydata")
)

sd_section(
  "Import functions",
  "These functions are used to import data either from csv-type files or from web servers",
  c("import", "importKCL", "importAURN", "importAirbase", "importSAQN", "importMeta", "airbaseFindCode", "airbaseInfo", "airbaseStats", "airbaseSplice", "importADMS")
)

sd_section(
  "Main functions",
  "These are the core functions in openair",
  c("calendarPlot", "linearRelation", "kernelExceed","GoogleMapsPlot", "percentileRose", "polarAnnulus", "polarCluster", "polarFreq", "polarPlot", "scatterPlot", "smoothTrend", "summaryPlot", "TheilSen", "timePlot", "timeProp", "timeVariation", "trendLevel", "windRose")
)

sd_section(
  "Utility functions",
  "Functions to help work with data e.g. selecting date periods and averaging data to different time resolutions",
  c("aqStats", "calcPercentile", "corPlot", "cutData", "openColours", "quickText", "rollingMean", "selectByDate", "selectRunning", "splitByDate", "timeAverage")
)

sd_section(
  "Functions for model evaluation",
  "These functions focus on the evaluation of air quality models and offer both numerical and graphical approaches",
  c("modStats", "conditionalEval", "conditionalQuantile", "TaylorDiagram")
)

sd_section(
  "Functions for working with back trajectories",
  "These functions deal with working with back trajectories, mostly related to teh NOAA Hysplit model",
  c("importTraj", "trajCluster", "trajPlot", "trajLevel")
)

