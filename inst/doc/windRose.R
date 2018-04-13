## ----setup, include=FALSE------------------------------------------------
# This vignette performs two purposes.
# (1) Provide a detailed description of the nuiances of the openair package for educational purposes.
# (2) Each example can doubles as a unit test.  When the vignette is knitted, it is easy to tell if and where the code breaks.

knitr::opts_chunk$set(echo = TRUE)

## ----basic windrose------------------------------------------------------
library(openair)
windRose(mydata)

## ----wind direction windrose---------------------------------------------
library(openair)
testData <- data.frame(ws = c(1,1), wd = c(90,90))
windRose(testData, 
         breaks = 1,                     # breaks = 1 disables wind speed intervals
         angle = 30,                     # sets the angle of the spoke to 15 degrees
         paddle = FALSE,                 # draws spokes as wedges
         annotate = FALSE,               # removes default annotations
         col = "blue",                   # draw a blue spoke  
         grid.line = list(max = 100),    # limits grid lines to 100%
         key = FALSE,                    # remove the key  
         main = "Test Wind Rose")

 
