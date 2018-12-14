
################################################################################
# CMIP5 Projections -- Interactive Visualization
# December, 2018
# Umit Taner
################################################################################

# Required libraries
#devtools::install_github("tanerumit/ggHydro")
#devtools::install_github("tanerumit/hydrosystems")
#devtools::install_github("cmartin/ggConvexHull")

library(ggHydro)
library(hydrosystems)

library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(highcharter)

# Source scripts
source("./R/cmip5TrendsInteractive.R")
source("./R/cmip5ScatterPlotInteractive.R")

########### Read-in CMIP5 Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Specify area name and where to save the outputs
areaName  <- "Georgia"
outputDir <- "./graphics"

# Read in CMIP5 data from the 'gcm_path' folder, write all data into a single list object 
gcm_data <- gcmDataTransform(path = "./input/GCM/", 
                             gridInfo = paste0(areaName, "_cmip5grid.xlsx"),
                             climateVars = c("prcp", "tavg"))

### Interactive Scatter plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cmip5ScatterPlotInteractive(
  data = gcm_data,
  hist.period = 1976:2005, 
  proj.period = 2066:2095, 
  tavg.breaks = seq(0,8,1), 
  prcp.breaks = seq(-40,40,10))

### Interactive Climate Trends Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cmip5TrendsInteractive(
  data = gcm_data, 
  variable = "tavg", 
  hist.period = 1976:2005, 
  proj.period = 2006:2095)
