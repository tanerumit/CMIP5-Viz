
################################################################################
# CMIP5 Projections -- Visualization
# December, 2018
# Umit Taner
################################################################################

# Required libraries
#devtools::install_github("tanerumit/ggHydro")
#devtools::install_github("tanerumit/hydrosystems")
#devtools::install_github("cmartin/ggConvexHull")

library(ggHydro)
library(hydrosystems)
library(ggConvexHull)

library(lubridate)
library(readr)
library(dplyr)
library(tidyr)

########### Read-in CMIP5 Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Specify area name and where to save the outputs
areaName  <- "Georgia"
outputDir <- "./graphics"

# Read in CMIP5 data from the 'gcm_path' folder, write all data into a single list object 
gcm_data <- gcmDataTransform(path = "./input/GCM/", 
  gridInfo = paste0(areaName, "_cmip5grid.xlsx"),
  climateVars = c("prcp", "tavg"))


########### Static scatter plot of CMIP5 Projections ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use the custom function 'gcmScatterPlot()' to plot selected RCP scenarios.
# Specify historical & future period for plotting

# 1) Scatter plot of mean temp & precip changes 
p <- gcmScatterPlot(data = gcm_data, 
                    plot.title = FALSE,
                    hist.period = 1976:2005, 
                    proj.period = 2066:2095, 
                    scenarios = c("rcp26", "rcp45", "rcp60", "rcp85"),
                    tavg.breaks = seq(0,7,1), 
                    prcp.breaks = seq(-40,40,10), output.df = TRUE)

cmip5_delta <- p[[2]]
p_cmip5_scatter <- p[[1]] + theme_climSurface(font_size = 11)
ggsave(paste0(outputDir, "./gcmproj_scatterplot3.tiff"), width=7, height=7)


# IPCC STYLE GCM-PROJECTIONS ---------------------------------------------------

p <- gcmEnsembleTrends(data = data, 
  hist.period = 1950:2005, proj.period = 2006:2100, ref.period  = 1986:2005,
  variable = "tavg")
ggsave(paste0(outputDir, "./gcmproj_trends_tavg.tiff"), width=8.2, height=5)

p <- gcmEnsembleTrends(data = data, 
   hist.period = 1950:2005, proj.period = 2006:2100, ref.period  = 1986:2005,
   variable = "prcp")
ggsave(paste0(outputDir, "./gcmproj_trends_prcp.tiff"), width=8.2, height=5)

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# ANIMATED SCATTER PLOT OF GCM PROJECTIONS -------------------------------------

### Plot GCM-means (animated moving-averages)
projp_beg <- 2015
projp  <- lapply(seq(0,70,5), function(x) seq(projp_beg + x, projp_beg + 10 + x, 1))

for (x in 1: length(projp)) {
  
  p <- gcmScatterPlot(data = gcm_data, save = FALSE, 
  scenarios = c("rcp26", "rcp85"),
  hist_period = 1940:2000, 
  proj_period = projp[[x]],
  tavg_axis_breaks = seq(0,8,1), 
  prcp_axis_breaks = seq(-40,40,10))  

  p <- p + ggtitle(paste0("Year: ",projp[[x]][6])) + theme_climSurface(font_size = 11) +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  gg_name <- paste0("./graphics/gcm-animated/gcm_window_",x + 10,".png")
  ggsave(filename = gg_name, height = 7, width = 8)
  
}

### Prepare animated gif
setwd("./graphics/gcm-animated")
system('"C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe" -delay 30 *.png -delay 50 gcm_window_25.png gcm_animate.gif')
file.remove(list.files(pattern="gcm_window_"))
setwd(workdir)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
## Interactive Scatter Plot 

gcmScatterInteractive(data = gcm_data,
                      hist.period = 1976:2005, 
                      proj.period = 2066:2095, 
                      tavg.breaks = seq(0,8,1), 
                      prcp.breaks = seq(-40,40,10))


