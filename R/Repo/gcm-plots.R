
################################################################################
# CMIP5 Projections -- Visualization
# December, 2018
# Umit Taner
################################################################################

devtools::install_github("tanerumit/ggHydro")
devtools::install_github("tanerumit/hydrosystems")
devtools::install_github("cmartin/ggConvexHull")

# Required libraries
library(ggHydro)
library(hydrosystems)
library(ggConvexHull)


area_name     <- "Georgia"
gcm_data_path <- "./input/nenskra/GCM/"
output_path   <- "./input"

gcm_data <- gcmDataTransform(
  path = gcm_data_path, 
  gridInfo = paste0(area_name, "_cmip5grid.xlsx"),
  climateVars = c("prcp", "tavg"))

############################ SCATTER PLOT OF GCM PROJECTIONS 

# 1) Scatter plot of mean temp & precip changes 
p <- gcmScatterPlot(data = gcm_data, 
  hist.period = 1976:2005, 
  proj.period = 2066:2095, 
  scenarios = c("rcp26", "rcp85"), 
  tavg.breaks = seq(0,8,1), 
  prcp.breaks = seq(-50,50,10), 
  save = FALSE, 
  plot_historical = TRUE, 
  plot.title = TRUE, 
  chull = FALSE) 

pg <- ggplot_build(p)

legend_key <- data.frame(colour = unique(pg$data[[1]]["colour"]), 
              label = pg$plot$scales$scales[[1]]$labels)

gcm_delta <- layer_data(p, 1) %>% 
  select(colour, x, y) %>%
  left_join(legend_key, by = "colour") %>%
  select(label, tavg = x, precip = y) %>%
  as_tibble()

write_csv(gcm_delta, "./input/gcm_proj_future(2066_2095)_hist(1976_2005).csv")

p <- p + theme_climSurface(font_size = 11); p 
ggsave(filename = "./gcmproj_scatterplot3.jpeg", width = 7, height = 7)

#-------------------------------------------------------------------------------

# IPCC STYLE GCM-PROJECTIONS ---------------------------------------------------

hist_clim_data <- read_csv("C:/Users/taner/Dropbox/Research/Projects/02_Nenskra/Data/Climate data/meteo_Nenskra.csv")

hist_clim_daily <- hist_clim_data %>%
  mutate(date = as.Date(X1, "%m/%d/%Y")) %>%
  mutate(year = year(date), month = month(date), day = day(date)) %>%
  select(year, month, day, prcp = 2, tavg = 3, tmax = 4, tmin = 5)
  
hist_clim_annual <- hist_clim_daily %>% group_by(year) %>% 
  summarize(prcp = sum(prcp, na.rm = TRUE), 
            tavg = mean(tavg),
            tmin = mean(tmin, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE)) 


font_size <- 10
scn_colors <- c("black", "blue", "red")


GCMtrends <- function(data = gcm_data,
  climvar = "tavg",
  hist.period = 1900:2005,
  proj.period = 2006:2100,
  scenarios = c("RCP 2.6", "RCP 8.5")) 
  
  {
  
  ts1 <- hist_clim_annual %>% mutate(scenario = "Historical")
  
  dfs <- lapply(names(data), function(x) data[[x]] %>% bind_rows(.id = "model") %>% 
    mutate(scenario = x)) %>% bind_rows() %>%
    mutate(scenario = factor(scenario, levels = c("historical", "rcp26", "rcp45", "rcp60", "rcp85"),
                             labels = c("Historical", "RCP 2.6", "RCP 4.5", 'RCP 6.0', "RCP 8.5")))
  df1 <- dfs %>%
    filter(ifelse(scenario == "Historical", year %in% hist.period, year %in% proj.period)) %>%
    filter(scenario %in% scenarios) %>%
    select(scenario, model, year, mon, value = climvar)

  df2 <- df1 %>%
    group_by(scenario, model, year) %>%
    summarize(value = mean(value)) %>%
    group_by(scenario, year) %>%
    summarize(mean = median(value), min = min(value), max = max(value),
              q25 = quantile(value, probs = 0.10), 
              q75 = quantile(value, probs = 0.90))
    
  ### Baseline plot
  p0 <- ggplot(df2, mapping = aes(x = year, y = mean, group = scenario, fill = scenario)) + 
    geom_line(aes(color = scenario), size = 0.8) + 
    geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.15) +
    scale_fill_manual(values = scn_colors) +
    scale_color_manual(values = scn_colors) +
    theme_climTrends() +
    geom_line(data = ts1, aes(x = year, y = tavg + 10), size = 2, color = "blue")

  p1 <- p0 +
    geom_vline(xintercept = c(1950, 2000, 2050), linetype = "dotted", color = "gray50") +
    scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 50)) +
    scale_y_continuous(limits = c(4,22), breaks = seq(5,20,5))


### Temperature trends
p1 <- p0 %+% df_tavg +
  labs(x = NULL, y =  expression("Temperature ("* degree * C *")")) +
  

p1 + 

p1b <- p1 %+% filter(df_tavg, scenario == "Historical") 

ggsave(plot = p1, filename = "./graphics/temp_trends_hist.png", height = 7, width = 8, dpi = 300)
ggsave(plot = p1b, filename = "./graphics/temp_trends_proj.png", height = 7, width = 8, dpi = 300)

### Precip. trends
p2 <- p0 %+% df_prcp +
  labs(x = NULL, y =  'Precipitation ("mm")') +
  geom_vline(xintercept = c(1950, 2000, 2050), linetype = "dotted", color = "gray50") +
  scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 50)) +
  scale_y_continuous(limits = c(15,128), breaks = seq(25,125,25))

p2b <- p2 %+% filter(df_prcp, scenario == "Historical") 

ggsave(plot = p2, filename = "./graphics/prcp_trends_hist.png", height = 7, width = 8, dpi = 300)
ggsave(plot = p2b, filename = "./graphics/prcp_trends_proj.png", height = 7, width = 8, dpi = 300)

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
# (EXPERIMENTAL) SCATTER PLOT /w uncertainty bounds

p <- gcmScatterPlot(data = gcm_data, save = FALSE, 
  scenarios = c("rcp26", "rcp45", "rcp60", "rcp85"),
  hist.period = 1971:2000, proj.period = 2016:2045, 
  tavg.breaks = seq(0,8,1), prcp.breaks = seq(-50,50,10), 
  chull = TRUE, plot.title = FALSE)



