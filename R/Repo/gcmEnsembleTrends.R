
#' IPCC Style Climate Trends Plot
#'
#' @param data object storing climate data
#' @param variable climate variable to be plotted either 'tavg' or 'prcp'
#' @param hist.period historical period e.g., 1900:2005
#' @param proj.period projection period e.g., 2006:2100
#' @param ref.period  reference climate period e.g., 1985:2005
#'
#' @return
#' @export
#'
#' @examples
gcmEnsembleTrends <- function(data, variable, 
                              hist.period, proj.period, ref.period) {
  
  hist_ref <- data[["historical"]] %>% bind_rows(.id = "model") %>%
    filter(year %in% ref.period) %>% ungroup() %>%
    summarize(prcp = mean(prcp) * 12, tavg = mean(tavg))
  
  hist <- data[["historical"]] %>% bind_rows(.id = "model") %>% 
    mutate(scenario = "Historical") %>% filter(year %in% hist.period)
  
  rcp1 <- data[["rcp26"]] %>% bind_rows(.id = "model") %>% mutate(scenario = "RCP2.6")
  rcp2 <- data[["rcp45"]] %>% bind_rows(.id = "model") %>% mutate(scenario = "RCP4.5")
  rcp3 <- data[["rcp60"]] %>% bind_rows(.id = "model") %>% mutate(scenario = "RCP6.0")
  rcp4 <- data[["rcp85"]] %>% bind_rows(.id = "model") %>% mutate(scenario = "RCP8.5")
  rcp  <- bind_rows(rcp1, rcp2, rcp3, rcp4) %>% filter(year %in% proj.period)
  
  clim_abs <- bind_rows(hist, rcp) %>%
    group_by(model, scenario, year) %>%
    summarize(prcp = mean(prcp)*12, tavg = mean(tavg))
  
  clim_anm <- clim_abs %>% ungroup() %>%
    mutate(prcp = prcp/hist_ref$prcp * 100 - 100) %>%  #relative change 
    mutate(tavg = tavg - hist_ref$tavg) # increase
  
  df <- clim_anm %>%
    select(model:year, var = variable) %>%
    group_by(scenario, year) %>%
    summarize(mean = mean(var), min = quantile(var, probs = 0.05), 
              max = quantile(var, probs = 0.95))
  
  #Plotting parameters
  font_size <- 10
  rcp_col <- c("black", "#08519c", "#abd9e9", "#fe9929", "#f03b20")
  
  theme_IPCC <- theme_light() %+replace%
    theme(text = element_text(color="#444444"), 
          strip.background   = element_rect(colour="#f0f0f0", fill="#f0f0f0"), 
          plot.margin        = unit(c(10, 5, 5, 5), "mm"), 
          legend.title       = element_blank(), 
          panel.grid.major   = element_line(color="gray95", size=0.1), 
          panel.grid.minor   = element_blank(), 
          panel.border       = element_rect(colour="gray95", fill=NA, size=0.1))
  
  ylabel <- if(variable == "tavg") {
    expression("Temperature change ("* degree * C *")")
  } else {"Precipitation change (%)"}
  
  ggplot(df, mapping = aes(x=year, y=mean, group=scenario, fill=scenario)) + 
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3) +
    geom_line(aes(color=scenario), size=0.8) + 
    scale_fill_manual(values=rcp_col) + guides(fill = FALSE) +
    scale_color_manual(values=rcp_col) +
    theme_climTrends() + 
    geom_vline(xintercept = c(1950, 2000, 2050), linetype = "dotted") +
    xlab("Year") + ylab(ylabel)
  
}

