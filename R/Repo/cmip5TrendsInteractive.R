
#' Title
#'
#' @param data 
#' @param variable 
#' @param hist.period 
#' @param proj.period 
#' @param ref.period 
#'
#' @return
#' @export
#'
#' @examples
cmip5TrendsInteractive <- function(data = NULL, variable = NULL, 
  hist.period = NULL, proj.period = NULL, ref.period = 1986:2005)
  
{

  rcp_col <- c("black", "#08519c", "#abd9e9", "#fe9929", "#f03b20")
  
  
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
  
  
  df1 <- df %>% select(scenario, x = year, low = min, high = max) %>%
    mutate(low = round(low,2), high = round(high,2))
  
  df2 <- df %>% select(scenario, x = year, y = mean) %>%
    mutate(y = round(y, 2))
  
  highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_chart(borderWidth = 0.1) %>%
    hc_add_series(df1, type = "arearange", lineWidth = 0.1,
                  hcaes(color = "scenario", group = "scenario"),
                  fillOpacity = 0.2, showInLegend = F) %>%
    hc_add_series(df2, type = "line",
                  hcaes(color = "scenario", group = "scenario")) %>%
    hc_colors(rcp_col) %>%
    hc_legend(align = "right", verticalAlign = "top", 
              layout = "vertical", x = 0, y = 50) %>%
    hc_tooltip(formatter = JS("function(){
       return (
       ' Scenario: ' + this.point.scenario +
       ' <br> Year: ' + this.x +
       ' <br> Value: ' + this.y
     )}"),
               borderWidth = 2) %>%
    hc_exporting(enabled = T)

}