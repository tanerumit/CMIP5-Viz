


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
  
    
