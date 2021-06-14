library(lubridate)
library(patchwork)

# Load necessary data 
source('R/Figure3_clComparison.R')

plotTimeseries <- function(hydroid, dist = 10000) {
  # Filter for river
  river = wqp.catchments %>% 
    filter(hydroID_GLAHF == hydroid) %>% 
    mutate(month = month(ActivityStartDate))
  name = river$Name[1]
  # 
  # Points close to outlet
  outletPoint = tributary_sf %>% filter(hydroID_GLAHF == hydroid) %>% slice(1)
  urban = round(outletPoint$urban, 1)
  
  Outlet = river %>% mutate(distanceOutlet = river %>% st_distance(outletPoint, by_element = TRUE)) %>% 
    filter(distanceOutlet < units::set_units(dist, "m")) %>% 
    filter(Result < quantile(Result, 0.99))
  
  # Get river name
  riverName = catchmentNames %>% filter(hydroID_GLAHF == hydroid) %>% pull(Name)
  
  p2 = ggplot(Outlet, aes(x = ActivityStartDate, y = Result)) +
    geom_point(size = 0.8, shape = 21, fill = '#c1d6e3', alpha = 0.8, stroke = 0.1) +
    # ylab('Chloride (mg/L)') +
    ylab(bquote(Chloride ~ (mg~L^-1))) +
    # scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0.5,12.5)) +
    labs(title = paste0(riverName,', < ',dist/1000,' km from outlet, ',urban,'% urban')) +
    theme_bw(base_size = 8) +
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_blank())
  
  return(p2) 
}
plotTimeseries(2109547, dist = 5000)

fox = plotTimeseries(2109413, dist = 5000) # Fox River
grand = plotTimeseries(2109493, dist = 20000) # Grand Creek
stj = plotTimeseries(2109534) # St. Joseph's River
men = plotTimeseries(2109298, dist = 5000) # Menominee River
musk = plotTimeseries(2109488, dist = 30000) # Muskegon River
kal = plotTimeseries(2109506, dist = 10000) # Kalamazoo River
man = plotTimeseries(2109428, dist = 5000) # Manistee River
manq = plotTimeseries(2109142, dist = 10000) # Manistique River

milw = plotTimeseries(2109496, dist = 5000)  # Milwaukee
root = plotTimeseries(2109508, dist = 5000) # Root River
trail = plotTimeseries(2109547, dist = 5000) # Trail Creek
pike = plotTimeseries(2109515, dist = 5000) # Pike River
oak = plotTimeseries(2109500, dist = 5000) # Oak Creek
calumet = plotTimeseries(2109554, dist = 5000) # Calumet River

# manitowoc = plotTimeseries(2109449, dist = 10000) # Manitowoc River
# susan = plotTimeseries(2109222, dist = 5000) # Susan Creek
# escanaba = plotTimeseries(2109188, dist = 5000) # Escanaba River
# peer = plotTimeseries(2109454, dist = 15000) # Peer Marquette River


fox/grand/stj/men/musk/kal/man/manq 
ggsave('Figures/Figure5_tribsTimeSeries_1.png',width = 6.5, height = 8, dpi = 500)

milw/root/trail/pike/oak/calumet
ggsave('Figures/Figure5_tribsTimeSeries_2.png',width = 6, height = 6, dpi = 500)



