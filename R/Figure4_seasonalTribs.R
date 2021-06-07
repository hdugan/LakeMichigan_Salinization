library(lubridate)
library(patchwork)

# Tributary results
tributary_sf = st_read('GIS/Tributary_catchmentPoints.geojson')

plotSeason <- function(hydroid, dist = 10000) {
# Filter for river
  river = wqp.catchments %>% filter(hydroID_GLAHF == hydroid) %>% 
    mutate(month = month(ActivityStartDate))
  name = river$Name[1]
  # 
  # Points close to outlet
  outletPoint = tributary_sf %>% filter(hydroID_GLAHF == hydroid)
  
  Outlet = river %>% mutate(distanceOutlet = river %>% st_distance(outletPoint, by_element = TRUE)) %>% 
    filter(distanceOutlet < units::set_units(dist, "m")) 

  # Get river name
  riverName = catchmentNames %>% filter(hydroID_GLAHF == hydroid) %>% pull(Name)
  
  p1 = ggplot(Outlet) + 
    geom_histogram(aes(x = month), fill = '#c1d6e3', color = 'black', size = 0.1) +
    scale_x_continuous(breaks = 1:12, labels = substr(month.abb,1,1), limits = c(0.5,12.5)) +
    labs(title = riverName) +
    ylab('# Obs') +
    theme_bw(base_size = 8) +
    theme(plot.title = element_text(size = 8), 
          axis.title.x = element_blank())
  
  p2 = ggplot(Outlet) +
    geom_boxplot(aes(x = month, y = Result, group = month), outlier.size = 0.5, size = 0.2, fill = '#c1d6e3') +
    # ylab('Chloride (mg/L)') +
    ylab(bquote(Chloride ~ (mg~L^-1))) +
    scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(0.5,12.5)) +
    labs(title = paste0(riverName,', < ',dist/1000,' km from outlet')) +
    theme_bw(base_size = 8) +
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_blank()); p2
  return(p2 + p1 +
           plot_layout(widths = c(2,1))) 
}
plotSeason(2109547, dist = 5000)

fox = plotSeason(2109413, dist = 5000) # Fox River
grand = plotSeason(2109493, dist = 20000) # Grand Creek
stj = plotSeason(2109534) # St. Joseph's River
men = plotSeason(2109298, dist = 5000) # Menominee River
musk = plotSeason(2109488, dist = 30000) # Muskegon River
kal = plotSeason(2109506, dist = 10000) # Kalamazoo River
man = plotSeason(2109428, dist = 10000) # Manistee River
manq = plotSeason(2109142, dist = 10000) # Manistique River

milw = plotSeason(2109496, dist = 5000)  # Milwaukee
root = plotSeason(2109508, dist = 5000) # Root River
trail = plotSeason(2109547, dist = 5000) # Trail Creek
pike = plotSeason(2109515, dist = 5000) # Pike River
oak = plotSeason(2109500, dist = 5000) # Oak Creek
calumet = plotSeason(2109554, dist = 5000) # Calumet River

manitowoc = plotSeason(2109449, dist = 10000) # Manitowoc River
susan = plotSeason(2109222, dist = 5000) # Susan Creek
escanaba = plotSeason(2109188, dist = 5000) # Escanaba River
peer = plotSeason(2109454, dist = 15000) # Peer Marquette River


fox/grand/stj/men/musk/kal/man/manq 
ggsave('Figures/Figure4_seasonalTribs_1.png',width = 6.5, height = 8, dpi = 500)

milw/root/trail/pike/oak/calumet
ggsave('Figures/Figure4_seasonalTribs_2.png',width = 6, height = 6, dpi = 500)



