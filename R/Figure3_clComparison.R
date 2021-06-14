library(sf)
library(tidyverse)
library(ggspatial)
library(rgdal)
library(lubridate)

# Read in Tributary chloride data 
trib_Cl = read_csv('Data/LM_Tributary_Chloride.csv') %>% 
  # left_join(trib_SiteInfo) %>% 
  dplyr::mutate(concArea = chloride * Areakm2/112453) %>% 
  mutate(chlorideLoad_kgday = chloride * Flowlitersday/ 1000000) %>% 
  mutate(chlorideYield_kgdaykm2 = chlorideLoad_kgday/Areakm2) %>% 
  filter(streamName != 'Susan Creek')

# WQP results
wqp_sf = st_read('GIS/WQP_catchmentPoints.geojson') 
# Tributary results
tributary_sf = st_read('GIS/Tributary_catchmentPoints.geojson') %>% 
  filter(streamName != 'Susan Creek')
# Catchments
catchments_sf = st_read('GIS/MI_catchments.geojson', stringsAsFactors = FALSE)

# Intersect WQP data with Lake Michigan catchments
wqp.catchments <- wqp_sf %>% 
  mutate(intersection = as.integer(st_intersects(geometry, catchments_sf))) %>% 
  mutate(hydroID_GLAHF = if_else(!is.na(intersection), catchments_sf$HydroID[intersection], NA_real_)) %>% 
  mutate(Areakm2 = if_else(!is.na(intersection), catchments_sf$Shape_Area[intersection]/1000000, NA_real_)) %>% 
  mutate(Name = if_else(!is.na(intersection), catchments_sf$GNIS_NAME[intersection], NA_character_)) %>% 
  mutate(logArea = log(Areakm2)) %>% 
  dplyr::select(hydroID_GLAHF, Areakm2, logArea, Name, MonitoringLocationName, ActivityStartDate, Result, Result_discrete)

# Keep catchments with more than 50 data points
wqp.catchments.n = wqp.catchments %>% group_by(hydroID_GLAHF) %>% mutate(n = n()) %>%
  filter(hydroID_GLAHF >= 0) %>% # Filter out interfluves 
  filter(n > 20) %>% #Keep catchments with lots of data 
  ungroup()

# Improve list of catchment names
catchmentNames = as.data.frame(wqp.catchments.n) %>% group_by(hydroID_GLAHF) %>% 
  dplyr::select(-geometry) %>% 
  summarise_all(list(first)) %>% 
  left_join(trib_Cl %>% dplyr::select(hydroID_GLAHF, streamName) %>% group_by(hydroID_GLAHF) %>% summarise_all(list(first))) %>% # Double check with Tributary names
  mutate(Name = case_when(hydroID_GLAHF == 2109542 ~ 'Galien River',
                          hydroID_GLAHF == 2109554 ~ 'Calumet River',
                          hydroID_GLAHF == 2109551 ~ 'Dunes Creek',
                          hydroID_GLAHF == 	2109205 ~ 'Ford River',
                          hydroID_GLAHF == 	2109316 ~ 'Peshtigo River',
                          hydroID_GLAHF == 	2109496 ~ 'Milwaukee River',
                          TRUE ~ Name))

# Labels for plotting to avoid overlap 
catchmentLabels_a = catchmentNames %>% 
  filter(Name %in% c('Pensaukee River','East Twin River','Portage-Burns Waterway', 'Oconto River','	White River','Ford River','Kalamazoo River', 'Bear River'))
catchmentLabels_b = catchmentNames %>% 
  filter(!Name %in% c('Pensaukee River','East Twin River','Portage-Burns Waterway', 'Oconto River','White River','Ford River','Kalamazoo River', 'Bear River'))

p1 = ggplot(wqp.catchments.n) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_vline(aes(xintercept = logArea), linetype = 1, size = 0.1, col = 'grey80') +
  # All Tributary points 
  geom_point(data = tributary_sf,
             aes(x = log(Areakm2), chloride), size = 0.8, color = 'gold4', alpha = 0.5, stroke = 0) +
  # WQP boxplots 
  # geom_boxplot(aes(x = logArea, y = Result, group = logArea), width = 0.1,
  #              size = 0.2, outlier.size = 0.5, outlier.shape = 21, outlier.stroke = 0.2, na.rm = TRUE,
  #              position = position_identity()) +
  stat_summary(geom = "boxplot", 
               aes(x = logArea, y = Result, group = logArea),
               width = 0.1,
               size = 0.2,
               position = position_identity(),
               fun.data = function(x) setNames(quantile(x, c(0.01, 0.25, 0.5, 0.75, 0.99)), 
                                               c("ymin", "lower", "middle", "upper", "ymax"))) + 
  stat_summary(geom = "boxplot", 
               aes(x = logArea, y = Result, group = logArea),
               width = 0.1, color = 'red4',
               size = 0.3, outlier.size = 0.5, outlier.shape = 21, outlier.stroke = 0.2, na.rm = TRUE,
               position = position_identity(),
               fun.data = function(x) setNames(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)), 
                                               c("ymin", "lower", "middle", "upper", "ymax"))) +

  # Points matching WQP
  geom_point(data = tributary_sf %>% filter(hydroID_GLAHF %in% outletDist.df$hydroID_GLAHF),
    aes(x = log(Areakm2), chloride), size = 0.8, color = 'red4', fill = 'gold', shape = 21) +
  
  scale_x_continuous(expand = c(0,0.1), sec.axis = dup_axis(breaks = catchmentNames$logArea, labels = catchmentNames$Name),
                     breaks = log(c(10,100,1000,5000,10000)), labels = c(10,100,1000,5000,10000),
                                         name = bquote(Watershed~Area ~ (km^2))) +
  geom_text(data = catchmentLabels_b, aes(x = logArea, y = -150, group = logArea, label = Name),
            angle = 90, size = 1.8) +
  geom_text(data = catchmentLabels_a, aes(x = logArea, y = 700, group = logArea, label = Name),
            angle = 90, size = 1.8) +
  ylab(bquote(Chloride ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  # theme(axis.text.x.bottom = element_text(angle = 70, vjust = 1, hjust=1)) +
  theme(axis.text.x.top = element_blank(),
        axis.title.x.top = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylim(-250,850) +
  NULL
p1
ggsave(plot = p1, 'Figures/Figure3_Cl_comparison.png',width = 6.5, height = 5, dpi = 500)


# Watershed comparison between WQP data and Tributary data 
# get distance from outlet 
outletDist = list()
hydroids = unique(wqp.catchments$hydroID_GLAHF)
for (i in 1:length(hydroids)) {
  river = wqp.catchments %>% filter(hydroID_GLAHF == hydroids[i])
  # Points close to outlet
  outletPoint = tributary_sf %>% filter(hydroID_GLAHF == hydroids[i]) %>% slice(1)
  if(nrow(outletPoint) == 0) {next}
  outletDist[[i]] = river %>% mutate(distanceOutlet_m = river %>% st_distance(outletPoint, by_element = TRUE)) 
}

outletDist.df <- dplyr::bind_rows(outletDist)
WQP_tribs = outletDist.df %>% filter(distanceOutlet_m < units::set_units(1000, "m")) %>% 
  left_join(tribMean)

WQP_tribs.median = WQP_tribs %>% group_by(hydroID_GLAHF, Name, streamName) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE) %>% 
  filter(Name != 'Susan Creek')

p2 = ggplot(WQP_tribs.median) +
  geom_abline(alpha = 0.5) +
  geom_point(aes(x = Result, y = chloride, fill = urban),  alpha = 1,
             size = 2, stroke = 0.1, shape = 21) +
  scale_fill_distiller(palette = 'RdYlBu', name = '% Urban') +
  xlab(bquote(Median~Watershed~Chloride ~ (mg~L^-1))) +
  ylab(bquote(Tributary~Chloride ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  theme(legend.position = c(0.9,0.4),
        legend.key.height = unit(0.3,'cm'),
        legend.key.width = unit(0.2,'cm')); p2

p1/p2 + plot_layout(heights = c(3,1)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  theme(plot.tag = element_text(size = 8))
ggsave('Figures/Figure3_Cl_comparison_2.png',width = 6.5, height = 6, dpi = 500)


