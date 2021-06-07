library(sf)
library(tidyverse)
library(ggspatial)
library(rgdal)

# Tributary chloride data 
trib_SiteInfo = read_csv('Data/LM_Tributary_SiteInfo.csv') %>% 
  dplyr::select(hydroID_GLAHF, bottle_vial_ID, Population_Density_n_km2,Road_Density_kmroad_km2,MaxImperviousness,MeanImperviousness)

trib_Cl = read_csv('Data/LM_Tributary_Chloride.csv') %>% 
  left_join(trib_SiteInfo) %>% 
  dplyr::mutate(concArea = chloride * Areakm2/112453) %>% 
  mutate(chlorideLoad_kgday = chloride * Flowlitersday/ 1000000) %>% 
  mutate(chlorideYield_kgdaykm2 = chlorideLoad_kgday/Areakm2)

# WQP results
wqp_sf = st_read('GIS/WQP_catchmentPoints.geojson') 
# Tributary results
tributary_sf = st_read('GIS/Tributary_catchmentPoints.geojson')
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
  filter(n > 50) %>% #Keep catchments with lots of data 
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
  filter(Name %in% c('Pensaukee River','East Twin River','Portage-Burns Waterway', 'Oconto River','	White River','Ford River','Kalamazoo River'))
catchmentLabels_b = catchmentNames %>% 
  filter(!Name %in% c('Pensaukee River','East Twin River','Portage-Burns Waterway', 'Oconto River','White River','Ford River','Kalamazoo River'))

ggplot(wqp.catchments.n) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
  geom_vline(aes(xintercept = logArea), linetype = 1, size = 0.1, col = 'grey80') +
  # All Tributary points 
  geom_point(data = tributary_sf,
             aes(x = log(Areakm2), chloride), size = 0.8, color = 'gold', alpha = 0.5, stroke = 0) +
  # WQP boxplots 
  geom_boxplot(aes(x = logArea, y = Result, group = logArea), width = 0.1,
               size = 0.2, outlier.size = 0.5, outlier.shape = 21, outlier.stroke = 0.2, na.rm = TRUE,
               position = position_identity()) +
  # Points matching WQP
  geom_point(data = tributary_sf %>% filter(hydroID_GLAHF %in% wqp.catchments.n$hydroID_GLAHF),
    aes(x = log(Areakm2), chloride), size = 0.8, color = 'red4', fill = 'gold', shape = 21) +
  
  scale_x_continuous(expand = c(0,0.1), sec.axis = dup_axis(breaks = catchmentNames$logArea, labels = catchmentNames$Name),
                     breaks = log(c(10,100,1000,5000,10000)), labels = c(10,100,1000,5000,10000),
                                         name = bquote(Watershed~Area ~ (km^2))) +
  geom_text(data = catchmentLabels_b, aes(x = logArea, y = -150, group = logArea, label = Name),
            angle = 90, size = 1.8) +
  geom_text(data = catchmentLabels_a, aes(x = logArea, y = 900, group = logArea, label = Name),
            angle = 90, size = 1.8) +
  ylab(bquote(Chloride ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  # theme(axis.text.x.bottom = element_text(angle = 70, vjust = 1, hjust=1)) +
  theme(axis.text.x.top = element_blank(),
        axis.title.x.top = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylim(-250,1000) +
  NULL

ggsave('Figures/Figure3_Cl_comparison.png',width = 6.5, height = 5, dpi = 500)

