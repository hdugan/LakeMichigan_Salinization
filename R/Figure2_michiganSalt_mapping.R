library(tidyverse)
library(sf)
library(ggspatial)
library(patchwork)

### Figure 2
# a) Chloride concentrations from stream sites in the Lake Michigan watershed collected across multiple agencies. 
# Data from the US Water Quality Portal. b) Chloride concentrations in 235 Lake Michigan tributaries collected between July 10-15, 2018

############## ############## MAPPING ############## ##############
## Esri basemap URLs ####
world_gray <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# Tributary results
tributary_sf = st_read('GIS/Tributary_catchmentPoints.geojson') %>% 
  st_transform(crs = 4269) 

table(tributary_sf$Result_discrete)

# WQP results
wqp_sf = st_read('GIS/WQP_catchmentPoints.geojson') %>% 
  st_transform(crs = 4269) %>% 
  mutate(Result_discrete = fct_relevel(Result_discrete, c("0-10", "10-50", "50-100", "100-1,000", "1,000-10,000","10,000-50,000")))

# Catchments
catchments_sf = st_read('GIS/MI_catchments.geojson')
catchments_sf_simple <- st_simplify(catchments_sf, preserveTopology = TRUE, dTolerance = 1000) %>% 
  st_transform(crs = 4269) # Simplify catchments

# Lake Michigan Shapefiles
lakeMI_sf = st_read('GIS/LakeMichiganShape.geojson', stringsAsFactors = F) 

#### Lat/Long Map of WQP chloride ####
map1 = ggplot(wqp_sf) +
  # annotation_map_tile(type = world_gray, zoom = 9) + # Esri Basemap
  geom_sf(data = catchments_sf_simple, fill = alpha('seagreen1',0.1), size = 0.2) +
  # geom_sf(aes(color = Result_discrete), alpha = 1,
  #         size = 2, stroke = 0) +
  geom_sf(data = lakeMI_sf, size = 0.2, fill = alpha('lightsteelblue1',0.8)) +
  geom_sf(aes(fill = Result_discrete), alpha = 1,
          size = 1.5, stroke = 0.1, shape = 21) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1, na.value = "grey90",
                    breaks = levels(wqp_sf$Result_discrete),
                    limits = levels(wqp_sf$Result_discrete),
                    name = bquote(Chloride ~ (mg~L^-1))) +
  theme_bw() +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.3,'in'), width = unit(0.3,'in'),
                         style = north_arrow_nautical) + # North Arrow
  xlab('') + ylab('') +
  theme(legend.text=element_text(size=6), legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"),
        text = element_text(size=10))

# ggsave(map1, filename = 'Figures/Map_WQPdata_4269.png',width = 6, height = 8)

#### Lat/Long Map of Tributary chloride####
map2 = ggplot(wqp_sf) +
  # annotation_map_tile(type = world_gray, zoom = 9) + # Esri Basemap
  geom_sf(data = lakeMI_sf, size = 0.2, fill = alpha('lightsteelblue1',0.8)) +
  geom_sf(data = catchments_sf_simple, fill = alpha('seagreen1',0.1), size = 0.2) +
  geom_sf(data = tributary_sf, aes(fill = Result_discrete), alpha = 1,
          size = 1.5, stroke = 0.1, shape = 21) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1, na.value = "grey90",
                     breaks = levels(wqp_sf$Result_discrete),
                     limits = levels(wqp_sf$Result_discrete),
                     name = bquote(Chloride ~ (mg~L^-1))) +
  theme_bw() +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.3,'in'), width = unit(0.3,'in'),
                         style = north_arrow_nautical) + # North Arrow
  xlab('') + ylab('') +
  theme(legend.text=element_text(size=6), legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"),
        text = element_text(size=10))

# Plot maps togethers
map2 + map1 + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

ggsave(filename = 'Figures/Figure2_Map_LakeMichigan_Cl.png',width = 6.5, height = 4.5, dpi = 600)
# copy the file to Overleaf Dropbox 
file.copy('Figures/Figure2_Map_LakeMichigan_Cl.png', '/Users/hilarydugan/Dropbox/Apps/Overleaf/Lake Michigan Chloride/Figures/', overwrite = TRUE)
