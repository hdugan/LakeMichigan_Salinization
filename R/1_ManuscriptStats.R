# Manuscript statistics 

# Load necessary data 
source('R/Figure3_clComparison.R')

# Take mean of duplicates
tribMean = trib_Cl %>% group_by(hydroID_GLAHF, streamName) %>% 
  summarise_all(list(mean))

# Mean scaled by area
sum(tribMean$concArea) # 25.5 mg/L
# Mean not-scaled by area
mean(tribMean$chloride) # 33.78 mg/L

# Years of observations
ys = wqp_sf %>% 
  st_set_geometry(NULL) %>% 
  group_by(year = year(ActivityStartDate)) %>% 
  tally()
# Percent of observations after 1990
sum(ys %>% filter(year >= 1990) %>% pull(n)) / sum(ys$n)

# Highest results from WQP
wqp_sf %>% 
  st_set_geometry(NULL) %>% filter(Result > 100) %>% arrange(desc(Result)) 

# Mean and median all year
wqp_sf %>% st_set_geometry(NULL) %>% summarise(mean = mean(Result), median = median(Result))
# Mean and median in JFM
wqp_sf %>% st_set_geometry(NULL) %>% 
  filter(month(ActivityStartDate) <= 3) %>% 
  summarise(mean = mean(Result), median = median(Result))


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

# hydroids[which(!hydroids %in% unique(outletDist.df$hydroID_GLAHF))]
outletDist.df <- dplyr::bind_rows(outletDist)
WQP_tribs = outletDist.df %>% filter(distanceOutlet_m < units::set_units(1000, "m")) %>% 
  left_join(tribMean)
WQP_tribs %>% group_by(hydroID_GLAHF, Name, streamName) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  ggplot() +
  geom_point(aes(x = Result, y = chloride))
