# Manuscript statistics 

# Load necessary data 
source('R/Figure3_clComparison.R')

# Lake Michigan area - 58.03 billion m²
miArea = 58.03e9 #m2
# Lake Michigan volume 4,920 km3
miVol = 4920 * 1000 * 1000 * 1000 #m3

# How much would it take to raise the concentration of Lake Michigan by 0.125 mg/L (g/m3)? 
0.125 * miVol / 1e12 #(Tg or million metric tonnes)

# Take mean of duplicates
tribMean = trib_Cl %>% group_by(hydroID_GLAHF, streamName) %>% 
  summarise_all(list(mean)) %>% 
  # mutate(chloride = if_else(streamName == 'Milwaukee River',120, chloride)) %>% # for a test
  mutate(concArea = chloride*(Areakm2/sum(tribMean$Areakm2))) %>% #just double checking
  mutate(chlorideLoad_annual_kgday = chloride*(Flow_median_m3s*1000*24*3600)/ 1e6) #just double checking

# Mean scaled by area
sum(tribMean$concArea) # 25.4 mg/L
# Mean not-scaled by area
mean(tribMean$chloride) # 31.23 mg/L

# Flow on that day vs annual 
sum(tribMean$Flowlitersday)/24/3600/1000
sum(tribMean$Flow_median_m3s)

# Total load
sum(tribMean$concArea) * sum(tribMean$Flow_median_m3s) * 3600*24*365 # grams
sum(tribMean$chlorideLoad_annual_kgday) * 365 / 1e9 # Tg

# Years of observations
ys = wqp_sf %>% 
  st_set_geometry(NULL) %>% 
  group_by(year = year(ActivityStartDate)) %>% 
  tally()
# Percent of observations after 1990
sum(ys %>% filter(year >= 1990) %>% pull(n)) / sum(ys$n)

# Highest results from WQP
# wqp_sf %>% 
#   st_set_geometry(NULL) %>% filter(Result > 100) %>% arrange(desc(Result)) 

# Mean and median all year
wqp_sf %>% st_set_geometry(NULL) %>% summarise(mean = mean(Result), median = median(Result))
# Mean and median in JFM
wqp_sf %>% st_set_geometry(NULL) %>% 
  filter(month(ActivityStartDate) <= 3) %>% 
  summarise(mean = mean(Result), median = median(Result))


# Watershed comparison between WQP data and Tributary data 
# get distance from outlet 
WQP_tribs.median %>% dplyr::select(Result, chloride)
summary(lm(chloride ~ Result, data = WQP_tribs.median))

# area weighted concentration between the two
WQP_tribs.median %>% ungroup() %>% mutate(Result.area = Result * Areakm2 / sum(WQP_tribs.median$Areakm2),
                            chloride.area = chloride * Areakm2 / sum(WQP_tribs.median$Areakm2)) %>% 
  summarise(Result.area = sum(Result.area), chloride.area = sum(chloride.area))

## Total loads 
tribMean %>% ungroup() %>% dplyr::select(streamName, chloride, chlorideLoad_kgday, chlorideLoad_annual_kgday) %>% 
  arrange(desc(chlorideLoad_annual_kgday)) %>% 
  summarise(sum(chlorideLoad_annual_kgday))
# 2957085 kg/day
# 2957085 = 2957 metric tonnes/day
2957*365 # metric tonnes per year 

# Load from rain 
0.5 * 0.917 * miArea / 1e12 #(metric tonnes from precipitation assuming 1 m/L rain conc. and 917 mm of rain)

# Songnozi loads 1970s
598000 + 220400 + 82900 #(tributary + direct + atmospheric) metric tonnes per year

top5load = tribMean %>% ungroup() %>% dplyr::select(streamName, chloride, chlorideLoad_kgday, chlorideLoad_annual_kgday) %>% 
  arrange(desc(chlorideLoad_annual_kgday)) %>% slice(1:5) %>% 
  summarise(sum(chlorideLoad_annual_kgday))

MilwaukeeLoad = tribMean %>% ungroup() %>% 
  filter(streamName == 'Milwaukee River') %>%
  summarise(sum(chlorideLoad_annual_kgday))

#  Top 5 tributaries account for 65% of that load 
top5load/sum(tribMean$chlorideLoad_annual_kgday)
# Milwaukee Load
MilwaukeeLoad/sum(tribMean$chlorideLoad_annual_kgday)

tribMean %>% ungroup() %>% dplyr::select(streamName, chloride, chlorideLoad_annual_kgday, chlorideYield_kgdaykm2) %>% 
  arrange(desc(chlorideLoad_annual_kgday)) %>% 
  mutate(chlorideLoad_Tgyear = chlorideLoad_annual_kgday*365/1e3) %>% 
  mutate(percentLoad = chlorideLoad_annual_kgday/sum(tribMean$chlorideLoad_annual_kgday))

# Values from Hunter et al 2015
# https://www-sciencedirect-com.ezproxy.library.wisc.edu/science/article/pii/S0380133014002676?via%3Dihub#s0045
# Precipitation is 917 mm /year
pm3 = (917/1000) * miArea # m3
pm3s = pm3 / (365*24*60*60) # m3/s?

# Evaporation is 600 mm /year
em3 = (600/1000) * miArea # m3
em3 / (365*24*60*60) # m3/s?

