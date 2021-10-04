# To assess if our chloride grab samples are representative of a 5-day mean, we
# analysed high-frequency specific conductance (SpC) records from two Lake
# Michigan tributaries from 2020 to 2021: (1) the Fox River at Green Bay, WI
# (USGS 040851385), which drains 16394.62 km2, and (2) the Manitowoc River at
# Manitowoc, WI (USGS 04085427), which drains 1362.33 km2. At both sites, SpC is
# measured at a sub-hourly frequency. First, we randomly sampled the datasets to
# downscale to an hourly timestep and subset the data to dates spanning June
# through September, as we were comparing to our summer samples. We did not
# exclude any storm flow. We then computed the 5-day mean and standard
# deviation. For each hourly observation, we computed whether the observation
# was within one standard deviation of the mean.

library(tidyverse)
library(lubridate)
library(zoo)

# Retrieve USGS gage data 
# 040851385	 FOX RIVER AT OIL TANK DEPOT AT GREEN BAY, WI 
fox = read_tsv('Data/USGS_SpC/Fox_USGS_040851385_noheader.txt') %>% 
  rename(Q = "262076_00060", dateTime = datetime, SpC = "157404_00095") %>% 
  select(dateTime, Q, SpC) %>% 
  mutate(floordate = floor_date(dateTime, unit = "hour")) # Convert minute column to datetime. Find the hour of the minute column using floor_date

fox2 = fox %>%
  select(floordate, SpC) %>% 
  filter(!is.na(SpC)) %>% 
  group_by(floordate) %>% 
  sample_n(1)

fox3 = fox %>%
  select(floordate, Q) %>% 
  filter(!is.na(Q)) %>% 
  group_by(floordate) %>% 
  sample_n(1) %>% 
  left_join(fox2) %>% # Join back with random hourly grab sample
  filter(month(floordate) %in% c(6:9)) %>%
  ungroup() %>% 
  mutate(ma5.mean = rollapply(SpC, 24*5, mean, align='center', fill=NA)) %>% 
  mutate(ma5.sd = rollapply(SpC, 24*5, sd, align='center', fill=NA)) %>% 
  filter(!is.na(ma5.mean)) %>% 
  mutate(sd.rep1 = if_else(abs(SpC - ma5.mean) < ma5.sd, TRUE, FALSE)) %>% 
  mutate(sd.rep0.5 = if_else(abs(SpC - ma5.mean) < ma5.sd/2, TRUE, FALSE))

# Is an hourly grab sample representative of the 5 day mean? 
table(fox3$sd.rep1)/nrow(fox3)
table(fox3$sd.rep0.5)/nrow(fox3)

# Plot data
ggplot(fox3) +
  geom_path(aes(x = floordate, y = SpC)) +
  geom_path(aes(x = floordate, y = Q/100), col = 'blue')

# Retrieve USGS gage data 
# 04085427 MANITOWOC RIVER AT MANITOWOC, WI 
manitowoc = read_tsv('Data/USGS_SpC/Manitowoc_USGS_040851385_noheader.txt') %>% 
  rename(Q = "157416_00060", dateTime = datetime, SpC = "157417_00095") %>% 
  select(dateTime, Q, SpC) %>% 
  mutate(floordate = floor_date(dateTime, unit = "hour")) # Convert minute column to datetime. Find the hour of the minute column using floor_date
  
manitowoc2 = manitowoc %>%
  select(floordate, SpC) %>% 
  filter(!is.na(SpC)) %>% 
  group_by(floordate) %>% 
  sample_n(1)

manitowoc3 = manitowoc %>%
  select(floordate, Q) %>% 
  filter(!is.na(Q)) %>% 
  group_by(floordate) %>% 
  sample_n(1) %>% 
  left_join(manitowoc2) %>% # Join back with random hourly grab sample
  filter(month(floordate) %in% c(6:9)) %>%
  ungroup() %>% 
  mutate(ma5.mean = rollapply(SpC, 24*5, mean, align='center', fill=NA)) %>% 
  mutate(ma5.sd = rollapply(SpC, 24*5, sd, align='center', fill=NA)) %>% 
  filter(!is.na(ma5.mean)) %>% 
  mutate(sd.rep1 = if_else(abs(SpC - ma5.mean) < ma5.sd, TRUE, FALSE)) %>% 
  mutate(sd.rep0.5 = if_else(abs(SpC - ma5.mean) < ma5.sd/2, TRUE, FALSE))

# Is an hourly grab sample representative of the 5 day mean? 
table(manitowoc3$sd.rep1)/nrow(manitowoc3)
table(manitowoc3$sd.rep0.5)/nrow(manitowoc3)

# Plot data
ggplot(manitowoc3) +
  geom_path(aes(x = floordate, y = SpC)) +
  geom_path(aes(x = floordate, y = Q), col = 'blue')

