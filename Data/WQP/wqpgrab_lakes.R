library(dataRetrieval)
library(tidyverse)
options(scipen = 999)

stateCd
# Michigan = 26
# Wisconsin = 55
# Minnesota = 27
# New York = 36
# Ohio = 39
# Indiana = 18
# Illinois = 17

# Sites from EPA
sites.EPA <- whatWQPsites(characteristicName = "Chloride", 
                          organization = 'EPA_GLNPO') %>% 
  filter(MonitoringLocationTypeName == 'Great Lake')

data.EPA = list()
for (i in 1:nrow(sites.EPA)) {
  tryCatch({
    data.EPA[[i]] <- readWQPdata(siteid = sites.EPA$MonitoringLocationIdentifier[i],
                                characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}


# Sites from Lake Michigan: Michigan
sites.MI <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Lake%2C%20Reservoir%2C%20Impoundment', 
                         statecode = "US:26") %>% 
  filter(str_detect(MonitoringLocationName, "Lake Michigan"))
length(table(sites.MI$HUCEightDigitCode))

# Sites from Wisconsin streams
sites.WI <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Lake%2C%20Reservoir%2C%20Impoundment', 
                         statecode = "US:55") %>% 
  filter(str_detect(MonitoringLocationName, "Lake Michigan"))

# Sites from Indiana streams
sites.IN <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Lake%2C%20Reservoir%2C%20Impoundment', 
                         statecode = "US:18") %>% 
  filter(str_detect(MonitoringLocationName, "Lake Michigan"))

# Sites from Illinois streams
sites.IL <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Lake%2C%20Reservoir%2C%20Impoundment', 
                         statecode = "US:17") %>% 
  filter(str_detect(MonitoringLocationName, "Lake Michigan"))

data.MI = list()
for (i in 1:nrow(sites.MI)) {
  tryCatch({
  data.MI[[i]] <- readWQPdata(siteid = sites.MI$MonitoringLocationIdentifier[i],
                              characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}
data.WI = list()
for (i in 1:nrow(sites.WI)) {
  tryCatch({
    data.WI[[i]] <- readWQPdata(siteid = sites.WI$MonitoringLocationIdentifier[i],
                                characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}
data.IN = list()
for (i in 1:nrow(sites.IN)) {
  tryCatch({
    data.IN[[i]] <- readWQPdata(siteid = sites.IN$MonitoringLocationIdentifier[i],
                                characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}
data.IL = list()
for (i in 1:nrow(sites.IL)) {
  tryCatch({
    data.IL[[i]] <- readWQPdata(siteid = sites.IL$MonitoringLocationIdentifier[i],
                                characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}

data.EPA = list()
for (i in 1:nrow(sites.EPA)) {
  tryCatch({
    data.EPA[[i]] <- readWQPdata(siteid = sites.EPA$MonitoringLocationIdentifier[i],
                                 characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}

# Bind to dataframe
bindDF <- function(df, sites) {
  
  sitesDF = sites %>% select(OrganizationIdentifier:LongitudeMeasure,StateCode,CountyCode)
  
  df.bind = as.data.frame(do.call(rbind, df)) %>% 
    filter(!is.na(ResultMeasureValue)) %>% 
    mutate(Result = as.numeric(ResultMeasureValue), ResultMeasureValue = as.character(ResultMeasureValue)) %>% 
    mutate(Result = if_else(ResultMeasure.MeasureUnitCode == 'mg/l', Result, Result / 1000)) %>% 
    select(MonitoringLocationIdentifier, OrganizationIdentifier:ActivityStartDate, 
           Depth = ActivityDepthHeightMeasure.MeasureValue,
           HydrologicCondition:ResultMeasure.MeasureUnitCode,
           Result) %>% 
    left_join(sitesDF, by = 'MonitoringLocationIdentifier') %>%
    I()
  
  print(table(df.bind$ResultMeasure.MeasureUnitCode))
  return(df.bind)
}

data.MI.df = bindDF(data.MI, sites.MI) 
data.WI.df = bindDF(df = data.WI, sites =  sites.WI)
data.IN.df = bindDF(data.IN, sites.IN)
data.IL.df = bindDF(data.IL, sites.IL)

allD = bind_rows(data.MI.df, data.WI.df) %>% 
  bind_rows(data.IN.df) %>% 
  bind_rows(data.IL.df)
table(allD$StateCode, useNA = 'always')
allD %>% filter(Result > 10000)
write_csv(allD, 'WQP/allLakeMichigan.csv')

# Lake Michigan chloride from WQP
ggplot(allD) +
  geom_histogram(aes(x = Result, fill = as.character(StateCode)),bins = 100, position="identity", alpha = 0.4) +
  scale_x_log10()

ggplot(allD) +
  geom_point(aes(x = ActivityStartDate, y = Result)) +
  xlim(as.Date('2000-01-01'),NA) +
  theme_bw(base_size = 8)

# Filter out MMSD outfall data 
allD2 = allD %>% 
  filter(OrganizationFormalName.y != 'Milwaukee Metropolitan Sewerage District') %>% 
  filter(StateCode != 18) #Filter out Indidana
table(allD2$StateCode)
table(allD2$OrganizationIdentifier.x)
# Michigan = 26
# Wisconsin = 55
# Indiana = 18
# Illinois = 17

ggplot(allD2) +
  geom_point(aes(x = ActivityStartDate, y = Result, col = as.character(StateCode))) +
  xlim(as.Date('2000-01-01'),NA) +
  theme_bw(base_size = 8)
View(allD2 %>% arrange(-Result))

write_csv(allD2, 'Data/WQP/allLakeMichigan.csv')

#### Lake Michigan chloride from EPA ####
# Bind EPA sites 
data.EPA.df = bindDF(data.EPA, sites.EPA) %>% 
  filter(ActivityMediaName == 'Water') %>% 
  filter(ActivityTypeCode == 'Sample-Routine')
unique(data.EPA.df$MonitoringLocationDescriptionText)
# Isolate just Lake Michigan from EPA sites
data.EPA.df.MI = data.EPA.df %>% filter(LongitudeMeasure <=-83.95 & LatitudeMeasure <46)
plot(data.EPA.df$LongitudeMeasure, data.EPA.df$LatitudeMeasure)
points(data.EPA.df.MI$LongitudeMeasure, data.EPA.df.MI$LatitudeMeasure, col = 'blue')
ggplot(data.EPA.df.MI) +
  geom_point(aes(ActivityStartDate, Result))

# Isolate just Lake Ontario from EPA sites
data.EPA.df.ON = data.EPA.df %>% filter(LongitudeMeasure >=-78.3 & LatitudeMeasure <46)
plot(data.EPA.df$LongitudeMeasure, data.EPA.df$LatitudeMeasure)
points(data.EPA.df.ON$LongitudeMeasure, data.EPA.df.ON$LatitudeMeasure, col = 'blue')
ggplot(data.EPA.df.ON) +
  geom_point(aes(ActivityStartDate, Result))

# Isolate just Lake Erie from EPA sites
data.EPA.df.Er = data.EPA.df %>% filter(LongitudeMeasure >=-83.95 & LatitudeMeasure <43)
plot(data.EPA.df$LongitudeMeasure, data.EPA.df$LatitudeMeasure)
points(data.EPA.df.Er$LongitudeMeasure, data.EPA.df.Er$LatitudeMeasure, col = 'blue')
ggplot(data.EPA.df.Er) +
  geom_point(aes(ActivityStartDate, Result))

# Save EPA data
write_csv(data.EPA.df, 'Data/WQP/allGreatLakes_EPA.csv')
write_csv(data.EPA.df.MI, 'Data/WQP/allLakeMichigan_EPA.csv')
