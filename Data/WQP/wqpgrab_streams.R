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
hucs = read_csv('Data/LakeMichigan_Subwatersheds_HUC.csv')
sort(hucs$HUC)

# Sites from Michigan streams
sites.MI <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Stream', 
                         statecode = "US:26")
length(table(sites.MI$HUCEightDigitCode))

# Sites from Wisconsin streams
sites.WI <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Stream', 
                         statecode = "US:55")

# Sites from Indiana streams
sites.IN <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Stream', 
                         statecode = "US:18")

# Sites from Illinois streams
sites.IL <- whatWQPsites(characteristicName = "Chloride", 
                         siteType = 'Stream', 
                         statecode = "US:17")

# Lake Michigan HUCs
hucs = c(as.numeric(paste0(040500,'00')):as.numeric(paste0(040500,'99')),
as.numeric(paste0(040301,'00')):as.numeric(paste0(040301,'99')),
as.numeric(paste0(040302,'00')):as.numeric(paste0(040302,'99')),
as.numeric(paste0(040400,'00')):as.numeric(paste0(040400,'99')),
as.numeric(paste0(040500,'00')):as.numeric(paste0(040500,'99')),
as.numeric(paste0(040601,'00')):as.numeric(paste0(040601,'99')))

# Isolate to lake michigan HUCs
sites.MI.tribs = sites.MI %>% filter(as.numeric(HUCEightDigitCode) %in% hucs)
sites.WI.tribs = sites.WI %>% filter(as.numeric(HUCEightDigitCode) %in% hucs)
sites.IN.tribs = sites.IN %>% filter(as.numeric(HUCEightDigitCode) %in% hucs)
sites.IL.tribs = sites.IL %>% filter(as.numeric(HUCEightDigitCode) %in% hucs)


data.MI = list()
for (i in 1:nrow(sites.MI.tribs)) {
  tryCatch({
  data.MI[[i]] <- readWQPdata(siteid = sites.MI.tribs$MonitoringLocationIdentifier[i],
                              characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}
data.WI = list()
for (i in 1:nrow(sites.WI.tribs)) {
  tryCatch({
    data.WI[[i]] <- readWQPdata(siteid = sites.WI.tribs$MonitoringLocationIdentifier[i],
                                characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}
data.IN = list()
for (i in 1:nrow(sites.IN.tribs)) {
  tryCatch({
    data.IN[[i]] <- readWQPdata(siteid = sites.IN.tribs$MonitoringLocationIdentifier[i],
                                characteristicName="Chloride")
  }, error=function(e){})
  print(i)
}
data.IL = list()
for (i in 1:nrow(sites.IL.tribs)) {
  tryCatch({
    data.IL[[i]] <- readWQPdata(siteid = sites.IL.tribs$MonitoringLocationIdentifier[i],
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

data.MI.df = bindDF(data.MI, sites.MI.tribs) 
data.WI.df = bindDF(data.WI, sites.WI.tribs)
data.IN.df = bindDF(data.IN, sites.IN.tribs)
data.IL.df = bindDF(data.IL, sites.IL.tribs)

allD = bind_rows(data.MI.df, data.WI.df) %>% 
  bind_rows(data.IN.df) %>% 
  bind_rows(data.IL.df)

table(allD$StateCode, useNA = 'always')

allD %>% filter(Result > 10000)

# Tributaries in Lake Michigan watershed
ggplot(allD) +
  geom_histogram(aes(x = Result, fill = as.character(StateCode)),bins = 100, position="identity", alpha = 0.4) +
  scale_x_log10()

write_csv(sites.MI.tribs, 'Data/WQP/sitesMI.csv')
write_csv(sites.WI.tribs, 'Data/WQP/sitesWI.csv')
write_csv(sites.IN.tribs, 'Data/WQP/sitesIN.csv')
write_csv(sites.IL.tribs, 'Data/WQP/sitesIL.csv')
write_csv(data.MI.df, 'Data/WQP/dataMI.csv')
write_csv(data.WI.df, 'Data/WQP/dataWI.csv')
write_csv(data.IN.df, 'Data/WQP/dataIL.csv')
write_csv(data.IN.df, 'Data/WQP/dataIL.csv')
write_csv(allD, 'Data/WQP/allSites.csv')
