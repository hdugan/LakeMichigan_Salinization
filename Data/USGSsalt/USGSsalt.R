library(sf)
library(rgdal)
library(raster)

# All data obtained from Estimates of Road Salt Application across the Conterminous United States, 1992-2015
# https://www.sciencebase.gov/catalog/item/5b15a50ce4b092d9651e22b9
# Must download 1992_2015 geotiff files to get this code to work (~150 Mb)

years = 1992:2015

saltuse.df = data.frame(years = years, saltuse_metton = NA)
files = list.files('Data/USGSsalt/1992_2015/', full.names = T)

# Load catchment shape
catchments = st_read('GIS/catchments.shp') %>% 
  st_transform(crs(raster(files[1])))

for (i in 1:length(years)) {
  USGSsalt <- raster(x = files[i])
  # Salt use values in pounds 
  # now use the mask function to mask salt use to Lake Michigan watershed
  rr <- mask(USGSsalt, catchments)
  
  salt_lbs = sum(getValues(rr), na.rm = T)
  salt_metrictonnes = salt_lbs * 0.000453592
  saltuse.df$saltuse_metton[i] = salt_metrictonnes
}
saltuse.df

# Export csv 
write_csv(saltuse.df, 'Data/USGSsalt/saltuseDF.csv')
