library(scales)
library(tidyverse)
library(patchwork)

### Figure 1 
# a) Estimated road salt use in the Lake Michigan watershed derived from \citep{bock_estimates_2018}. 
# b) Chloride concentrations in Lake Michigan collected by multiple agencies (US Environmental Protection Agency 
# (EPA) Great Lakes National Program Office, Illinois EPA, Wisconsin Department of Natural Resources, Little River 
# Band of Ottawa Indians) across multiple sites. Data from the US Water Quality Portal. Two outliers were removed.}

# Read in WQP data
data.EPA.df.MI = read_csv('Data/WQP/allLakeMichigan_EPA.csv')
allD2 = read_csv('Data/WQP/allLakeMichigan.csv')
# Read in USGS salt use data
saltuse.df = read_csv('Data/USGSsalt/saltuseDF.csv')


p1 = ggplot(data.EPA.df.MI) +   # Remove 3 highest and lowest values +
  geom_point(aes(x = ActivityStartDate, y = Result), shape = 21, fill = 'lightblue4', stroke = 0.1, alpha = 0.7) +
  xlim(as.Date('1982-01-01'),NA) +
  geom_point(data = allD2 %>% arrange(-Result) %>%  # arrange by Results
               slice(3:(n() - 3)), 
             aes(x = ActivityStartDate, y = Result), shape = 21, fill = 'lightblue4', stroke = 0.1, alpha = 0.7) +
  ylab(bquote(Chloride ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())

p2 = ggplot(saltuse.df) +
  geom_col(aes(x = years, y = saltuse_metton), fill = 'lightblue4') +
  theme_bw(base_size = 8) +
  ylab('Road salt (metric tonnes)') +
  scale_y_continuous(label = comma) +
  theme(axis.title.x = element_blank())

p2 + p1 + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(widths = c(1,3)) &
  theme(plot.tag = element_text(size = 8))

ggsave('Figures/Figure1_LakeMichigan_WatershedSaltUse_Choride.png',width = 6.5, height = 2.6, units = 'in', dpi = 500)
# copy the file to Overleaf Dropbox 
ggsave('~/Dropbox/Apps/Overleaf/Lake Michigan Chloride/Figures/Figure1_LakeMichigan_WatershedSaltUse_Choride.png',width = 6.5, height = 2.6, units = 'in', dpi = 500)


