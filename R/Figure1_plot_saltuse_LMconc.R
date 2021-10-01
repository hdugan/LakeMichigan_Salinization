library(scales)
library(tidyverse)
library(patchwork)
library(lubridate)

### Figure 1 
# a) Estimated road salt use in the Lake Michigan watershed derived from \citep{bock_estimates_2018}. 
# b) Chloride concentrations in Lake Michigan collected by multiple agencies (US Environmental Protection Agency 
# (EPA) Great Lakes National Program Office, Illinois EPA, Wisconsin Department of Natural Resources, Little River 
# Band of Ottawa Indians) across multiple sites. Data from the US Water Quality Portal.}

# Read in WQP data and remove any outliers based on > 5 SD from the linear model residual mean 
data.EPA.df.MI = read_csv('Data/WQP/allLakeMichigan_EPA.csv')
data.EPA.df.MI.lm <- lm(Result ~ (ActivityStartDate), data = data.EPA.df.MI) # Linear Model 
data.EPA.df.MI.outliers = data.EPA.df.MI %>% 
  filter(!is.na(Result)) %>% 
  mutate(fitted = data.EPA.df.MI.lm$fitted.values, 
         resids = Result - fitted) %>%  
  mutate(outlier = if_else(abs(resids) > mean(resids) + 5*sd(resids), TRUE, FALSE))
table(data.EPA.df.MI.outliers$outlier)

allD2 = read_csv('Data/WQP/allLakeMichigan_filtered.csv') %>% 
  filter(year(ActivityStartDate) >= 1980) # Does not include Milwaukee Metropolitan Sewerage District
allD2.lm <- lm(Result ~ (ActivityStartDate), data = allD2)  # Linear Model 
allD2.outliers = allD2 %>% 
  filter(!is.na(Result)) %>% 
  mutate(fitted = allD2.lm$fitted.values, 
                        resids = Result - fitted) %>%  
  mutate(outlier = if_else(abs(resids) > mean(resids) + 5*sd(resids), TRUE, FALSE))
table(allD2.outliers$outlier)

55/(6064+1542) # total data tagged as outlier

# Read in USGS salt use data
saltuse.df = read_csv('Data/USGSsalt/saltuseDF.csv')



p1 = ggplot(data.EPA.df.MI.outliers %>% filter(outlier == FALSE)) +   # Remove 3 highest and lowest values +
  geom_point(aes(x = ActivityStartDate, y = Result), shape = 21, fill = 'lightblue4', stroke = 0.1, alpha = 0.7) +
  xlim(as.Date('1982-01-01'),NA) +
  geom_point(data = allD2.outliers %>% filter(outlier == FALSE),
             aes(x = ActivityStartDate, y = Result), shape = 21, fill = 'lightblue4', stroke = 0.1, alpha = 0.7) +
  ylab(bquote(Chloride ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())

p2 = ggplot(saltuse.df) +
  geom_col(aes(x = years, y = saltuse_metton), fill = 'lightblue4') +
  theme_bw(base_size = 8) +
  ylab('Road salt (Tg)') +
  scale_y_continuous(label = comma) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))

p2 + p1 + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(widths = c(1,3)) &
  theme(plot.tag = element_text(size = 8))

ggsave('Figures/Figure1_LakeMichigan_WatershedSaltUse_Choride.png',width = 6.5, height = 2.6, units = 'in', dpi = 500)


