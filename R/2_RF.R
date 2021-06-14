library(ranger)
library(ggpubr) # for as_ggplot function to get legend
library(cowplot)
library(forestFloor)
source("R/Functions/ranger_RFadaptor.R")
source("R/Functions/ranger_plot.forestFloor.HD.R")

## Random Forest covariance matrix, with only predictors selected ####
trib_Cl.select <- trib_Cl %>% 
  group_by(hydroID_GLAHF, streamName) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE) %>% 
  ungroup() 

rf_cov =  trib_Cl.select %>%  dplyr::select(urban:Areakm2, 
                                            Population_Density_n_km2, 
                                            Road_Density_kmroad_km2, 
                                            MeanImperviousness)

sapply(rf_cov, function(x) sum(is.na(x))) # Check if there are NA values

# Sampling routine to use 95% of lakes as in-bag samples  ####
ntree = 1000

# Custom inbag sampling routine 
random_lake_samps <- lapply(1:ntree, function(i){
  unique_lakes <- unique(trib_Cl.select$hydroID_GLAHF)
  lake_samp <- sample(unique_lakes, size =0.95*length(unique_lakes), replace=F) # In-bag uses 95% of lakes
  samp = as.integer(trib_Cl.select$hydroID_GLAHF %in% lake_samp)
  return(samp)
}
)

## Run RF model ####
rf_model <- ranger(dependent.variable.name = 'chloride',
                   data = data.frame(chloride = trib_Cl.select$chloride, rf_cov),
                   inbag = random_lake_samps,
                   mtry = 4,
                   num.trees = ntree, quantreg = T,
                   importance = 'permutation',
                   keep.inbag = TRUE)
rf_model

# Calculate oob quantiles
oob_quantiles <- predict(rf_model, type = 'quantiles', quantiles = c(0.05,0.50,0.95))

#variable importance
v <- as.numeric(rf_model$variable.importance)
w <- as.character(names(rf_model$variable.importance))
DF <- data.frame(w=w,v=as.numeric(v)) %>% arrange(v)
DF$w <- factor(DF$w, levels = DF$w)

# variable importance plot 
pvar = ggplot(DF, aes(x=w, y=v,fill=v))+
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  scale_fill_gradient(low = 'lightsteelblue3', high = 'lightsteelblue4') +
  ylab("Variable Importance") + xlab("")+
  theme_bw(base_size = 8) +
  # theme(axis.text = element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=F)
pvar

# Forest floor plots
ff_rf_model <- ranger_RFadaptor(rf_model,trib_Cl.select$chloride)
ffra = forestFloor(ff_rf_model,rf_cov,calc_np = T)
ffra$FCmatrix

# Row sums should add up to predicted value
rws = rowSums(ffra$FCmatrix)
rowMeans(ffra$FCmatrix)
plot(rws, ff_rf_model$predicted)
abline(0,1)
lm(ff_rf_model$predicted ~ rws)

# varNames = read_csv('LAGOS_prediction/variableNames.csv')
ffra2 = ffra 
names = data.frame(Name = names(ffra2$X), FullName = 
                     c('Urban (%)', 'Barren (%)','Forest (%)','Shrubland (%)',
                       'Herbaceous (%)','Agriculture (%)','Wetland (%)',
                       'Strahler Order','Area (km2)','Population (#/km2)','Road Density (km/km2)','Mean Imperviousness (%)')) #%>% left_join(varNames)
ffra2$imp_ind = order(ffra2$importance,decreasing = T)

library(scales)
Col = fcol.HD(ffra2,1)
# plot(ffra, plot_seq=c(1,2,3), plot_GOF=F, limitY=F, col=Col, orderByImportance = T, pch = 16)
# Plot feature contribution plots
pp = plot.forestFloor.HD(ffra2,plot_seq=c(1,2,3,4,5,6,7,8), cols = Col, varNames = names, shape = 16, size = 0.7)
# do.call(plot_grid, c(pp, list(nrow = 2, align = 'hv'))) # plot multiple plots 

# Fake plot just to get legend
WSlegend = ggplot() +
  geom_point(aes(x=(ffra2$X$urban), y=ffra2$FCmatrix[,ffra2$imp_ind[1]], 
                 fill = (ffra2$X$urban)), pch = 21, color = 'grey50') + 
  scale_fill_distiller(palette='RdYlBu', direction = -1, name = 'Imperviousness (%)',
                       breaks= c(25,50,75), labels = c(25,50,75)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  theme_bw(base_size = 8) +
  theme(legend.text = element_text(size=8),
        legend.title = element_text(size=8)) 

# Using the cowplot package
legend <- cowplot::get_legend(WSlegend)

# layout <- "
# AAAAAAA
# BCCDDEE
# #FFGGHH
# "
# pvar + as_ggplot(legend) +pp[[1]] + pp[[2]] + pp[[3]] + pp[[4]] + pp[[5]] + pp[[6]] +
#   plot_layout(design = layout)
# ggsave('Figures/Figure6_RF.png',width = 6.5, height = 5, units = 'in', dpi = 500)
# 
# plot_grid(pvar, as_ggplot(legend),
#           pp[[1]], pp[[2]], pp[[3]], pp[[4]], pp[[5]], pp[[6]], 
#           rel_widths = c(2,1,1,1),
#           nrow = 3, align = 'h', labels = c('a','b','c','d','e','f'),label_size = 10)

toprow = plot_grid(pvar, as_ggplot(legend),
                   rel_widths = c(4,1),
                   nrow = 1, align = 'h', labels = c('a', 'b'),label_size = 10)
bottomrow = plot_grid(pp[[1]], pp[[2]], pp[[3]], pp[[4]], pp[[5]], pp[[6]], 
          nrow = 2, align = 'h', labels = c('c','d','e','f','g','h'),label_size = 10)
plot_grid(toprow, bottomrow, nrow = 2, rel_heights = c(1,2))

ggsave('Figures/Figure6_RF.png',width = 6.5, height = 6, units = 'in', dpi = 500)
