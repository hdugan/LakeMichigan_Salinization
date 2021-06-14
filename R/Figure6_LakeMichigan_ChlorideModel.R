#' Based on:
#' @article{sonzogni_chloride_1983,
#'   title = {Chloride {Pollution} of the {Great} {Lakes}},
#'   volume = {55},
#'   number = {5},
#'   journal = {Journal (Water Pollution Control Federation)},
#'   author = {Sonzogni, William and Richardson, William and Rodgers, Paul and Monteith, Timothy},
#'   year = {1983},
#'   pages = {513--521}
#' }

# C(t) = (sum(W)/Q) * (1 - exp(-Q/V)) + (Co * exp(-Q/V))

Co = 7 # mg/L or g/me in 1975
# W = 0.99e12 # g # Fom Sognozi
W = 1.079e12 + 0.025e12 # g # This study tributary + precipitation load 
# Q = 45.2e9 #m3/year # Fom Sognozi
Q = 44.9e9 #m3 /year (Chapra)
V = 4976 * 1e9

RT = V/Q

# input load in grams
calcCl <- function(load) {
  C = rep(Co, 50)
  for (t in 2:500) {
    C[t] = (load/Q) * (1 - exp(-Q/V)) + (C[t-1] * exp(-Q/V))
  }
return(C)
}

C1 = calcCl(load = W) # g # Out study 
C2 = calcCl(load = 1.2e12) # g # Chapra 2009 low estimate
C3 = calcCl(load = 1.6e12) # g # Chapra 2009 high estimate

LakeMichigan_Model = data.frame(years = 1975:2474, C1, C2, C3)
# Read in WQP data to add to plot 
data.EPA.df.MI = read_csv('Data/WQP/allLakeMichigan_EPA.csv')
allD2 = read_csv('Data/WQP/allLakeMichigan_filtered.csv')

# Figure 2 recreation: Pojections of chloride concentrations over time in response to current external loads
ggplot(LakeMichigan_Model) +
  geom_vline(xintercept = 2020, linetype = 2) +
  geom_point(data = data.EPA.df.MI, aes(x = year(ActivityStartDate), y = Result), shape = 21, fill = 'lightblue4', stroke = 0.1, alpha = 0.7) +
  # geom_point(data = allD2 %>% arrange(-Result) %>%  # arrange by Results
  #              slice(3:(n() - 3)), 
  #            aes(x = year(ActivityStartDate), y = Result), shape = 21, fill = 'lightblue4', stroke = 0.1, alpha = 0.7) +
  geom_path(aes(x = years, y = C2), linetype = 2) +
  geom_path(aes(x = years, y = C3), linetype = 3) +
  geom_path(aes(x = years, y = C1)) +
  annotate('text',x = 2350, y = 22.2, label = 'Load = 1.104 Tg', size = 2) +
  annotate('text',x = 2350, y = 27.5, label = 'Load = 1.2 Tg', size = 2) +
  annotate('text',x = 2350, y = 33, label = 'Load = 1.6 Tg', size = 2) +
  ylim(0,40) +
  ylab(bquote(Chloride ~ (mg~L^-1))) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())

ggsave('Figures/Figure7_MassBalanceModel.png', width = 4, height = 3, units = 'in', dpi = 500)

