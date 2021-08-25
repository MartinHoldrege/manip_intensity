# martin holdrege

# script started April 15, 2021

# figure of distiribtion of daily event size, and key descriptive stats
# for appendix of manuscript

# dependencies ------------------------------------------------------------

library(tidyverse)
source("../sitedata_analysis/scripts/fig_params.R")
# read in data ------------------------------------------------------------


ppt_path <- "../sitedata/ppt/markov_ppt_200sites.csv"
df <- ppt_markov0 <- read_csv(ppt_path) %>% 
  filter(PPT_cm > 0) # to reduce size 


# descriptive stats -------------------------------------------------------


# PPt vectors for each intensity level
ppt_l <- split(df, df$intensity) %>% 
  map(. , function(df) df$PPT_cm[df$PPT_cm > 0])

# percentiles
levs <- c(0.001, 0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99, 0.999)

x <- map(ppt_l, quantile, probs = levs)
x
names(x)
names_intense <- str_subset(names(x), "intensity")

# each percentile is doubled, roughly for 2x intensity, etc
map(x[names_intense], function(vec) vec/x$ambient)

map_dbl(ppt_l, mean)

min(df$PPT_cm) # min is 0.01, no 0! (must be truncation in the underlying
# c code)


# probability theory ------------------------------------------------------

# because we're using a truncated normal distribution in the 
# WGEN, doubling mean event size and sd also doubles each percentile. 
# I don't understand why (this isn't the case with a 'regular' normal
# distribution)

y1 <- truncnorm::qtruncnorm(p = levs, 0, mean = 1, sd = 1)
y2 <- truncnorm::qtruncnorm(p = levs, 0, mean = 2, sd = 2) # double mean and sd

y2/y1 # value of each percentile exactly doubled, as in the simulated WGEN
# data

# figure ------------------------------------------------------------------

# mean distribtuion across 200 sites
jpeg("figures/distribution_mean.jpg", width = 4, height = 3, units = "in",
     res = 600)

df %>% 
  filter(PPT_cm > 0) %>% 
  ggplot(aes(PPT_cm, color = intensity)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(values = cols_intensity) +
  labs(x = "Daily precipitation event size (cm)",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 5)) +
  theme(legend.title = element_blank())

dev.off()
