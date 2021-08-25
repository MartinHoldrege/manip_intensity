# Martin Holdrege


# Script started May 6, 2021

# purpose is to calculate the mean change in temperature (current vs gcm)
# to determine how much temp should be warmed for warming treatment


# dependencies ------------------------------------------------------------

library(tidyverse)

# read in data ------------------------------------------------------------

# MAT for each site/gcm calculated in 01_calc_MAT_by_GCM.R
mat1 <- read_csv("data-processed/MAT_by_GCM-site.csv")

# create summaries --------------------------------------------------------

mat2 <- mat1 %>% 
  # rudimentary estimate of mean temp
  mutate(MAT = (Tmax_mean + Tmin_mean)/2)

# median across gcms for given site/perdiod/rcp
mat_med <- mat2 %>% 
  group_by(RCP, period, site) %>% 
  summarise(MAT = median(MAT), 
            .groups = "drop")
  
mat_diff <- mat_med %>% 
  group_by(site) %>% 
  summarise(
    current = MAT[RCP == "Current" & period == "Current"],
    t_diff_d50_rcp45 = MAT[RCP == "RCP45" & period == "d50yrs"] - current,
    t_diff_d90_rcp45 = MAT[RCP == "RCP45" & period == "d90yrs"] - current,
    t_diff_d50_rcp85 = MAT[RCP == "RCP85" & period == "d50yrs"] - current,
    t_diff_d90_rcp85 = MAT[RCP == "RCP85" & period == "d90yrs"] - current,
    .groups = "drop") %>% 
  rename(MAT_current = current)

mat_diff

# mean difference across sites by scenerio/period
mat_diff_mean <- mat_diff%>% 
  summarise_at(.vars = vars(matches("t_diff")),
               .funs = mean)
mat_diff_mean

# # A tibble: 1 x 4
# t_diff_d50_rcp45 t_diff_d90_rcp45 t_diff_d50_rcp85 t_diff_d90_rcp85
# <dbl>            <dbl>            <dbl>            <dbl>
# 2.05             3.07             2.57             5.40
