# Martin Holdrege

# script started March 3, 2021

# purpose is to try and adjust the the two .in files for the stepwat2
# weather generator to increase precip intensity. 

# Continue here--this script hasn't really be started even
# dependencies ------------------------------------------------------------

library(rSOILWAT2) # 
library(tidyverse)

# -------------------------------------------------------------------------

res1 <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))

# illustration of the bug in rSOILWAT2::dbW_estimate_WGen_coefs

# make sure the correct verstion of rSOILWAT2 is being used (ie the bug fix)
res2 <- dbW_estimate_WGen_coefs(wdata, propagate_NAs = FALSE) # default
head(res2[[1]]) # no CF_ for dry days
res2 <- dbW_estimate_WGen_coefs(wdata, propagate_NAs = TRUE)
head(res2[[1]])

wdata <- precipr::wx_data %>% 
  rename(Year = year) %>% 
  select(Year, DOY, Tmax_C, Tmin_C, PPT_cm)

# wdata$PPT_cm[wdata$DOY == 1] <- 0 # tested the effect of no rain days


# The bias from imputation shouldn't actually be a problem. If 
# no rain days for a given DOY then mean and sd are 0. 
# sd becomes NA with only one rain day. Imputing that sd
# won't cause a directional shift in total precip, and seems totally appropriate. 
res1 <- dbW_estimate_WGen_coefs(wdata, imputation_type = "mean")

res1[[1]]

# MarkovWeatherFileGenerator.R is already doing this:
# res <- rSOILWAT2::dbW_estimate_WGen_coefs(sw_weatherList[[s]][[h]], imputation_type = "mean",
#                                           propagate_NAs = FALSE)




# these are the DOYS throwing errors, they all just have 1 day of rain across
# 30 years. 

wdata %>% 
  filter(DOY %in% c(3, 7, 12, 14, 18, 19, 27, 32, 35, 51, 70, 134, 205, 213, 
                    231, 232, 239, 271, 283, 297, 298, 299, 301, 304, 306, 
                    314, 317, 324, 335, 339, 343, 345, 353, 356, 359) 
         & PPT_cm > 0) %>% 
  arrange(DOY)

rSOILWAT2::week



res2 <- dbW_estimate_WGen_coefs(wdata)
?rSOILWAT2::print_mkv_files()
sw_in <- rSOILWAT2::sw_exampleData
swMarkov_Prob(sw_in) <- res2[["mkv_doy"]]
swMarkov_Conv(sw_in) <- res2[["mkv_woy"]]
