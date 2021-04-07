# Martin Holdrege

# script started March 3, 2021

# purpose is to try and adjust the the two .in files for the stepwat2
# weather generator to increase precip intensity. 



# dependencies ------------------------------------------------------------

library(rSOILWAT2) 
library(tidyverse)
library(precipr) # my misc functions

# read in observed map data -----------------------------------------------

# calculated based on output from 200 sites, 150 yrs, 200 niter, ambient intensity
map_sim <- read_csv("../sitedata/ppt/map_fromwgen_ambient_200sites.csv")

# connect to database -----------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)

# example data ------------------------------------------------------------

wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))


# create .in files --------------------------------------------------------

coeffs <- dbW_estimate_WGen_coefs(wdata, propagate_NAs = FALSE)
head(coeffs[[2]])

# simulate some weather data ----------------------------------------------
years <- 2000:2300
x_empty <- list(new("swWeatherData")) # empty weather object

# generate weather just based on the input coeffs
wout1 <- dbW_generateWeather(x_empty, years = years,
                             wgen_coeffs = coeffs)
calc_map(wout1) # MAP of simulated ppt
calc_map(wdata) # map of original data
expected_ppt(coeffs) # expected map based on markov file

# generate figures that compare reference weather to simulated weather
compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout1),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "ambient_intensity")


# increase intensity ------------------------------------------------------

# adjusting coeffs to double ppt intensity
coeffs_2x <- precipr::adjust_coeffs(coeffs = coeffs, data = wdata,
                                    mean_mult = 2,
                                    adjust_sd = TRUE)

expected_ppt(coeffs_2x, adjust_for_truncnorm = TRUE)
expected_ppt(coeffs, adjust_for_truncnorm = TRUE)
# compare coeffs
head(coeffs$mkv_doy)
head(coeffs_2x$mkv_doy)

wout2 <- dbW_generateWeather(x_empty, years = years,
                             wgen_coeffs = coeffs_2x)
# this difference shows that expected_ppt still isn't being calculated
# correctly
calc_map(wout2); expected_ppt(coeffs_2x, adjust_for_truncnorm = TRUE)  

# note when compring to original data adjust_for_truncnorm = FALSE
# gives closer match, b/ the wgen actually adds some biases
calc_map(wdata); expected_ppt(coeffs) 

compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout2),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "2x_intensity")


# create coeffs for each site ---------------------------------------------

sites <- 1:200

# list of coeffs for each site
coeff_l <- map(sites, function(site) {
  wdata <- dbW_getWeatherData(Site_id = site)
  wdata_df <- wdata %>% 
    dbW_weatherData_to_dataframe() %>% 
    as.data.frame()
  
  # true MAP 
  map_obs <- wdata_df %>% 
    group_by(Year) %>% 
    summarise(PPT_cm = sum(PPT_cm), .groups = "drop") %>% 
    pull(PPT_cm) %>% 
    mean()
  
  # markov files
  out <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                 imputation_type = "mean")
  out$map_obs <- map_obs
  out
})

# calculating the mean z score of zero ppt (given the mean and sd for that day)
# large z score suggests that many random draws would be negative and converted
# to 0 in wgen
z0 <- map_dbl(coeff_l, function(x) {
  z_vec <- (0 - x$mkv_doy$PPT_avg)/x$mkv_doy$PPT_sd # z score of 0
  z_vec[!is.finite(z_vec)] <- NA
  # mean z weighted by approximation of the expected ppt on a given day
  p_W <- calc_p_W(x$mkv_doy)
  out <- weighted.mean(z_vec, w = p_W*x$mkv_doy$PPT_avg, 
                       na.rm = TRUE)
  out
})
hist(z0)

site_max_z <- which(z0 == max(z0)) # site with highest (least negative z0)
coeff_l[[site_max_z]]$mkv_doy

# comparing observed and calc MAP -----------------------------------------

map_obs <- map_dbl(coeff_l, function(x) x$map_obs) %>% 
  tibble(map_obs = .,
         site = sites)

# expected map based on markov file and understanding of wgen
map_obs$map_exp <- map_dbl(coeff_l, expected_ppt, adjust_for_truncnorm = TRUE)


map <- map_sim %>% 
  rename(map_sim = MAP) %>% 
  left_join(map_obs, by = "site") %>% 
  arrange(site) %>% 
  mutate(z0 = z0,
         map_diff = map_sim - map_obs,
         map_diff_perc = map_diff/map_obs*100,
         map_exp_diff = map_exp - map_sim)


hist(map$map_diff_perc)
plot(map$map_diff_perc ~ map$z0)


max(map$map_exp_diff) # the maximum difference between expected map
# and simulated map is only 1/2 mm
with(map, plot(map_exp~map_sim)); abline(0, 1)

# difference between observed and simulate map are due to some bias
# in the wgen
with(map, plot(map_exp~map_obs)); abline(0, 1)
with(map, plot(map_sim~map_obs)); abline(0, 1)

