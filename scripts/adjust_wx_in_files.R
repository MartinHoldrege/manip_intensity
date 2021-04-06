# Martin Holdrege

# script started March 3, 2021

# purpose is to try and adjust the the two .in files for the stepwat2
# weather generator to increase precip intensity. 

# NEXT: consider figuring out issue with doy 366 in mkv_doy.in file
# try determing the issue with changing sd, and why expected
# value isn't perfect. 


# dependencies ------------------------------------------------------------

library(rSOILWAT2) 
library(tidyverse)
library(precipr)
source("scripts/functions.R")

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
coeffs_2x <- precipr::adjust_mkv_in(mkv_in = coeffs, data = wdata,
                                    mean_mult = 2,
                                    adjust_sd = TRUE)
# this issues suggests that both expected_ppt and adjust_mkv_in functions
# have issues!
expected_ppt(coeffs_2x); expected_ppt(coeffs)
# compare coeffs
head(coeffs$mkv_doy)
head(coeffs_2x$mkv_doy)


wout2 <- dbW_generateWeather(x_empty, years = 2000:2500,
                             wgen_coeffs = coeffs_2x)
# this difference shows that expected_ppt still isn't being calculated
# correctly
calc_map(wout2); expected_ppt(coeffs_2x)  

calc_map(wdata); expected_ppt(coeffs) # should be exactly(?) the same
compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout2),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "2x_intensity")


# testing coeffs ----------------------------------------------------------

# CONINTINUE here--note! increasing PPT_sd increases annual ppt! why?--because
# negatives can be created by the normal distribution???, but aren't excepted?
# if that is true, why is MAP correctly calculated for original PPT_avg and
# PPT_sd values??

# could it be cause be this code in SW_Markov.c?
# /* Calculate Precipitation:
#   prop = probability that it precipitates today depending on whether it
# was wet (precipitated) yesterday `wetprob` or
# whether it was dry yesterday `dryprob` */
#   prob = (GT(*rain, 0.0)) ? SW_Markov.wetprob[doy0] : SW_Markov.dryprob[doy0];
# 
# p = RandUni(&markov_rng);
# if (LE(p, prob)) {
#   x = RandNorm(SW_Markov.avg_ppt[doy0], SW_Markov.std_ppt[doy0], &markov_rng);
#   *rain = fmax(0., x);
# } else {
#   *rain = 0.;
# }


# testing effects of changing coeffs
coeffs_test <- coeffs
coeffs_test$mkv_doy$PPT_sd <- coeffs$mkv_doy$PPT_sd*2
wout3 <- dbW_generateWeather(x_empty, years = years,
                             wgen_coeffs = coeffs_test)
calc_map(wout3)
expected_ppt(coeffs_test)
compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout3),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "test_intensity")

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
    summarise(PPT_cm = sum(PPT_cm)) %>% 
    pull(PPT_cm) %>% 
    mean()
  
  # markov files
  out <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                 imputation_type = "mean")
  out$map_obs <- map_obs
  out
})

# calculating the mean z score of zero ppt (given the mean and sd for that day)
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
map_obs$map_exp <- map_dbl(coeff_l, expected_ppt)

map <- map_sim %>% 
  rename(map_sim = MAP) %>% 
  left_join(map_obs, by = "site") %>% 
  arrange(site) %>% 
  mutate(z0 = z0,
         map_diff = map_sim - map_obs,
         map_diff_perc = map_diff/map_obs*100,
         map_exp_diff = map_exp - map_obs)


hist(map$map_diff_perc)
plot(map$map_diff_perc ~ map$z0)

# simulated and expected map should be same, if expected_ppt function
# were actually appropriately calculating expected ppt. it seems that it is 
# not. 
with(map, plot(map_exp~map_sim)); abline(0, 1)
with(map, plot(map_exp~map_obs)); abline(0, 1)
with(map, hist((map_exp_diff)/map_obs*100)) # % difference between observed and expected map
mean(map$map_diff)
mean(map$map_diff_perc)
median(map$map_diff)

# site for which expected map farthest off
filter(map, abs(map_exp_diff) == max(abs(map_exp_diff)))

# CONTINUE--just trying to figure out what the difference is 
# between coeffs were expected vals exactly match observed map
# and those that don't

good_exp <- map %>% 
  filter(map_exp_diff == 0) %>% 
  pull(site)

coeff_l[[good_exp[1]]]$mkv_doy$PPT_avg %>% 
  pull() %>% 
  sum()

check<- map_dbl(coeff_l, function(x) {
  sum(x$mkv_doy$PPT_avg > 0 & x$mkv_doy$PPT_sd == 0)
  
})
which(check == 0)
good_exp
coeff_l[[good_exp[1]]]$mkv_doy %>% View()
View(coeff_l[[1]]$mkv_doy)
# very strangely p_W_W for doy 366 is always 1 and p_W_D for doy 366 is always
# 0, strange, not sure why! It's possible that it has something to do with 
# summing across NAs?. Turns out that it is becuase in the weather
# data base doy 366 is always just a repeat of doy 365. ie if 365 was
# wet then 366 must also be wet. 

map(coeff_l[1:10], function(x) tail(x$mkv_doy))

