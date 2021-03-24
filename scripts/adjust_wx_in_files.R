# Martin Holdrege

# script started March 3, 2021

# purpose is to try and adjust the the two .in files for the stepwat2
# weather generator to increase precip intensity. 

# dependencies ------------------------------------------------------------

library(rSOILWAT2) 
library(tidyverse)


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
head(coeffs[[1]])

# simulate some weather data ----------------------------------------------
years <- 2000:2300
x_empty <- list(new("swWeatherData")) # empty weather object

# generate weather just based on the input coeffs
wout1 <- dbW_generateWeather(x_empty, years = years,
                             wgen_coeffs = coeffs, seed = 123)

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
                                    adjust_sd = FALSE)

# compare coeffs
head(coeffs$mkv_doy)
head(coeffs_2x$mkv_doy)


wout2 <- dbW_generateWeather(x_empty, years = years,
                             wgen_coeffs = coeffs_2x, seed = 1)

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
coeffs_test <- precipr::adjust_mkv_in(mkv_in = coeffs, data = wdata,
                                      mean_mult = 2, adjust_sd = TRUE)
coeffs_test$mkv_doy$PPT_sd/coeffs$mkv_doy$PPT_sd
wout3 <- dbW_generateWeather(x_empty, years = years,
                             wgen_coeffs = coeffs_test, seed = 2)
compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout3),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "test_intensity")

## examining issues regarding expected values on given days
# here just calculated expected on days given previous day was dry. But 
# think the logic holds either way

# if it rains, probability of drawing a value from normal distribution
# that is greater than 0 (if - value drawn then it is replace w/ 0),
# thereby creating a dry day.

# expected value of ppt
expected_ppt <- function(coeffs) {
  prob_gt0 <- with(coeffs$mkv_doy,
                   pnorm(0, mean = PPT_avg, sd = PPT_sd, lower.tail = FALSE))
  
  with(coeffs$mkv_doy,
       pnorm(0, mean = PPT_avg*2, sd = PPT_sd*15, lower.tail = TRUE))[1:5]
  # corrected Prob wet given prev day dry
  P_W_D_cor <- coeffs$mkv_doy$p_W_D*prob_gt0

  # expected values on dry days
  ex <- P_W_D_cor*truncnorm::etruncnorm(a = 0, b = Inf,
                                        mean = coeffs$mkv_doy$PPT_avg,
                                        sd = coeffs$mkv_doy$PPT_sd)
  sum(ex) # expected total (if not consecutive wet days)
}

# doubling sd and avg while cutting in half P_W_D
# should create same expected values (it does according to this calculation
# but isn't in the wgen)
expected_ppt(coeffs)
expected_ppt(coeffs_test)


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
  # mean z weighted by rough approximation of the expected ppt on a given day
  out <- weighted.mean(z_vec, w = x$mkv_doy$p_W_W*x$mkv_doy$p_W_D*x$mkv_doy$PPT_avg, 
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

map <- map_sim %>% 
  rename(map_sim = MAP) %>% 
  left_join(map_obs, by = "site") %>% 
  arrange(site) %>% 
  mutate(z0 = z0,
         map_diff = map_sim - map_obs,
         map_diff_perc = map_diff/map_obs*100)


hist(map$map_diff_perc)
plot(map$map_diff_perc ~ map$z0)
mean(map$map_diff)
mean(map$map_diff_perc)
median(map$map_diff)

# I would expect a strong positive relationship between z0  and map_diff
# but I'm not seeing. My understanding of the wgen also doesn't explain
# why so many sites have simulated map lower than observed map
lm(map$map_diff_perc ~ map$z0) %>% 
  summary()

