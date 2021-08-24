# Martin Holdrege

# Code started Aug, 23, 2021

# Purpose is to generated ambient and 2x intensity ppt for200 sites
# to provide descriptive stats for appendix of MS

# dependencies ------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(rSOILWAT2)

# connect to db ---------------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)

# Diverse set of representative sites from KP
sites <- 1:200
names(sites) <- sites
# estimate coeffs ---------------------------------------------------------

coeff_l <- map(sites, function(site) {
  wdata <- dbW_getWeatherData(Site_id = site)
  wdata_df <- wdata %>% 
    dbW_weatherData_to_dataframe() %>% 
    as.data.frame()
  out <- list()
  # markov files
  out[["coeffs_ambient"]] <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                            imputation_type = "mean")
  
  # 1.25 markov files
  out[["coeffs_1.25x"]] <- precipr::adjust_coeffs(coeffs = out$coeffs_ambient, 
                                      data = wdata_df,
                                      mean_mult = 1.25,
                                      adjust_sd = TRUE)
  # 2x markov files
  out[["coeffs_1.5x"]] <- precipr::adjust_coeffs(coeffs = out$coeffs_ambient, 
                                      data = wdata_df,
                                      mean_mult = 1.5,
                                      adjust_sd = TRUE)
  # 2x markov files
  out[["coeffs_2x"]] <- precipr::adjust_coeffs(coeffs = out$coeffs_ambient, 
                                      data = wdata_df,
                                      mean_mult = 2,
                                      adjust_sd = TRUE)
  out
})


# simulate wx -------------------------------------------------------------

out_path <- "../sitedata/ppt/markov_ppt_200sites.csv"

# create empty file so that column names exist to append to
out <- data.frame(
  Year = double(),
  DOY = double(),
  Tmax_C = double(),
  Tmin_C = double(),
  PPT_cm = double(),
  site = double(),
  intensity = character()
)
write_csv(out, out_path)
years <- 2000:2050 # short period to reduce computing time
x_empty <- list(new("swWeatherData")) # empty weather object

# * ambient intensity -----------------------------------------------------

# generate weather just based on the input coeffs
seed <- 12345
map2(coeff_l, sites, function(x, site) {

  wout1 <- dbW_generateWeather(x_empty, years = years,
                               wgen_coeffs = x$coeffs_ambient,
                               seed = seed)

  out <- dbW_weatherData_to_dataframe(wout1) %>% 
    as.data.frame() %>% 
    mutate(site = site,
           intensity = "ambient")
  # write in iteration of 'loop', so can avoid memory constrains
  write_csv(out, out_path, append = TRUE)
  print(site) # so can watch progress
  site # just returning site num
})


# * 2x intensity ----------------------------------------------------------

map2(coeff_l, sites, function(x, site) {
  
  wout1 <- dbW_generateWeather(x_empty, years = years,
                               wgen_coeffs = x$coeffs_2x,
                               seed = seed)
  
  out <- dbW_weatherData_to_dataframe(wout1) %>% 
    as.data.frame() %>% 
    mutate(site = site,
           intensity = "2x intensity")
  
  write_csv(out, out_path, append = TRUE)
  print(site)
  site
})


# * 1.25x and 1.5x --------------------------------------------------------
# this code run later, so not integrated into above

map2(coeff_l, sites, function(x, site) {
  # 1.25x
  wout1.25 <- dbW_generateWeather(x_empty, years = years,
                               wgen_coeffs = x$coeffs_1.25x,
                               seed = seed)
  
  out1.25 <- dbW_weatherData_to_dataframe(wout1.25) %>% 
    as.data.frame() %>% 
    mutate(site = site,
           intensity = "1.25x intensity")
  write_csv(out1.25, out_path, append = TRUE)
  # 1.5x
  wout1.5 <- dbW_generateWeather(x_empty, years = years,
                                  wgen_coeffs = x$coeffs_1.5x,
                                  seed = seed)
  
  out1.5 <- dbW_weatherData_to_dataframe(wout1.5) %>% 
    as.data.frame() %>% 
    mutate(site = site,
           intensity = "1.5x intensity")
  
  write_csv(out1.5, out_path, append = TRUE)
  print(site)
  site
})



rSOILWAT2::dbW_disconnectConnection()
