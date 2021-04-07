# Martin Holdrege

# Code started April 7, 2021

# Purpose is to generated ambient and 2x intensity ppt for 14 sites
# to allow for further testing of whether manipulating the input
# markov files works

# dependencies ------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(rSOILWAT2)

# connect to db ---------------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)

# Diverse set of representative sites from KP
sites <- c(5, 15, 74, 76, 102, 103, 119, 124, 141, 156, 162, 172, 177, 184)

# estimate coeffs ---------------------------------------------------------

coeff_l <- map(sites, function(site) {
  wdata <- dbW_getWeatherData(Site_id = site)
  wdata_df <- wdata %>% 
    dbW_weatherData_to_dataframe() %>% 
    as.data.frame()
  # markov files
  coeffs_ambient <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                            imputation_type = "mean")

  # 2x markov files
  coeffs_2x <- precipr::adjust_coeffs(coeffs = coeffs_ambient, 
                                      data = wdata_df,
                                      mean_mult = 2,
                                      adjust_sd = TRUE)
  out <- list(coeffs_ambient = coeffs_ambient, coeffs_2x = coeffs_2x)
  out
})

# simulate wx -------------------------------------------------------------

out_path <- "../sitedata/ppt/markov_ppt_14sites_20210407.csv"

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
years <- 2000:2300
x_empty <- list(new("swWeatherData")) # empty weather object

# * ambient intensity -----------------------------------------------------

# generate weather just based on the input coeffs
map2(coeff_l, sites, function(x, site) {
    wout1 <- dbW_generateWeather(x_empty, years = years,
                                 wgen_coeffs = x$coeffs_ambient)
    
    out <- dbW_weatherData_to_dataframe(wout1) %>% 
      as.data.frame() %>% 
      mutate(site = site,
             intensity = "ambient")
    
    write_csv(out, out_path, append = TRUE)
    site # just returning site num so can watch progress
})


# * 2x intensity ----------------------------------------------------------

map2(coeff_l, sites, function(x, site) {

  wout1 <- dbW_generateWeather(x_empty, years = years,
                               wgen_coeffs = x$coeffs_2x)
  
  out <- dbW_weatherData_to_dataframe(wout1) %>% 
    as.data.frame() %>% 
    mutate(site = site,
           intensity = "2x intensity")
  
  write_csv(out, out_path, append = TRUE)
  site
})
