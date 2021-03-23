# Martin Holdrege

# script started March 3, 2021

# purpose is to try and adjust the the two .in files for the stepwat2
# weather generator to increase precip intensity. 

# dependencies ------------------------------------------------------------

library(rSOILWAT2) 
library(tidyverse)


# example data ------------------------------------------------------------

wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))


# create .in files --------------------------------------------------------

coeffs <- dbW_estimate_WGen_coefs(wdata, propagate_NAs = FALSE)
head(coeffs[[1]])

# simulate some weather data ----------------------------------------------

x_empty <- list(new("swWeatherData")) # empty weather object

# generate weather just based on the input coeffs
wout1 <- dbW_generateWeather(x_empty, years = 2000:2055,
                             wgen_coeffs = coeffs, seed = 123)

# generate figures that compare reference weather to simulated weather
compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout1),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "Example1-WeatherGenerator")


# NEXT: create modified coeffs and then compa