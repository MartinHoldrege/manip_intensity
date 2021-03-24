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
years <- 2000:2500
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
                                    mean_mult = 2)

# compare coeffs
head(coeffs$mkv_doy)
head(coeffs_2x$mkv_doy)
all.equal(coeffs_2x$mkv_doy$p_W_D, coeffs$mkv_doy$p_W_D/2)
all.equal(coeffs_2x$mkv_doy$p_W_W, coeffs$mkv_doy$p_W_W/2)
all.equal(coeffs_2x$mkv_doy$PPT_avg, coeffs$mkv_doy$PPT_avg*2)

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
coeffs_test <- coeffs
mean_mult <- 4
coeffs_test$mkv_doy$PPT_avg <- coeffs$mkv_doy$PPT_avg*mean_mult
coeffs_test$mkv_doy$PPT_sd <- coeffs$mkv_doy$PPT_sd*(mean_mult*10)
coeffs_test$mkv_doy$p_W_W <- coeffs$mkv_doy$p_W_W/mean_mult
coeffs_test$mkv_doy$p_W_D <- coeffs$mkv_doy$p_W_D/mean_mult

wout3 <- dbW_generateWeather(x_empty, years = 2000:2300,
                             wgen_coeffs = coeffs_test, seed = 2)
compare_weather(ref_weather = as.matrix(wdata), 
                weather = dbW_weatherData_to_dataframe(wout3),
                N = 1, 
                path = "figures/compare_wgen/", 
                tag = "test_intensity")
