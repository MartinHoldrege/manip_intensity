# Martin Holdrege

# Code started April 7, 2021

# Purpose is to generated ambient and 2x intensity ppt for 14 sites
# to allow for further testing of whether manipulating the input
# markov files works

# NEXT: consider running this (in parallel) on the cluster for 200 sites
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
names(sites) <- sites
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


# examining coeffs --------------------------------------------------------
# difference in expected MAP between ambient and 2x. Note these differences
# are miniscule but for some sites (especially 119 and 177), the simulated
# ambient and 2x MAPs were quite different. This suggests the expected ppt 
# function is missing something. 

exp_diffs <- map_dbl(coeff_l, function(x) {
  ex_2x <- precipr::expected_ppt(x$coeffs_2x, adjust_for_truncnorm = TRUE)
  ex_amb <- precipr::expected_ppt(x$coeffs_amb, adjust_for_truncnorm = TRUE)
  ex_2x - ex_amb
})
exp_diffs
max(exp_diffs)

exp_nwet_2x <- map_dbl(coeff_l, function(x) {
  precipr::expected_nwet(x$coeffs_2x, adjust_for_truncnorm = TRUE)
})
exp_map_2x <- map_dbl(coeff_l, function(x) {
  precipr::expected_ppt(x$coeffs_2x, adjust_for_truncnorm = TRUE)
})
exp_map_2x/exp_nwet_2x
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
seed <- 12345
map2(coeff_l, sites, function(x, site) {
    wout1 <- dbW_generateWeather(x_empty, years = years,
                                 wgen_coeffs = x$coeffs_ambient,
                                 seed = seed)
    
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
                               wgen_coeffs = x$coeffs_2x,
                               seed = seed)
  
  out <- dbW_weatherData_to_dataframe(wout1) %>% 
    as.data.frame() %>% 
    mutate(site = site,
           intensity = "2x intensity")
  
  write_csv(out, out_path, append = TRUE)
  site
})




# long runs ---------------------------------------------------------------
# 2000 years of weather simulate for each site. This was done to see
# if doubling intensity is working. Stochasticity in MAP requires long runs
# Note--this code will be slow

# summarize daily weather data to key yearly variables
summarize2yr <- function(df) {
  df %>% 
    group_by(.data$Year) %>% 
    summarize(
      ap = sum(.data$PPT_cm),
      nwet = sum(.data$PPT_cm > 0),
      Tmin_C = mean(.data$Tmin_C),
      Tmax_C = mean(.data$Tmax_C),
      .groups = "drop") 
}

# generate and summarize weather data
generate_summarize <- function(x_empty, years, wgen_coeffs, site, ...) {
  wdata <- dbW_generateWeather(x_empty, years = years,
                              wgen_coeffs = wgen_coeffs, ...)
  
  out <- dbW_weatherData_to_dataframe(wdata) %>% 
    as.data.frame() %>% 
    summarize2yr() %>% 
    mutate(site = site)
  
  out
}

years1 <- 2000:2999
years2 <- 3000:3999

# for testing:
# years1 <- 2001:2010
# years2 <- 2011:2020


# doing it this way so that at no time is more than a 1000 year df stored
# in memory, before it is summarized, and removed from function environment
yearly_amb <- map2_dfr(coeff_l, sites, function(x, site) {
  print(site)
  yr_df1 <- generate_summarize(x_empty = x_empty, years = years1,
                     wgen_coeffs = x$coeffs_ambient,
                     site = site)

  yr_df2 <- generate_summarize(x_empty = x_empty, years = years2,
                               wgen_coeffs = x$coeffs_ambient,
                               site = site)
  out <- bind_rows(yr_df1, yr_df2)
  out
})

yearly_amb$intensity = "ambient"
yearly_2x <- map2_dfr(coeff_l, sites, function(x, site) {
  print(site)
  yr_df1 <- generate_summarize(x_empty = x_empty, years = years1,
                               wgen_coeffs = x$coeffs_2x,
                               site = site)
  
  yr_df2 <- generate_summarize(x_empty = x_empty, years = years2,
                               wgen_coeffs = x$coeffs_2x,
                               site = site)
  out <- bind_rows(yr_df1, yr_df2)
  out
})
yearly_2x$intensity = "2x intensity"

yearly_comb <- bind_rows(yearly_amb, yearly_2x)

write_csv(yearly_comb, "../sitedata/ppt/markov_ppt_14sites_yrly_means_20210409.csv")

tail(yearly_comb)
site_means <- yearly_comb %>% 
  select(-Year) %>% 
  group_by(site, intensity) %>% 
  summarise_all(.funs = mean,
                .groups = "drop") 
site_diffs <- site_means %>%   
  group_by(site) %>% 
  mutate(ap_diff = ap[intensity == "2x intensity"] - ap[intensity == "ambient"],
         nwet_ratio = nwet[intensity == "2x intensity"]/nwet[intensity == "ambient"],
         Tmax_diff = Tmax_C[intensity == "2x intensity"] - Tmax_C[intensity == "ambient"],
         Tmindiff = Tmin_C[intensity == "2x intensity"] - Tmin_C[intensity == "ambient"]) %>% 
  filter(intensity == "ambient") %>% # duplicated rows otherwise
  select(-intensity)

mean(site_diffs$ap_diff)
site_diffs %>% 
  arrange(desc(abs(ap_diff)))

max_abs <- function(x) max(abs(x))
site_diffs %>% 
  ungroup() %>% 
  summarize_all(max_abs) 

# max ap_diff is 0.75, site 177


# further testing of site 177 ---------------------------------------------

start_yrs <- seq(from = 2000, to = 11000, by = 1000)
end_yrs <- start_yrs + 999
yearl_l <- map2(start_yrs, end_yrs, function(x, y) x:y)

if (FALSE) {
  # simulating just site 177 for 10,000 years
  df177 <- map_dfr(yearl_l, function(x) {
    df_amb <- generate_summarize(x_empty = x_empty, years =x,
                                 wgen_coeffs = coeff_l$`177`$coeffs_ambient,
                                 site = 177)
    df_amb$intensity <- "ambient"
    df_2x <- generate_summarize(x_empty = x_empty, years = x,
                                 wgen_coeffs = coeff_l$`177`$coeffs_2x,
                                 site = 177)
    df_2x$intensity <- "2x intensity"
    out <- bind_rows(df_amb, df_2x)
    out
  })
  nrow(df177)
  means_177 <- df177 %>% 
    select(-Year, -site) %>% 
    group_by(intensity) %>% 
    summarize_all(.funs = mean)
  with(means_177, ap[intensity == "2x intensity"] - ap[intensity == "ambient"])
  # diff is only -0.016424 cm!
  with(means_177, nwet[intensity == "2x intensity"]/nwet[intensity == "ambient"])
  # ratio is 0.4997265. Very close to 2
  
  # intensity       ap  nwet Tmin_C Tmax_C
  # <chr>        <dbl> <dbl>  <dbl>  <dbl>
  #   1 2x intensity  74.6  28.8  -1.29   15.7
  # 2 ambient       74.6  57.6  -1.32   15.7
}


rSOILWAT2::dbW_disconnectConnection()
