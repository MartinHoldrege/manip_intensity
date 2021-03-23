# Martin Holdrege

# Script started March 23, 2021

# Purpose is to simulate weather for a site where the following type of
# warnings (when running rSFSTEP2) were thrown, to see what is going on:
# WARNING: Zero growing season precipitation in year = 60 of iteration = 5


# dependencies ------------------------------------------------------------

library(tidyverse)
library(rSOILWAT2)
source("scripts/functions.R")


# read in logfile ---------------------------------------------------------

# sites that had zero growing season precip warnings
# for ambient intensity run of 200 sites

logfile <-  readLines("data-raw/logfile_compiled.txt")
bad_sites <- logfile %>% 
  str_subset("R_program") %>% 
  str_extract("\\d{1,3}") %>% 
  as.numeric()

# connect to database -----------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)


# extract weather data ----------------------------------------------------

wdata <- dbW_getWeatherData(Site_id = 85)

wdata_df <- wdata %>% 
  dbW_weatherData_to_dataframe() %>% 
  as.data.frame()


# simulate wx -------------------------------------------------------------
# long simulation for one site that had warnings

# markov files
coeffs <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                  imputation_type = "mean")
head(coeffs[[1]])


x_empty <- list(new("swWeatherData")) # empty weather object

# generate weather just based on the input coeffs
# running for a long time to increase chances of getting zero ppt growing
# season
wout1 <- dbW_generateWeather(x_empty, years = 2000:3000,
                             wgen_coeffs = coeffs, seed = 123)

wout1 <- wout1 %>% 
  dbW_weatherData_to_dataframe() %>% 
  as.data.frame() 
# growing season ----------------------------------------------------------

# months that are the growing season
gsmonths <- wdata_df %>% 
  mutate(month = doy2month(Year, DOY),
         # growing season is based on mean temp (i assume this is how its 
         # calculated)
         Tmean = (Tmax_C + Tmin_C)/2) %>% 
  group_by(month) %>% 
  summarize(Tmean = mean(Tmean)) %>% 
  # cuttoff for growing season
  filter(Tmean >= 4.4) %>% 
  pull(month)

# growing season ppt
no_gs_ppt <- wout1 %>% 
  mutate(month = doy2month(Year, DOY)) %>% 
  # growing season only
  filter(month %in% gsmonths) %>% 
  group_by(Year) %>% 
  summarise(gsppt = sum(PPT_cm, na.rm = TRUE),
            # determine weather gs = 0 driven by NAs
            gspptna = sum(PPT_cm, na.rm = FALSE)) %>% 
  filter(gsppt == 0)

wout1 %>% 
  mutate(month = doy2month(Year, DOY)) %>% 
  filter(Year %in% no_gs_ppt$Year,
         month %in% gsmonths)



# coeffs during the growing season
gs_coeffs <- coeffs[[2]] %>% 
  # just putting in random year to make doy2month fun work
  mutate(month = doy2month(year = 2010, DOY)) %>% 
  filter(month %in% gsmonths)

# probability of dry day given previous day was dry
p_D_D <- 1 - gs_coeffs$p_W_D
prod(p_D_D)


# prob of no growing season ppt for each site -------------------------------

sites <- 1:200

# probability of no growing season ppt for each site
prob_no_ppt <- map_dbl(sites, function(site) {
  wdata <- dbW_getWeatherData(Site_id = site)
  wdata_df <- wdata %>% 
    dbW_weatherData_to_dataframe() %>% 
    as.data.frame()
  coeffs <- dbW_estimate_WGen_coefs(wdata_df, propagate_NAs = FALSE,
                                    imputation_type = "mean")
  # months that are the growing season
  gsmonths <- wdata_df %>% 
    mutate(month = doy2month(Year, DOY),
           # growing season is based on mean temp (i assume this is how its 
           # calculated)
           Tmean = (Tmax_C + Tmin_C)/2) %>% 
    group_by(month) %>% 
    summarize(Tmean = mean(Tmean)) %>% 
    # cuttoff for growing season
    filter(Tmean >= 4.4) %>% 
    pull(month)
  
  # coeffs during the growing season
  gs_coeffs <- coeffs[[2]] %>% 
    # just putting in random year to make doy2month fun work
    mutate(month = doy2month(year = 2010, DOY)) %>% 
    filter(month %in% gsmonths)
  
  # probability of dry day given previous day was dry
  p_D_D <- 1 - gs_coeffs$p_W_D
  # probability of each day being dry
  # I think this calculation is correct, as long as you assume the first
  # day of the season was dry
  out <- prod(p_D_D)
  out
})

hist(prob_no_ppt)

prob_no_ppt_df <- tibble(prob_no_ppt = prob_no_ppt,
                         site = sites) %>% 
  mutate(ppt_warn = ifelse(site %in% bad_sites, "warning", "no warning"))

# mean for each group
mean_probs <- prob_no_ppt_df %>% 
  group_by(ppt_warn) %>% 
  summarize(prob_no_ppt = mean(prob_no_ppt))

# expected mean across all sites
exp_all_prob <- prob_no_ppt_df$prob_no_ppt %>% 
  mean()
# observed probabilities --------------------------------------------------

# number of zero growing season ppt warnings
num_warnings <- str_detect(logfile, "Zero growing") %>% 
  sum()

# number of years simulations run for sites with warnings
niter <- 200
nyears <- 150
num_warn_yrs <- niter*nyears*length(bad_sites)

# this probability is likely biased because we're only calculating
# the probability for sites observed to have a warning
obs_warn_prob <- tibble(prob_no_ppt = c(num_warnings/num_warn_yrs, 0),
                        ppt_warn = c("warning", "no warning"))

# confidence interval
binom.test(x = num_warnings, n = num_warn_yrs)

# across all sites probabilities
# unsure why this CI doesn't come close to the expected value
# its possible that I'm miscalculating growing season?
num_all_yrs <- niter*nyears*length(sites)
binom.test(x = num_warnings, n = num_all_yrs, p = exp_all_prob)

num_all_yrs*exp_all_prob
num_warnings

# range of expected number of warnings (99% prediction interval)
qbinom(c(0.005, 0.5, 0.995), size = num_all_yrs, prob = exp_all_prob)
# figures -----------------------------------------------------------------

pdf("figures/probabilities_no-gs-ppt.pdf")
ggplot(prob_no_ppt_df, aes(prob_no_ppt)) +
  geom_histogram() +
  geom_vline(data = mean_probs, aes(xintercept = prob_no_ppt,
                                    linetype = "expected probability",
                                    color = "expected probability")) +
  geom_vline(data = obs_warn_prob, aes(xintercept = prob_no_ppt,
                                    linetype = "observed probability",
                                    color = "observed probability")) +
  facet_wrap(~ppt_warn, scales = "free_y", ncol = 1) +
  theme_classic() +
  labs(color  = "mean probability across sites", 
       linetype = "mean probability across sites",
       x = "probability of no ppt during growing season",
       subtitle = "Distributions of the calculated probabilities of no ppt during
       the growing season. Sites grouped by whether a no ppt warning thrown
       in simulation. Observed probability calculated as number of warnings
       divided by total number of years simulations run for")
dev.off()
  
# disconnect db -----------------------------------------------------------

rSOILWAT2::dbW_disconnectConnection()
