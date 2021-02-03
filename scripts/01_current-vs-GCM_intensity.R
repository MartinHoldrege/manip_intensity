# Martin Holdrege

# script started 2/3/21

# purpose of this script is to compare the precipitation intensity
# between current weather and future (gcm) weather in the weather
# database. ie this is to see if the 'ambient' intensities are the same. 

# dependencies ------------------------------------------------------------

library(tidyverse)
library(DBI)
library(precipr) # devtools::install_github("MartinHoldrege/precipr")
source("scripts/functions.R")
theme_set(theme_classic())

# connect to database -----------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)


# extract weather data ----------------------------------------------------

# model/rcp combination
# these are the GCMs for RCP 8.5 given in Main.R of rSFSTEP2
climate.conditions <- c("RCP85.CanESM2", 
                        "RCP85.CESM1-CAM5", "RCP85.CSIRO-Mk3-6-0", 
                        "RCP85.FGOALS-g2","RCP85.FGOALS-s2","RCP85.GISS-E2-R",
                        "RCP85.HadGEM2-CC","RCP85.HadGEM2-ES",
                        "RCP85.inmcm4","RCP85.IPSL-CM5A-MR","RCP85.MIROC5",
                        "RCP85.MIROC-ESM","RCP85.MRI-CGCM3")
                       
climate.conditions <- paste0("hybrid-delta-3mod.d90yrs.", climate.conditions)
climate.conditions <- c("Current", climate.conditions)

# Diverse set of representative sites from KP
sites <- c(5, 15, 74, 76, 102, 103, 119, 124, 141, 156, 162, 172, 177, 184)

sw_weatherList <- extract_data2(site_to_extract = sites, climate.conditions)

names(sw_weatherList) <- sites

rSOILWAT2::dbW_disconnectConnection()


# combine into 1 df -------------------------------------------------------

# daily weather data from the sites/scenarios
dly1 <- map(sw_weatherList, combine_scenarios, 
            climate.conditions = climate.conditions) %>% 
  bind_rows(.id = "site") %>% 
  as_tibble() %>% 
  mutate(site = as.numeric(site)) %>% 
  rename(PPT = PPT_cm)

# calculate intensity -----------------------------------------------------

yrly1 <- dly1 %>% 
  select(-Tmax_C, -Tmin_C) %>% 
  # yearly summaries
  group_by(site, scenario, year) %>% 
  summarise(# number of days with precip
    n_days = sum(PPT > 0),
    # annual precip
    ap = sum(PPT),
    # mean precip on days w/ precip
    mean_PPT = ap/n_days,
    med_PPT = median(PPT[PPT > 0]),
    n_events = n_events(PPT, min_length = 1),
    max_event_length = max_event_length(PPT),
    mean_event_size = mean_event_size(PPT),
    n_small = sum(PPT > 0 & PPT < 0.1),
    Rx1day = max(PPT)) %>%  # max event per year
  mutate(RCP = str_extract(scenario, "RCP\\d{2}"),
         RCP = ifelse(scenario == "Current", "Current", RCP))

# median values across gcm's in a given year
yrly_rcp1 <- yrly1 %>% 
  ungroup() %>% 
  select(-scenario) %>% 
  group_by(site, year, RCP) %>% 
  summarise_all(.funs = median)
  

# figures -----------------------------------------------------------------

var_names <- c(n_days = "number of wet days",
          ap = "annual precipitation (cm)",
          mean_PPT = "mean PPT on days with PPT",
          med_PPT = "median PPT on days with PPT",
          n_events = "number of PPT events",
          max_event_length = "max event length (days)",
          mean_event_size = "mean size of PPT event (cm)",
          n_small = "number of days with little PPT (0<x<0.1 cm)",
          Rx1day = "maximum daily PPT (cm)")

vars <- names(var_names)

pdf("figures/current-vs-GCM_intensity.pdf")

map2(vars, var_names, function(var, name) {
  df <- yrly1
  # reordering factors so overplotting is better.

  df$scenario <- factor(df$scenario) %>% 
    forcats::fct_rev()
  df$RCP <- factor(df$RCP) %>% 
    forcats::fct_rev()
  
  ggplot(df, aes(x = .data[[var]], color = RCP,
                    group = scenario)) +
    geom_density() +
    labs(x = name,
         title = name,
         subtitle = "By Site. Only 'ambient' intensity shown (i.e. original weather database data)",
         caption = "Distributions of yearly summary staistics.\nIndividuals lines represent separate lines for each GCM") +
    facet_wrap(~site) +
    scale_color_manual(values = c("RCP85" = "red", "Current" = "blue"))
})

dev.off()
