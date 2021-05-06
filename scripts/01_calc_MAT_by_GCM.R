# Martin Holdrege


# Script started May 5, 2021

# purpose is to calculate mean temperature for each site under current and
# future climate, based on GCM


# dependencies ------------------------------------------------------------

library(tidyverse)
library(rSOILWAT2)


# paths -------------------------------------------------------------------

database <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"


# setup info to to read db-------------------------------------------------

sites <- 1:200

# code  below pulled from main.r
climate.conditions <- c(
  "RCP45.CanESM2", "RCP45.CESM1-CAM5", "RCP45.CSIRO-Mk3-6-0", "RCP45.FGOALS-g2",
  "RCP45.FGOALS-s2", "RCP45.GISS-E2-R", "RCP45.HadGEM2-CC", "RCP45.HadGEM2-ES",
  "RCP45.inmcm4", "RCP45.IPSL-CM5A-MR", "RCP45.MIROC5", "RCP45.MIROC-ESM",
  "RCP45.MRI-CGCM3", "RCP85.CanESM2", "RCP85.CESM1-CAM5", "RCP85.CSIRO-Mk3-6-0", 
  "RCP85.FGOALS-g2","RCP85.FGOALS-s2","RCP85.GISS-E2-R", "RCP85.HadGEM2-CC",
  "RCP85.HadGEM2-ES","RCP85.inmcm4","RCP85.IPSL-CM5A-MR","RCP85.MIROC5",
  "RCP85.MIROC-ESM","RCP85.MRI-CGCM3")

split <- strsplit(climate.conditions, "\\.")   # Split entries in climate.conditions on the period
GCM <- c(); RCP <- c()       # Create our RCP and GCM vectors

#Setup parameters for the weather aquisition (years, scenarios, timeperiod, GCMs) 
simstartyr <- 1979
endyr <- 2010
climate.ambient <- "Current"

for(i in 1:length(split))    # For every GCM/RCP combination
{
  RCP[i] <- split[[i]][1]    # The first entry is the RCP
  GCM[i] <- split[[i]][2]    # The second entry is the GCM
}

GCM <- unique(GCM)           # Remove any duplicates. This shouldn't happen for GCMs, but just to be safe.
RCP <- unique(RCP)           # Remove any duplicates. This will most likely happen with RCPs.

RCP <- RCP[!grepl("Current", RCP)]   # Since "Current" doesn't contain a period, it will appear in RCP
GCM[is.na(GCM)] <- "Current"         # An NA value in GCM results from "Current" being parsed into RCP

# temp stores all climate conditions except climate.ambient
temp <- climate.conditions[!grepl(climate.ambient, climate.conditions)]

# If we are running future scenarios we need to append a downscaling method

#Difference between start and end year(if you want 2030-2060 use 50; if you want 2070-2100 use 90 below)
deltaFutureToSimStart_yr <- c("d50","d90")

#Downscaling method
downscaling.method <- c("hybrid-delta-3mod")
temp <- paste0(deltaFutureToSimStart_yr, "yrs.", rep(temp, each=length(deltaFutureToSimStart_yr)))	#add (multiple) deltaFutureToSimStart_yr

#Set Years
YEARS<-c("d50yrs","d90yrs")

 
# prepend the downscaling method to all future conditions
temp <- paste0(downscaling.method, ".", rep(temp, each=length(downscaling.method)))

climate.conditions <-  c("Current",temp)

# create empgy df
mat_df <- expand_grid(site = sites,
                      scenario = climate.conditions)

mat_df$Tmax_mean <- NA_real_
mat_df$Tmin_mean <- NA_real_
mat_df$map <- NA_real_

# extract data ------------------------------------------------------------

stopifnot(rSOILWAT2::dbW_setConnection(database, check_version = TRUE))


for (i in 1:nrow(mat_df)) {
  
  # print to keep track of progress
  if((i-1) %% length(climate.conditions) == 0) print (mat_df$site[i])
  
  # extract weather data
  wx <- dbW_getWeatherData(Site_id = mat_df$site[i], 
                           Scenario = mat_df$scenario[i])
  # calculate meant temp and precip
  mat_df$Tmax_mean[i] <- precipr::calc_mat(wx, "Tmax_C")
  mat_df$Tmin_mean[i] <- precipr::calc_mat(wx, "Tmin_C")
  mat_df$map[i] <- precipr::calc_map(wx)
}

rSOILWAT2::dbW_disconnectConnection()


# parse cols----------------------------------------------------------------

mat_df2 <- mat_df %>% 
  mutate(RCP = str_extract(scenario, "RCP\\d{2}|Current"),
         # gcm is last part of string
         GCM = str_extract(scenario, "(?<=RCP\\d{2}.).+$"),
         period = str_extract(scenario, "d\\d{2}yrs|Current"))

# save file ---------------------------------------------------------------

write_csv(mat_df2,
          "data-processed/MAT_by_GCM-site.csv")
