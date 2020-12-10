# Martin Holdrege

# Script Started 12/9/20

# First stab at manipulating precipitation intensity from the wx database.

# dependencies ------------------------------------------------------------

library(tidyverse)
library(DBI)


# connect to database -----------------------------------------------------

db_path <- "../dbWeather/dbWeatherData_STEPWAT2_200sites.sqlite3"

db <- dbConnect(RSQLite::SQLite(), db_path)

rSOILWAT2::dbW_setConnection(db_path, check_version = TRUE)

# examine db --------------------------------------------------------------

dbListTables(db)
dbGetQuery(db, 'SELECT * FROM Scenarios')
dbGetQuery(db, 'SELECT * FROM wd_all LIMIT 10')


# extract current scenarios -----------------------------------------------

# Diverse set of representative sites from KP
sites <- c(5, 15, 74, 76, 102, 103, 119, 124, 141, 156, 162, 172, 177, 184)

climate.conditions <- "Current"

# code below is from WeatherQuery.R

#Weather query script to extract respective weather data for all scenarios from a pre-generated weather database into a list (sw_weatherList)


#Connecting to the database

#########################################################################
#Functions to access respective data

#Function to extract data for a specific site
.local <- function(sid){
  i_sw_weatherList <- list()
  for(k in seq_along(climate.conditions))
    i_sw_weatherList[[k]] <- rSOILWAT2::dbW_getWeatherData(Site_id=sid, Scenario=climate.conditions[k])
  return(i_sw_weatherList)
  
}

#Function to extract respective data for all sites and save it as a list
extract_data<-function(site_to_extract=NULL)
{
  sw_weatherList <- NULL
  for(i in seq_along(site_to_extract)){
    sw_weatherList[[i]] <- try(.local(sid=site_to_extract[i]), silent=TRUE)
  }
  return (sw_weatherList)
}

sw_weatherList<-extract_data(site_to_extract = sites)
rSOILWAT2::dbW_disconnectConnection()


# run example for 1 site/yr -----------------------------------------------

data <- sw_weatherList[[1]][[1]][["1980"]]@data # this is a matrix
x <- df[ , "PPT_cm"] # vector of precip data for one year
hist(x[x>0], breaks = 20)
sum(x>0)


# function development, later put in separate script
# Continue here
increase_intensity <- function(x, rm_from = 1, add_to = 1) {
  # rm_from and add_to not implemented yet
  x
  # locations in vector on which it rained
  precip_loc <- which(x > 0)
  n <- length(precip_locs)
  
  rm_days <- seq(from = 1, to = length(precip_days), by = 2)
  add_days <- 1:
}

dbDisconnect(db)
