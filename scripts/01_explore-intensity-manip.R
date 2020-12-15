# Martin Holdrege

# Script Started 12/9/20

# First stab at manipulating precipitation intensity from the wx database.

# dependencies ------------------------------------------------------------

library(tidyverse)
library(DBI)
source("scripts/functions.R")
theme_set(theme_classic())

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
x <- data[ , "PPT_cm"] # vector of precip data for one year
hist(x[x>0], breaks = 20)
sum(x>0)





# extract all precip yrs/sites --------------------------------------------

names(sw_weatherList) <- as.character(sites)

# combine 
comb_wx <- map(sw_weatherList, function(x) {
  # just one scenerio so just one list element for each site?
  combine_yrs(x[[1]])
}) %>% 
  bind_rows(.id = "site") %>% 
  mutate(site = as.numeric(site))


# manipulate precip -------------------------------------------------------

comb_wx2 <- comb_wx %>% 
  arrange(site, year, DOY) %>% 
  # for now grouping by year to insure annual precip unchanged
  group_by(site, year) %>% 
  mutate(PPT_manip = increase_intensity(PPT_cm))

manip_lookup <- c("PPT_cm" = "ambient", "PPT_manip" = "incr intensity")
# yearly summaries
yrly_df <- comb_wx2 %>% 
  pivot_longer(matches("^PPT"), names_to = "manip", values_to = "PPT") %>% 
  # yearly summaries
  group_by(site, year, manip) %>% 
  summarise(# number of days with precip
            n_days = sum(PPT > 0),
            # annual precip
            ap = sum(PPT),
            # mean precip on days w/ precip
            mean_PPT = ap/n_days,
            med_PPT = median(PPT[PPT > 0])) %>% 
  mutate(manip = manip_lookup[manip])
  
# distributional figures --------------------------------------------------
wrap_site <- function(x) {
  fig1 <-  x + facet_wrap(~site) + 
    labs(subtitle = "by site")
  fig2 <- x + facet_wrap(~site, scales = "free") + 
    labs(subtitle = "by site, scales differ")

  list(fig1, fig2)
}

pdf("figures/ambient_vs_intensity_distributions_v1.pdf")

g <- ggplot() +
  geom_density(data = comb_wx2[comb_wx2$PPT_manip > 0, ], 
               mapping = aes(x = PPT_manip, color = "incr intensity")) +
  geom_density(data = comb_wx2[comb_wx2$PPT_cm > 0, ], 
               mapping = aes(x = PPT_cm, color = "ambient")) + 
  labs(x = "Daily PPT (cm)",
       title = "Intensity increased by adding odd ppt days to even days") +
  scale_color_manual(values = c("blue", "red"))

wrap_site(g)



# distributions of yrly stats ---------------------------------------------

g2 <- ggplot(yrly_df, aes(color = manip)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(subtitle = "by site",
       caption = "Distributions of yearly summaries")

g3 <- g2 +
  geom_density(aes(n_days)) +
  geom_rug(aes(n_days), sides = "b") +
  labs(title = "number of days per year with precip")

wrap_site(g3) 

g4 <- g2 +
  geom_density(aes(med_PPT)) +
  geom_rug(aes(med_PPT), sides = "b") +
  labs(title = "Median daily PPT on days with PPT")

wrap_site(g4)

g5 <- g2 +
  geom_density(aes(mean_PPT)) +
  geom_rug(aes(mean_PPT), sides = "b") +
  labs(title = "Mean daily PPT on days with PPT")

wrap_site(g5)

g6 <- g2 +
  geom_density(aes(ap)) +
  geom_rug(aes(ap), sides = "b") +
  labs(title = "Annual precip",
       x = "Annual PPT (cm)")

wrap_site(g6)

dev.off()
