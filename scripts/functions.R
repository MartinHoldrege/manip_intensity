# martin holdrege

# script started 12/14/20

# for tests

library(dplyr)


# add_year ----------------------------------------------------------------


#' extract df and add year
#'
#' @param x object of class swWeatherData
#'
#' @return df with value of year slot added to new column 'year'
#' @export
#'
#' @examples
add_yr <- function(x) {
  stopifnot(class(x) == "swWeatherData")
  
   year <- x@year
   df <- as.data.frame(x@data)
   df$year <- year
   df
}


# combine_years -----------------------------------------------------------

#' combine weather across years
#'
#' @param x list from weather data base, each element is weather data for one year
#'
#' @return
#' @export
#'
#' @examples
#' combine_yrs(x)
combine_yrs <- function(x) {
  stopifnot(class(x) == "list")
  
  # combine separate years into one data frame 
  out <- map_dfr(x, add_yr)
  out
}


# combine scenarios -------------------------------------------------------


#' combine climate scenarios into one dataframe for a site
#'
#' @param x list, level two of the sw_weatherList
#' @param climate.conditions Character vector with names of climate conditions
#'
#' @return dataframe
#' @export
#'
#' @examples
combine_scenarios <- function(x, climate.conditions) {
  stopifnot(class(x) == "list",
            length(x) == length(climate.conditions),
            is.character(climate.conditions))
  
  names(x) <- climate.conditions
  # for each scenario combine years
  yrs_combined <- map(x, combine_yrs)
  out <- bind_rows(yrs_combined, .id = "scenario")
  out
}

# extract weather db  -----------------------------------------------------
# rSFSTEP2 code that i've modeified slightly

#Functions to access respective data

#Function to extract data for a specific site
.local2 <- function(sid, climate.conditions){
  i_sw_weatherList <- list()
  for(k in seq_along(climate.conditions))
    i_sw_weatherList[[k]] <- rSOILWAT2::dbW_getWeatherData(
      Site_id=sid, Scenario=climate.conditions[k]
      )
  return(i_sw_weatherList)
  
}

#Function to extract respective data for all sites and save it as a list
extract_data2<-function(site_to_extract=NULL, climate.conditions)
{
  sw_weatherList <- NULL
  for(i in seq_along(site_to_extract)){
    sw_weatherList[[i]] <- try(.local2(sid=site_to_extract[i],
                                      climate.conditions = climate.conditions), 
                               silent=TRUE)
  }
  return (sw_weatherList)
}


# prev_wet ------------------------------------------------------------


#' Was previous day wet
#'
#' @param x numeric vector
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- c(0, 1, 2, 2, 0, 0, 1)
#' is_prev_wet(x)
is_prev_wet <- function(x) {
  stopifnot(is.numeric(x))
  n <- length(x)
  # previous value (ie. i-1)
  prev <- c(NA, x[1:(n-1)])
  # is previous value >0
  out <- prev > 0
  out
}


# figure functions --------------------------------------------------------

# create two facet wrapped versions of the figure
wrap_site <- function(x) {
  fig1 <-  x + facet_wrap(~site) + 
    labs(subtitle = "by site")
  fig2 <- x + facet_wrap(~site, scales = "free") + 
    labs(subtitle = "by site, scales differ")
  
  list(fig1, fig2)
}


# doy2month ---------------------------------------------------------------

doy2month <- function(year, doy) {
  # date is 0 based index
  date <- as.Date(doy - 1, origin = paste0(year, "-01-01"))
  month <- lubridate::month(date)
  month
}
