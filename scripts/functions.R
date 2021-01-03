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


