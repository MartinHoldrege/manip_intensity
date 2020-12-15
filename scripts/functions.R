# martin holdrege

# script started 12/14/20


# increase intensity ------------------------------------------------------
# function development, later put in separate script or package
# for now this function is just for doubling intensity 



#' Increase intensity
#'
#' @param x numeric vector
#'
#' @return
#' @export
#'
#' @examples
increase_intensity <- function(x) {
  # takes odd days of precip, adds them to even days
  
  # locations in vector on which it rained
  precip_loc <- which(x > 0)
  
  # num rain days
  n <- length(precip_loc)
  
  # days to remove rain from [ie locations in precip_loc vector]
  rm_days <- seq(from = 1, to = (n - 1), by = 2)
  add_days <- 1:n
  add_days <- add_days[!add_days %in% rm_days]
  
  # positions in precip_loc vector to add precip too. 
  add_days <- if (length(add_days) == length(rm_days)) {
    add_days
  } else if ((length(add_days) - 1) == length(rm_days)) {
    add_days[-length(add_days)] # removing last day b/ odd number of days
  } else {
    stop("vector of days removing precip from is wrong length\n")
  }
  
  # positions in original vector to add/remove from
  add_loc <- precip_loc[add_days]
  rm_loc <- precip_loc[rm_days]
  
  x_new <- x
  x_new[add_loc] <- x_new[add_loc] + x_new[rm_loc]
  
  # precip added to other days now goes to 0
  x_new[rm_loc] <- 0
  x_new
}

# test (should be all true)
# x2 <- increase_intensity(x)
# sum(x2) == sum(x)
# sum(x2 > 0) == ceiling(sum(x > 0) / 2)



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


