# martin holdrege

# script started 12/14/20

# for tests
x <- c(0, 0, 0, 0.1, 0, 2, 0.1, 0, 0, 1, 0, 0.2, 0.2, 0, 1)
library(dplyr)
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
incr_dly_intensity <- function(x) {
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


# wet_lengths -------------------------------------------------------------


#' Lengths of rain events
#'
#' @param x numeric vector
#'
#' @return vector containing the lengths (number of consecutive days) of 
#' rain events
#'
#' @examples
wet_lengths <- function(x) {
  stopifnot(is.numeric(x))
  
  # consider changing this threshold if necessary
  is_wet <- x > 0
  
  # length of wet and dry runs
  runs <- rle(is_wet)
  
  # lengths of consecutive runs of TRUE (ie wet day)
  out <- runs$lengths[runs$values]
  out
}


# n_events ----------------------------------------------------------------

#' Number of rain events
#'
#' @param x numeric vector
#' @param min_length a positive integer. The minimum number of days an event
#' has to be in order to count it. 
#'
#' @return The number of rain events in x. Rain events are defined
#' as a sequence with 1 (default) or more consecutive days with rain
#' @export
#'
#' @examples
#' n_events(rep(c(0, 1), 5)) == 5
#' n_events(rep(c(0, 1), each = 5)) == 1
#' n_events(c(1, 1, 0, 0.5, 0, .1, .2, 0), min_length = 2) == 2
n_events <- function(x, min_length = 1) {
  stopifnot(min_length >= 1,
            is.numeric(x))
  
  wet_length <- wet_lengths(x)

  # number of wet days sequences with at least min_length days long
  n <- sum(wet_length >= min_length)
  n
}


# max_event_length --------------------------------------------------------

#' Max rain event length
#'
#' @param x numeric vector of daily rain events
#'
#' @return the length of the longest sequence of consecutive rainy days
#' @export
#'
#' @examples
#' max_event_length(c(0, 1, 1, 2, 0, 1)) == 3
max_event_length <- function(x) {
  max(wet_lengths(x))
}

# mean_event_size ---------------------------------------------------------

#' Calculate mean event size
#'
#' @param x numeric vector (daily precip)
#'
#' @return mean event size, where an event is the amount of rain on one or more
#' consecutive days of rain
#' @export
#'
#' @examples
#' mean_event_size(c(0.1, 0.1, 0, 0.2)) == 0.2
#' mean_event_size(0)
#' mean_event_size(rep(0.1, 10)) == 1
mean_event_size <- function(x) {
  stopifnot(is.numeric(x))
  
    # consider changing this threshold if necessary
  is_wet <- x > 0
  
  # length of wet and dry runs
  runs <- rle(is_wet)
  
  num_events <- sum(runs$values) # how many "TRUE"s
  # lengths of consecutive runs of TRUE (ie wet day)
  
  if(num_events < 1) return(0) # code below won't work w/ 0 events
  
  event_seq <- 1:num_events
  
  # replace logicals with event numbers
  runs2 <- runs
  runs2$values[runs2$values] <- event_seq
  
  x_events <- inverse.rle(runs2) # sequence w/ length x, giving event numbers
  
  df <- tibble(x = x, event = x_events)

  # mean size of events
  mean_size <- df %>% 
    filter(.data$x > 0) %>%  # just days of rain
    group_by(.data$event) %>%
    # summing across days of an event
    summarise(event_size = sum(.data$x), .groups = "drop") %>% 
    pull(event_size) %>% 
    mean(.)
  
  mean_size
}


# incr_event_intensity ----------------------------------------------------


# for now just a doubling of event size/ cutting frequency in half

# pseudo code
# make data frame with columns of original x, event number, event length

# make separate named(?) vector of event sizes and there location
# us this vector to add additional column of previous event size, ie this
# vector offset by 1 ??

#CONTINUE HERE
# incr_event_intensity <- function(x) {
#   
# }






