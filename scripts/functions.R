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


# create_event_df ---------------------------------------------------------

#' Create dataframe with event numbers
#'
#' @param x numeric vector (daily precip)
#'
#' @return tibble with 2 columns, x (input vector), and event which is the
#' event number corresponding to that value of x
#'
#' @examples
#' create_event_df(c(0.1, 0.1, 0, 0.2)) 
create_event_df <- function(x) {
  stopifnot(is.numeric(x))
  
  # consider changing this threshold if necessary
  is_wet <- x > 0
  
  # length of wet and dry runs
  runs <- rle(is_wet)
  
  num_events <- sum(runs$values) # how many "TRUE"s
  # lengths of consecutive runs of TRUE (ie wet day)
  
  # this if statement not really needed
  event_seq <- if(num_events >= 1) {
    1:num_events
  } else {
    0
  }
  
  # replace logicals with event numbers
  runs2 <- runs
  runs2$values[runs2$values] <- event_seq
  
  x_events <- inverse.rle(runs2) # sequence w/ length x, giving event numbers
  
  df <- tibble(x = x, event = x_events)
  df
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
#' mean_event_size(0) == 0
#' mean_event_size(rep(0.1, 10)) == 1
mean_event_size <- function(x) {

  df <- create_event_df(x)
  
  if (all(df$x == 0)) return(0)
  
  # mean size of events
  mean_size <- df %>% 
    filter(.data$x > 0) %>%  # just days of rain
    group_by(.data$event) %>%
    # summing across days of an event
    summarise(event_size = sum(.data$x), .groups = "drop") %>% 
    pull(event_size) %>% 
    mean()

  mean_size
}


# incr_event_intensity ----------------------------------------------------


#' Increase event size intensity
#'
#' @param x numeric vector (daily precip)
#' @param from positive integer. The number of events to take precip from
#' and add to the following event(s)
#' @param to positive integer. The number of events to add the removed precip
#' to
#'
#' @return numeric vector, same length as x. Precip events are considered
#' 1 or more consecutive days with precip. Precip was removed from one or more (from)
#' events and added to one or more (to) events, and this is repeated for all events 
#' @export
#'
#' @examples
#' y1 <- incr_event_intensity(c(0.1, 0.1, 0, 1))
#' all.equal(y1, c(0, 0, 0, 1.2))
#' y2 <- incr_event_intensity(c(0.2, 0.1, 0.1, 0, 1, 2))
#' all.equal(y2, c(0, 0, 0, 0, 1.2, 2.2))
#' # 2 events added to 1
#' y3 <- incr_event_intensity(c(0.1, 0, 0.2, 0.1, 0, 1),
#'                            from = 2, to = 1)
#' all.equal(y3, c(0, 0, 0, 0, 0, 1.4))
#' y4 <- incr_event_intensity(c(0.3, 0, 0.2, 0.1, 0, 1, 0, 1),
#'                            from = 1, to = 2)
#' all.equal(y4, c(0, 0, 0.3, 0.2, 0, 1.1, 0, 1))
incr_event_intensity <- function(x, from = 1, to = 1) {
  
  stopifnot(from >= 1,
            to >=1)
  
  if(from %% 1 != 0 | to %% 1 != 0) {
    stop("from and to arguments need to be positive integers")
  }
  
  if (from != 1 & to != 1) {
    warning("at least one of from and to arguments should probably be 1")
  }
  
  df1 <- create_event_df(x)
  
  df1$is_event <- df1$x > 0
  
  # calculating length of each event
  df2 <- df1 %>% 
    group_by(.data$event) %>% 
    mutate(event_length = sum(.data$is_event)) %>% 
    ungroup()
  
  # size of individual events
  size_df <- df2 %>% 
    filter(.data$is_event) %>% 
    group_by(.data$event) %>% 
    summarize(event_size = sum(.data$x), .groups = "drop_last")
  
  n <- nrow(size_df) # number of events
  
  cycle_len <- from + to # length of 1 cycle
  n_cycles <- floor(n / (cycle_len)) # number of remove/add combos
  
  # one remove add cycle (TRUE is if remove)
  one_cycle <- c(rep(TRUE, from), rep(FALSE, to))
  
  cycles <- rep(one_cycle, n_cycles) # cycles of remove (T) and add (F)
  rm_event_nums = which(cycles)
  # which events to add to
  add_event_nums = which(!cycles)
  
  events_df <- tibble(
    # these are the event nums that will be manipulated 
    event_num = 1:(n_cycles*cycle_len),
    is_rm = cycles, # remove from this event
    cycle_num = rep(1:n_cycles, each = cycle_len)
  )
  
  df3 <- df2 %>% 
    left_join(events_df, by = c("event" = "event_num")) %>% 
    group_by(cycle_num, is_rm) %>% 
    mutate(
      # number of days that are to be added to in that cycle
      # (equal to event_length when to = 1)
      cycle_add_len = sum(!is_rm))

  # df of how much rain is being removed/added each cycle
  rm_df <- df3 %>% 
    filter(.data$is_rm) %>% 
    group_by(.data$cycle_num, .data$is_rm) %>% 
    summarize(add_size = sum(.data$x), # amount to be added 
              .groups = "drop") %>% 
    mutate(is_rm = FALSE) # reversing so can join with the add events

  df4 <- df3 %>% 
    left_join(rm_df, by = c("cycle_num", "is_rm")) %>% 
    mutate(x = ifelse(!is_rm & !is.na(is_rm),
                      # if add event add equal amount to each day
                      .data$x + .data$add_size/.data$cycle_add_len,
                      x))
  
  # events removed from become 0
  df4$x[df3$is_rm] <- 0
  
  return(df4$x)
}







