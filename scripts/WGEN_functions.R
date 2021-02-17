# Martin Holdrege

# script started 2/16/21

# functions to code a richardson and wright (1984) weather generator

library(dplyr)
data <- readr::read_csv("data-raw/wx_ambient_site-5.csv")
x <- data$PPT_cm[data$month == 1]


# precipitation functions -------------------------------------------------


#' Title
#'
#' @param x 
#' @param return 
#'
#' @return
#' @export
#'
#' @examples
#' P_W_X(x)
P_W_X <- function(x, return = "both") {
  # probability of wet day following wet day
  stopifnot(is.numeric(x),
            all(!is.na(x)),
            return %in% c("both", "P_W_W", "P_W_D"))
  
  # could change definition of wet day to some threshold but using 0
  # because that is what stepwat2 was using
  threshold <- 0
  is_wet <- x > threshold
  
  is_prev_wet <- is_wet[1:(length(x)-1)] # is previous day wet
  x2 <- x[-1] # can't look at day before the first day
  
  # wet days that were wet on the preceding day
  W_W <- sum(x2[is_prev_wet] > 0)
  P_W_W <- W_W/sum(is_prev_wet) # probability of wet day given previous day wet
  
  # dry day were wet on preceding day
  W_D <- sum(x2[!is_prev_wet] > 0)
  
  # prob of wet day given previous day dry
  P_W_D <- W_D/sum(!is_prev_wet)
  
  out <- c("P_W_W" = P_W_W,
           "P_W_D" = P_W_D)
  
  if(return == "both") {
    return(out)
  } else if (return == "P_W_D") {
    return(out["P_W_D"])
  } else if (return == "P_W_W") {
    return(out["P_W_W"])
  } else {
    stop("incorrect return string given")
  }
}



#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
monthly_ppt_params <- function(data) {
  threshold <- 0 # threshold for wet day
  # minimum required columns
  stopifnot(c("PPT_cm", "month") %in% names(data))
  
  if ("site" %in% names(data) & length(unique(data$site)) != 1) {
    stop("function only meant to handle one site worth of data") 
  }
  
  out <- data %>% 
    ungroup() %>% 
    group_by(.data$month) %>% 
    summarize(P_W_W = P_W_X(.data$PPT_cm, return = "P_W_W"), # prob wet, prev wet
              # prob wet given previous dry
              P_W_D = P_W_X(.data$PPT_cm, return = "P_W_D"),
              PPT_mean = mean(.data$PPT_cm[.data$PPT_cm > threshold]),
              PPT_SD = sd(.data$PPT_cm[.data$PPT_cm > threshold]),
              .groups = "drop")
  out
}


#' Title
#'
#' @param params 
#' @param start_date 
#' @param end_date 
#'
#' @return
#' @export
#'
#' @examples
#' params <- monthly_ppt_params(data)
#' df <- markov_chain(params)
#' df
#' sum(df$is_wet)/nrow(df) # percent of days that are wet
#' sum(data$PPT_cm > 0)/nrow(data) # comparison to original data 
markov_chain <- function(params, 
                         start_date = "1980-01-01", 
                         end_date = "2010-12-31") {
  
  stopifnot(is.data.frame(params),
            c("month", "P_W_W", "P_W_D") %in% names(params))
  
  date_seq <- seq(from = lubridate::ymd(start_date), 
                  to = lubridate::ymd(end_date), by = "1 day")
  
  stopifnot(length(date_seq) > 2)
  
  # data frame with sequential dates
  df1 <- dplyr::tibble(date = date_seq) %>% 
    mutate(year = lubridate::year(.data$date),
           month = lubridate::month(.data$date),
           day = lubridate::mday(.data$date),
           is_wet = NA)
  
  # markov chain
  
  # for starters making first day dry (could make stochastic)
  # shouldn't matter if simulated chain is long enough
  df1$is_wet[1] <- FALSE
  for (i in 2:length(df1$date)) {
    wet_prob <- if (df1$is_wet[(i-1)]) {
      # prob if prev day wet
      params$P_W_W[params$month == df1$month[i]]
    } else {
      # prob if prev day dry
      params$P_W_D[params$month == df1$month[i]]
    }
    
    # stochastically decide if day is wet or not
    df1$is_wet[i] <- runif(1) < wet_prob
  }
  
  return(df1)
}

#' Title
#'
#' @param df 
#' @param params 
#'
#' @return
#' @export
#'
#' @examples
#' params <- monthly_ppt_params(data)
#' df <- markov_chain(params)
#' df2 <- generate_events(df, params)
#' # calculate yearly ppt (original data is 31.6 cm)
#' df2 %>% group_by(year) %>% 
#' summarize(PPT_cm = sum(PPT_cm)) %>% 
#' summarize(PPT_cm = mean(PPT_cm))
generate_events <- function(df, params) {
  # df is output from markov_chain
  
  # make sure months are ordered
  stopifnot(1:12==params$month)
  
  # parameters of gamma distribution
  params$alpha <- params$PPT_mean^2/params$PPT_SD^2
  params$beta <-  params$PPT_mean/params$PPT_SD^2
  df$PPT_cm <- NA_real_
  df$PPT_cm[!df$is_wet] <- 0
  
  # random draws from gamma distribution
  df$PPT_cm[df$is_wet] <- rgamma(sum(df$is_wet),
         # params for each month
         shape = params$alpha[df$month[df$is_wet]],
         rate = params$beta[df$month[df$is_wet]]
         )
  df$is_wet <- NULL
  return(df)
}


#' Title
#'
#' @param params 
#' @param mean_mult 
#' @param sd_mult 
#'
#' @return
#' @export
#'
#' @examples
#' mean_mult <- 1.5
#' sd_mult <- 2
#' params <- monthly_ppt_params(data)
#' params2 <- adjust_params(params, mean_mult, sd_mult)
#' df <- markov_chain(params2)
#' df2 <- generate_events(df, params2)
#' # make histograms
#' breaks <- seq(from = 0, to =10, by = 0.25) 
#' hist(df2$PPT_cm[df2$PPT_cm > 0], breaks = breaks)
#' hist(data$PPT_cm[data$PPT_cm > 0],breaks = breaks)
adjust_params <- function(params, mean_mult, sd_mult) {
  
  stopifnot(is.data.frame(params),
            c("P_W_W", "P_W_D", "PPT_mean", "PPT_SD") %in% names(params)
            )
  
  # probability multiplier (to keep totals the same)
  P_mult <- 1/mean_mult
  
  # adjust mean, sd and probabilities, goal is that totals remain the same
  params2 <- params %>% 
    mutate(P_W_W = .data$P_W_W*P_mult,
           P_W_D = .data$P_W_D*P_mult,
           PPT_mean = .data$PPT_mean*mean_mult,
           PPT_SD = .data$PPT_SD*sd_mult)
  
  params2
}


# misc functions ----------------------------------------------------------

week52 <- function(date) {
  # make last week of year 'long' so don't have short partial week
  # that can't extimate parameters for
  week <- lubridate::week(date)
  week <- ifelse(week ==53, 52, week)
  week
}
# temperature functions ---------------------------------------------------


#' Title
#'
#' @param Tmax 
#' @param Tmin 
#'
#' @return
#' @export
#'
#' @examples
#' df <- filter(data, month == 1 & day %in% 1:7)
#' year <- df$year
#' Tmax <- df$Tmax_C
#' Tmin <- df$Tmin_C
calc_M0 <- function(Tmax, Tmin) {
 
  # M0 matrix;  eq 8 in Richardson and Wright 1984, except cov used (Wilks, eq 11.21)
  r0_max_min <- cor(Tmax, Tmin) # correlation (not lagged)
  out <- matrix(c(1, r0_max_min, r0_max_min, 1),
                ncol = 2,
                byrow = TRUE)
  out
}

calc_M1 <- function(Tmax, Tmin, year) {
  stopifnot(length(Tmax) == length(Tmin),
            length(year) == length(Tmax))
  # eq 11.22b in wilks 2011
  
  
  # year is included so lag 1 one works (not finding covariance)
  # with lagged value of the previous year
  
  # since I am calculating the lagged covariance by 'hand' (not using acf())
  # the values are be slighly (1% for one data set I check) 
  # different (b/ of how means calculated), but I'm
  # doing that to deal with the issue of lagging across years 
  
  # vectors for lagging
  t <- 1:(length(year)-1)
  
  # t + 1 (i.e. the lagged index)
  t_p_1 <- t + 1
  
  # when lagged values refer to the same year 
  # these are the endices to take the covariance at
  same_yr <- year[t] == year[t_p_1] 
  
  
  t_p_1 <- t_p_1[same_yr] # only use values when years match
  t <- t[same_yr]
  
  r1_max <- cor(Tmax[t], Tmax[t_p_1])
  r1_min <- cor(Tmin[t], Tmin[t_p_1])
  

  # cross correlations (lagged)
  
  # lagged Tmin
  r1_max_min <-  cor(Tmax[t], Tmin[t_p_1])
  
  # lagged Tmax
  r1_min_max <-  cor(Tmin[t], Tmax[t_p_1])

    out <- matrix(c(r1_max, r1_min_max, r1_max_min, r1_min),
                ncol = 2,
                byrow = TRUE)
  out
}

#' Title
#'
#' @param M0 
#' @param M1 
#'
#' @return
#' @export
#'
#' @examples
#' df <- filter(data, month == 2 & day %in% 1:7)
#' Tmax <- df$Tmax_C
#' Tmin <- df$Tmin_C
#' year <- df$year
#' M0 <- calc_M0(Tmax, Tmin)
#' M1 <- calc_M1(Tmax, Tmin, year)
#' A <- calc_A(M0, M1)
calc_A <- function(M0, M1) {
  # eq 6 in Richardson and Wright 1984 
  
  A <- M1 %*% solve(M0)
  A
}

calc_B <- function(M0, M1) {
  
  # eq 7 in Richardson and Wright 1984 
  BBT <- M0 - M1 %*% solve(M0) %*% t(M1)
  
  # taking square root
  # switched back to correlation matrices because the 
  # covariance matrix had imaginary numbers in the square root
  B <- expm::sqrtm(BBT)
  B
}

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
#' wk_list <- temp_wk_list(data)
#' # plotting min/max temps
#' Tmax_mean <- purrr::map_dbl(wk_list, function(x) x$means[1])
#' plot(Tmax_mean, type = "l", col = "red")
#' lines(purrr::map_dbl(wk_list, function(x) x$means[2]), col = "blue")
temp_wk_list <- function(data) {
  
  stopifnot(
    is.data.frame(data),
    c("date", "Tmax_C", "Tmin_C", "PPT_cm") %in% names(data)
  )
  
  threshold <- 0
  # create list with weekly means, and A and B matrices
  df1 <- data
  df1$week <- week52(df1$date)
  
  # for calculating temp on wet and dry days
  df1$is_wet <- df1$PPT_cm > threshold
  
  wk_list <- split(df1, df1$week)
  
  # making sure list is correctly ordered
  stopifnot(names(wk_list) == as.character(1:52))
  
  # M0 matrices (covariance version of what is in) Richardson wright 1984
  M0_list <- purrr::map(wk_list, function(df) {
    calc_M0(Tmax = df$Tmax_C, Tmin = df$Tmin_C)
  })
  
  M1_list <- purrr::map(wk_list, function(df) {
    calc_M1(Tmax = df$Tmax_C, Tmin = df$Tmin_C, year = df$year)
  })
  
  A_list <- purrr::map2(M0_list, M1_list, calc_A)
  B_list <- purrr::map2(M0_list, M1_list, calc_B)
  
  # eq 12 in Richardson and Wright 1984 calls for seasonal sd, (here
  # i'm using weekly sd, but across years)
  sd_list <- purrr::map(wk_list, function(df) {
    c(Tmax_sd = sd(df$Tmax_C), Tmin_sd = sd(df$Tmin_C))
  })
  
  # means for the week
  mean_list <- purrr::map(wk_list, function(df) {
      c("Tmax_mean" = mean(df$Tmax_C), "Tmin_mean" = mean(df$Tmin_C))
  })
  
  # temp on wet and dry days
  wet_dry_temp_list <- purrr::map(wk_list, function(df) {
    c("Tmax_dry" = mean(df$Tmax_C[!df$is_wet]), 
      "Tmax_wet" = mean(df$Tmax_C[df$is_wet]),
      "Tmin_dry" = mean(df$Tmin_C[!df$is_wet]), 
      "Tmin_wet" = mean(df$Tmin_C[df$is_wet]))
  })

  # combined elements needed for prediction (eq. 11.21 in Wilks, 2011)
  out <- purrr::pmap(list(mean_list, sd_list, wet_dry_temp_list,  A_list, B_list), 
                     function(mean, sd, wet_dry_temp, A, B) {
    list(means = mean, sd = sd, wet_dry_temp = wet_dry_temp, A = A, B = B)
  })
  out
}



#' Title
#'
#' @param wk_list 
#' @param start_date 
#' @param end_date 
#'
#' @return
#' @export
#'
#' @examples
#' wk_list <- temp_wk_list(data)
#' test <- generate_temp(wk_list, "1980-01-01", "1982-12-31")
#' plot(test$Tmax_C, col = "red", type = "l")
#' lines(test$Tmin_C, col = "blue")
#' hist((test$Tmax_C-test$Tmin_C))
#' # testing
#' test <- generate_temp(wk_list, "1980-01-01", "2079-12-31") # 30 sec to run
#' summarize(test, Tmax_C = mean(Tmax_C), Tmin_C = mean(Tmin_C))
#' # compare to original data
#' summarize(data, Tmax_C = mean(Tmax_C), Tmin_C = mean(Tmin_C))
generate_temp <- function(wk_list, 
                          start_date = "1980-01-01", 
                          end_date = "2010-12-31") {

  # setting up empty dataframe
  df1 <- tibble(date = seq(from = lubridate::ymd(start_date),
                           to = lubridate::ymd(end_date),
                           by = "1 day")) %>% 
    mutate(week = week52(.data$date),
           Tmax_C = NA_real_,
           Tmin_C = NA_real_)
  
  # first day in chain just using mean, to 'initialize'
  df1$Tmax_C[1] <- wk_list[[as.character(df1$week[1])]]$means["Tmax_mean"]
  df1$Tmin_C[1] <- wk_list[[as.character(df1$week[1])]]$means["Tmin_mean"]
  
  # generating Tmax/tmin as per eq 11.21 in wilks, 2011
  for (t in 1:(nrow(df1) -1)) {
    
    # mean vector for the given week (note just using sample mean, not some
    # smoothed value, to simplify things)
    mu <- wk_list[[as.character(df1$week[t])]]$means
    A <- wk_list[[as.character(df1$week[t])]]$A
    B <- wk_list[[as.character(df1$week[t])]]$B
    
    sd <- as.matrix(wk_list[[as.character(df1$week[t])]]$sd, ncol = 1)
    
    # error at time t +1
    et1 <- rnorm(2, mean = 0, sd = 1)
    et1 <- as.matrix(et1, ncol = 1)
    xt <- c(df1$Tmax_C[t], df1$Tmin_C[t])
    rt <- (xt  - mu)/sd # residuals at time t
    rt <- as.matrix(rt, ncol = 1)

    # residual vector (Tmax, Tmin), at time t + 1 (eq 3 Richardson and wilks)
    rt1 <- A%*%rt + B%*%et1

    # eq 12 in R & W 1984). Calculate observed value 
    xt1 <- rt1*sd + as.matrix(mu, ncol = 1)
    
    df1$Tmax_C[(t+1)] <- xt1[1,]
    df1$Tmin_C[(t+1)] <- xt1[2,]
  }
  df1
  
}


