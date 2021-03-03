# Martin Holdrege

# Script created 2/24/21

# Purpose is testing the weather generator in the precipr package


# dependencies ------------------------------------------------------------

library(tidyverse)
library(precipr)
theme_set(theme_classic())
# generate weather -------------------------------------------------------- 

# generate ambient weather
gen_amb <- WGEN_main(precipr::wx_data,
                     mean_mult = 1,
                     sd_mult = 1,
                     start_date = "1980-01-01",
                     end_date = "2279-12-31")

gen_2x <- WGEN_main(precipr::wx_data,
                     mean_mult = 2,
                     sd_mult = 4,
                     start_date = "1980-01-01",
                     end_date = "2279-12-31")


# combine dfs -------------------------------------------------------------

gen_amb <- gen_amb %>% 
  mutate(intensity = "ambient WGEN")

gen_2x <- gen_2x %>% 
  mutate(intensity = "2x WGEN")


df <- wx_data %>% 
  select(-site) %>% 
  mutate(intensity = "original") %>% 
  bind_rows(gen_amb) %>% 
  bind_rows(gen_2x) %>% 
  select(-week) %>% 
  rename(PPT = PPT_cm)

saveRDS(df, "data-processed/site5_test_sim.RDS")
df <- readRDS("data-processed/site5_test_sim.RDS")


# annual summaries --------------------------------------------------------

yrly1 <- df %>% 
  # yearly summaries
  group_by(intensity, year) %>% 
  summarise(# number of days with precip
    Tmax_mean = mean(Tmax_C),
    Tmin_mean = mean(Tmin_C),
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
    Rx1day = max(PPT),
    .groups = "drop") 


var_names <- c(Tmax_mean = "mean daily max temp",
               Tmin_mean = "mean daily in temp",
               n_days = "number of wet days",
               ap = "annual precipitation (cm)",
               mean_PPT = "mean PPT on days with PPT",
               med_PPT = "median PPT on days with PPT",
               n_events = "number of PPT events",
               max_event_length = "max event length (days)",
               mean_event_size = "mean size of PPT event (cm)",
               n_small = "number of days with little PPT (0<x<0.1 cm)",
               Rx1day = "maximum daily PPT (cm)")

vars <- names(var_names)

base <- function() {
  list( scale_color_manual(values = c("original" = "black", 
                                      "ambient WGEN" = "blue",
                                      "2x WGEN" = "red")))
}

pdf("figures/original-vs-WGEN_distributions.pdf")

# daily ppt distribution
df %>% 
  filter(PPT > 0) %>% 
  ggplot(aes(PPT, color = intensity)) +
  geom_density() +
  labs(x = "daily PPT (cm)",
       title = "Daily PPT on days with PPT > 0",
       caption = "precipr WGEN used. Data from site 5") +
  base()

# distributions of yearly summary stats
map2(vars, var_names, function(var, name) {
  ggplot(yrly1, aes(x = .data[[var]], color = intensity)) +
    geom_density() +
    labs(x = name,
         title = name,
         subtitle = "By Site. Only 'ambient' intensity shown (i.e. original weather database data)",
         caption = "Distributions of yearly summary staistics, precipr WGEN used. Data from site 5") +
    base()
})

dev.off()











