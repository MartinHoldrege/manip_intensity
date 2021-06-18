# martin holdrege

# script started April 15, 2021

# example figure of distirubtion of daily event size

# dependencies ------------------------------------------------------------

library(tidyverse)
source("../sitedata_analysis/scripts/fig_params.R")
# read in data ------------------------------------------------------------


ppt_path <- "../sitedata/ppt/markov_ppt_14sites_20210407.csv"
df <- ppt_markov0 <- read_csv(ppt_path)


# figure ------------------------------------------------------------------

# trying to find a representantive site
df %>% 
  filter(intensity == "ambient") %>% 
  group_by(site) %>% 
  summarise(ppt = mean(PPT_cm[PPT_cm > 0])) %>% 
  mutate(mean_diff = ppt - mean(ppt)) %>% 
  arrange(mean_diff)

jpeg("figures/distribution_example.jpg", width = 4, height = 3, units = "in",
     res = 600)
df %>% 
  filter(site == 5, PPT_cm > 0) %>% 
  ggplot(aes(PPT_cm, color = intensity)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(values = cols2) +
  labs(x = "Daily precipitation event size (cm)",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 4))

dev.off()
