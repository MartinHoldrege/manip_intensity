# Martin Holdrege

# script started Feb 15, 2021

# Purpose of this script is to better understand the foreSIGHT package

# note the biggest issue may be that foreSIGHT can only deal with daily mean
# temp not min/max so I don't see a reasonable way to simulate (even unchanged)
# daily min/max temp

# dependencies ------------------------------------------------------------

# install.packages("foreSIGHT")

library(foreSIGHT)
library(tidyverse)

# following tutorial ------------------------------------------------------

# https://cran.r-project.org/web/packages/foreSIGHT/vignettes/Vignette_Tutorial.html

a <- viewAttributes()

# examing ppt attributes
p <- str_subset(a, "^[Pp]_")
names(p) <- p
map_chr(p, viewAttributeDef)

# precip compabitable models

# doesn't look like there is a weather gen model where each month can
# be held totally constant (just harmonic smoother fit)
viewModels("P", compatibleAtts = TRUE)

data("tankDat")
tank_obs

# attributes to track
attSel <- c("P_ann_tot_m", "P_ann_dyWet_m", "P_ann_P99_m", "P_ann_dyWet99p_m", "P_ann_nWet_m")
attSel %in% p
mean(tank_obs$P[tank_obs$P >= 1]) # P_ann_dyWet_m is based on days > 1 mm

# Note to apply this to function to the weather database values first need
# to be transformed to mm, (then back to cm for soilwat2), because I don't 
# see a way of adjust the wet day threshold (1 mm)

tank_obs_atts <- calculateAttributes(tank_obs, attSel)
tank_obs_atts
hist(tank_obs$P)
e <- ecdf(tank_obs$P) # double checking 99th percentile calculation
quantile(e, 0.99)

# the reason I'm just adjusting the 99th percentile of wet days not all days
# is that the gamma distribution is only fit to wet days. 
# therefore, daily precip will drive changes in number of wet days, but
# P_ann_dyWet99p_m will not. While both will drive the shape of the gamma
# distribution
attPerturb <- c("P_ann_dyWet_m", "P_ann_dyWet99p_m")
# attributes to hold constant
monthly_atts <- str_subset(p, "P_[A-Z][a-z]{2}_tot_m") # attributes of monthly ppt
attHold <- c("P_ann_tot_m", monthly_atts)
n_hold <- length(attHold)

# the file should contain all perturbed and held attributes
tempFile <- paste0(tempdir(), "\\targetsFile.csv")
attTargets <- rbind(c(1, 1, rep(1, n_hold)), # hold all constant
                    c(1.25, 1.25, rep(1, n_hold)),
                    c(1.25, 1.5, rep(1, n_hold))) # increase tail more

colnames(attTargets) <- c(attPerturb, attHold)
write.table(attTargets, file = tempFile, sep = ",")

# creating exposure space using targets from csv file
expSpace_Irreg <- createExpSpace(attPerturb = attPerturb,
                                 attPerturbSamp = NULL,
                                 attPerturbMin = NULL,
                                 attPerturbMax = NULL,
                                 attPerturbType = NULL,
                                 attPerturbBy = NULL,
                                 attHold = attHold,
                                 attTargetsFile = tempFile)


# running example ---------------------------------------------------------

sim <- generateScenarios(reference = tank_obs,   # reference time series 
                         expSpace = expSpace_Irreg,    # exposure space
                         numReplicates = 1)      # number of replicates

plotScenarios(sim)
obsAtts <- calculateAttributes(tank_obs, attSel)

# get the simulated precipitation and dates from sim & calculate the same attributes

# Note: fit is terrible. total ppt not adjusted appropriately.

P <- sim[["Rep1"]][["Target1"]][["P"]][["sim"]]   
simData <- cbind(sim[["simDates"]], P)
simAtts <- calculateAttributes(simData, attSel)
simAtts
obsAtts
