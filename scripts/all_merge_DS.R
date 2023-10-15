# Preparing analysis dataset ====================================================

library(dplyr)
library(tidyverse)
library(here)
here()

#names(conflict.dat)

# binary variable indicating the presence of conflict for each country–year observation 
# (0 = no, <25 battle-related deaths; 1 = yes, >=25 battle-related deaths) [29]

# Conflict data DS
source("scripts/armedconflict_DS.R")

# Disaster DS
source("scripts/disaster_DS.R")

# Covariates DS
dat.covar <- read.csv("raw_data/covariates.csv", stringsAsFactors = FALSE)

# Clean mortality DS
dat.mort <- read.csv("data/mortality_clean.csv", stringsAsFactors = FALSE) %>%
  rename(year = Year)

# Merging them
final.lst <- list(dat.covar, conflict.dat, dat.mort, dat.dis)

final.lst |> reduce(left_join, by = c("ISO", "year")) -> dat.mrg

# Imputing NAs as 0 values for armed conflict variables, earthquakes and draughts
dat.mrg <- dat.mrg |> mutate(confl.IND = ifelse(is.na(confl.IND), 0, confl.IND), 
                             total.best = ifelse(is.na(total.best), 0, total.best),
                             drought = ifelse(is.na(drought), 0, drought), 
                             earthquake = ifelse(is.na(earthquake), 0, earthquake))

write.csv(dat.mrg, "data/final_dat.csv", row.names = FALSE)

#sum(is.na(dat.mrg$confl.IND))

