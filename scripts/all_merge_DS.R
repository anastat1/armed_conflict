# Preparing disaster dataset ====================================================

library(dplyr)
library(tidyverse)
library(here)
here()

#names(conflict.dat)

# binary variable indicat- ing the presence of conflict for each country–year observation 
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
# drought earthquake death armed conflict if na then 0

dat.mrg$earthquake

write.csv(dat.mrg, "data/final_dat.csv", row.names = FALSE)



