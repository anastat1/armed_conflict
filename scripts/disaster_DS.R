# Preparing disaster dataset ====================================================

library(dplyr)
library(tidyverse)
library(here)
here()

# Download disaster.csv
raw.dat <- read.csv("raw_data/disaster.csv", stringsAsFactors = FALSE)
# Use the filter() function to subset the data set to only include years 2000–2019 and 
# the disaster types “Earthquake” and “Drought”
dat.dis <- raw.dat %>% filter(Year>1999 & Year <2020 & Disaster.Type %in% c("Earthquake", "Drought")) %>%
# Subset the data set to only include the following variables: Year, ISO, Disaster.type.
  select(Year, ISO, Disaster.Type) %>%
# dummy variable drought and another dummy variable earthquake such that:
  mutate(drought = ifelse(Disaster.Type == "Drought", 1, 0), 
         earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0)) %>%
# Use the group_by() and summarize() functions to create a data set where only 
  # one row of observation exists for each country and each year,
  group_by(Year, ISO) %>% summarise(drought = ifelse(sum(drought) >0, 1, 0), 
                                    earthquake = ifelse(sum(earthquake) >0, 1, 0))

names(dat.dis)

