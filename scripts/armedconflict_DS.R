# Preparing disaster dataset ====================================================

library(dplyr)
library(tidyverse)
library(here)
here()

#names(conflict.dat)

# total.best Best estimate for total number of battle related deaths

# binary variable indicat- ing the presence of conflict for each country–year observation 
# (0 = no, <25 battle-related deaths; 1 = yes, >=25 battle-related deaths) [29]

# Download disaster.csv
conflict.dat <- read.csv("raw_data/conflictdata.csv", stringsAsFactors = FALSE) %>%
  group_by(ISO, year) %>% summarise(total.best = sum(best)) %>% ungroup() %>%
  mutate(confl.IND = ifelse(total.best <25, 0, 1)) |>
  mutate(year = year + 1) 

# checking difference between lagged and added year versions
check.lag <- read.csv("raw_data/conflictdata.csv", stringsAsFactors = FALSE) %>%
  group_by(ISO, year) %>% summarise(total.best = sum(best)) %>% ungroup() %>%
  mutate(confl.IND = ifelse(total.best <25, 0, 1)) |> group_by(ISO) |> 
  mutate(year.plus = year + 1) |>
   mutate(confl.IND.lag = dplyr::lag(confl.IND, n=1)) |> ungroup() |>
  mutate(check.yr = paste(ISO, year.plus, confl.IND, sep = "_"), 
         check.lag = paste(ISO, year, confl.IND.lag, sep = "_")); 
# Conclusion: the lagged version gives 1 additional record for the country compared to lagged variable
#  i.e. if country observations are up and including yr 2018, the lagged version will only have record for 2018, 
# but the year plus version will additionally have record for 2019.
rm(check.lag)



  



  



