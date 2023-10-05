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
  mutate(confl.ID = ifelse(total.best <25, 0, 1)) 



