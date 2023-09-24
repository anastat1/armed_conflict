library(here)


library(dplyr)
library(tidyverse)
getwd()



raw.dat <- read.csv("raw_data/maternalmortality.csv", stringsAsFactors = FALSE)

#select() function  subset the data to have only the variables Country.Name, X2000 – X2019
dat.mat <- raw.dat %>% select(Country.Name, X2000:X2019)
head(dat)

# pivot_longer() function to convert the data set into a long format
#  select the columns X2000 to X2019, 
# remove the prefix X from them, 
# change the name of the variable to Year, change the values to MatMor. 
# Finally, make sure the year variable is stored as numeric
dat.mat.long <- dat.mat %>% pivot_longer(X2000:X2019, names_prefix = "X", names_to = "Year", values_to = "MatMor", 
                                 names_transform = list(Year = as.double))
# str(dat.long)

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


library(usethis)
usethis::use_git_config(user.name = "anastat", user.email = "anastasia.teterina@gmail.com")
usethis::git_sitrep()

usethis::use_git()

usethis::use_github() 
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)


usethis::create_github_token()
# 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



