library(dplyr)
library(tidyverse)
getwd()

raw.dat <- read.csv("raw_data/maternalmortality.csv", stringsAsFactors = FALSE)

#select() function  subset the data to have only the variables Country.Name, X2000 – X2019
dat <- raw.dat %>% select(Country.Name, X2000:X2019)
head(dat)

# pivot_longer() function to convert the data set into a long format
#  select the columns X2000 to X2019, 
# remove the prefix X from them, 
# change the name of the variable to Year, change the values to MatMor. 
# Finally, make sure the year variable is stored as numeric
dat.long <- dat %>% pivot_longer(X2000:X2019, names_prefix = "X", names_to = "Year", values_to = "MatMor", 
                                 names_transform = list(Year = as.double))
# str(dat.long)

library(usethis)
usethis::use_git_config(user.name = "anastat", user.email = "anastasia.teterina@gmail.com")
usethis::git_sitrep()

usethis::use_git()


