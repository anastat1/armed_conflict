# Preparing maternal mortality dataset =========================================

library(dplyr)
library(tidyverse)
library(here)

here()

# Reading in source data; all in wide format
mat.raw <- read.csv("raw_data/maternalmortality.csv", stringsAsFactors = FALSE)
inf.raw <- read.csv("raw_data/infantmortality.csv", stringsAsFactors = FALSE)
neonat.raw <- read.csv("raw_data/neonatalmortality.csv", stringsAsFactors = FALSE)
und5.raw <- read.csv("raw_data/under5mortality.csv", stringsAsFactors = FALSE)

# function to transform data from wide into long format
cleanmort_ds1 <- function(ds, name){
  ds %>% select(Country.Name, X2000:X2019) %>%
   pivot_longer(X2000:X2019, names_prefix = "X", names_to = "Year", values_to = name, 
                                 names_transform = list(Year = as.double))
}

# transforming all 4 datasets in wide format into 4 datasets in long format
raw.dat.lst <- list(mat.raw, inf.raw, neonat.raw, und5.raw)
names.vec <- c("mat.mort", "inf.mort", "neonat.mort", "und5.mort")
cln.dat.lst <- mapply(cleanmort_ds1, raw.dat.lst, names.vec, SIMPLIFY = FALSE)
#check <- cln.dat.lst[[2]]

# merging all datasets into one with 4 outcome variables
dat.fin <- reduce(cln.dat.lst, full_join)
#str(dat.fin)

# adding the ISO-3 country code variable 
library(data.table)
library(states)
library(countrycode)
dat.fin$ISO <- countrycode(dat.fin$Country.Name, 
                             origin = "country.name", 
                             destination = "iso3c")
dat.fin$Country.Name <- NULL

# writing the result into .csv file
#write.csv(dat.fin, "data/mortality_clean.csv", row.names = FALSE)

rm(list = ls())

