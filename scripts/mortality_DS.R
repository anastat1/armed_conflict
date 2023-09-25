# Preparing maternal mortality dataset =========================================

library(dplyr)
library(tidyverse)
library(here)

here()
mat.raw <- read.csv("raw_data/maternalmortality.csv", stringsAsFactors = FALSE)
inf.raw <- read.csv("raw_data/infantmortality.csv", stringsAsFactors = FALSE)
neonat.raw <- read.csv("raw_data/neonatalmortality.csv", stringsAsFactors = FALSE)
und5.raw <- read.csv("raw_data/under5mortality.csv", stringsAsFactors = FALSE)

<<<<<<< HEAD
names.vec <- c("mat.mort", "inf.mort", "neonat.mort", "und5.mort")

=======
>>>>>>> 0aeca007f4e73e28179dd89f269f0c8f46fb8fcc
cleanmort_ds1 <- function(ds, name){
  ds %>% select(Country.Name, X2000:X2019) %>%
   pivot_longer(X2000:X2019, names_prefix = "X", names_to = "Year", values_to = name, 
                                 names_transform = list(Year = as.double))
}

<<<<<<< HEAD
raw.dat.lst <- list(mat.raw, inf.raw, neonat.raw, und5.raw)
cln.dat.lst <- mapply(cleanmort_ds1, raw.dat.lst, names.vec, SIMPLIFY = FALSE)
#check <- cln.dat.lst[[2]]

# apply data cleaning function - DEPRECATED
#names.vec <- c("mat.mort", "inf.mort", "neonat.mort", "und5.mort")
#mat.cln <- cleanmort_ds1(mat.raw, names.vec[1])
#inf.cln <- cleanmort_ds1(inf.raw, names.vec[2])
#neonat.cln <- cleanmort_ds1(neonat.raw, names.vec[3])
#und5.cln <- cleanmort_ds1(und5.raw, names.vec[4])
# create list
#cln.dat.lst <- list(mat.cln, inf.cln, neonat.cln, und5.cln)
=======
# apply data cleaning function
names.vec <- c("mat.mort", "inf.mort", "neonat.mort", "und5.mort")
mat.cln <- cleanmort_ds1(mat.raw, names.vec[1])
inf.cln <- cleanmort_ds1(inf.raw, names.vec[2])
neonat.cln <- cleanmort_ds1(neonat.raw, names.vec[3])
und5.cln <- cleanmort_ds1(und5.raw, names.vec[4])

# create list
cln.dat.lst <- list(mat.cln, inf.cln, neonat.cln, und5.cln)
>>>>>>> 0aeca007f4e73e28179dd89f269f0c8f46fb8fcc

# merged dataset
dat.fin <- reduce(cln.dat.lst, full_join)

library(data.table)
library(states)
library(countrycode)

dat.fin$ISO <- countrycode(dat.fin$Country.Name, 
                             origin = "country.name", 
                             destination = "iso3c")
dat.fin$Country.Name <- NULL

write.csv(dat.fin, "data/mortality_clean.csv", row.names = FALSE)



