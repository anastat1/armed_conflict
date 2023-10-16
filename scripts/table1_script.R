library(tidyverse)
library(here)
library(table1)

rfa <- read.csv(paste0(here(), "/data/final_dat.csv"), stringsAsFactors = FALSE)
# subset only observations from year 2000
rft <- rfa |> filter(year == 2000) |> mutate(confl.IND.ch = case_when(confl.IND == 1 ~ "Yes", 
                                                 confl.IND == 0 ~ "No"), 
                        drought.ch = case_when(drought == 1 ~ "Yes", 
                                               drought == 0 ~ "No"), 
                        eathquake.ch = case_when(earthquake == 1 ~ "Yes", 
                                                 earthquake == 0 ~ "No"), 
                        OECD.ch = case_when(OECD == 1 ~ "Yes", 
                                              OECD == 0 ~ "No"))

labels <- list(variables=list(drought.ch = "Drought", 
                   eathquake.ch = "Earthquake", 
                   temp = "Temperature", 
                   OECD.ch = "OECD membership", 
                   popdens = "Population density", 
                   urban = "Urban residence", 
                   agedep = "Age dependency ratio", 
                   male_edu = "Education (yrs per capita)", 
                   mat.mort = "Maternal mortality", 
                   neonat.mort = "Neotatal mortality", 
                   inf.mort = "Infant mortality", 
                   und5.mort = "Under 5 mortality", 
                   total.best = "Battle-related deaths"),
    groups=list("", "", "Armed conflict"))

CC <- complete.cases(rft[, c("GDP", "OECD", "popdens", "urban", "agedep", "male_edu", "temp", 
                             "total.best", "mat.mort")])
check <- rft[CC,]
strata <- c(list(Total=rft), list(CompleteCases = check), split(rft, rft$confl.IND.ch))

table1(strata, labels, groupspan=c(1, 1, 2), 
       render.continuous=c("Median [Min, Max]"), 
       footnote = 
         "Continuous variables presented as median (min, max), categorical as n(%); 
       Complete Cases are for outcome of maternal mortality", 
       caption = "Table 1. Main outcomes and covariates, Armed conflict dataset") 