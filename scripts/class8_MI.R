# Step 1. Load dataset

library(tidyverse)
library(here)
# filter() and lag() are functions that exist in both R packages dplyr and stats
# make default choice the functions from the dplyr package
#filter <- dplyr::filter
#lag <- dplyr::lag

# rfa = ready for analysis
rfa <- read.csv(paste0(here(), "/data/final_dat_300923.csv"), stringsAsFactors = FALSE)

# Step 1. Describe the patterns of missing data

library(naniar)
library(VIM)
library(finalfit)
library(ggplot2)

# Plot the missingness patterns
naniar::vis_miss(rfa) + theme(axis.text.x = element_text(angle = 90))

# extract names of variables containing missing values to subset and fit into plot
var_with_miss_fn <- function(dat){
  names(dat)[(apply(dat, 2, function(x) sum(is.na(x))>0))]
}
var_with_miss_fn(rfa)
# plot only pattern of missing values for variables with missing ones to fit into the plot
check <- rfa[, c("GDP", "popdens", "urban", "male_edu", "temp", 
                 "mat.mort", "inf.mort", "neonat.mort", "und5.mort")]
VIM::aggr(check, numbers = TRUE, prop = c(TRUE, FALSE), cex.axis = 0.8) ; rm(check)

# compare missing vs non-missing maternal mortality with # of battle-related deaths
rfa %>% 
  finalfit::missing_compare(dependent="mat.mort",
                            explanatory=c("total.best")) 

# GDP was scaled up by 1000, population density was rescaled from 0 to 1
rfa$GDP1000 <- rfa$GDP / 1000
rfa$popdens100 <- rfa$popdens / 100

preds <- as.formula(~ confl.IND + 
                      GDP1000 + OECD + popdens100 + urban + agedep + male_edu +
                      temp + earthquake + drought + 
                      ISO + as.factor(year))

mat.mort.mod <- lm(update.formula(preds, mat.mort ~ .), data = rfa)
inf.mort.mod <- lm(update.formula(preds, inf.mort ~ .), data = rfa)
neonat.mort.mod <- lm(update.formula(preds, neonat.mort ~ .), data = rfa)
und5.mort.mod <- lm(update.formula(preds, und5.mort ~ .), data = rfa)

#library(texreg)

tablevars <- list("confl.IND" = "Armed conflict", 
                  "GDP1000" = "GDP",
                  "OECD" = "OEDC membership", 
                  "popdens100" = "population density", 
                  "urban" = "Urban",
                  "agedep" = "Age dependency",
                   "male_edu" = "Male education",
                   "temp" = "Average temperature",
                  "earthquake" = "Earthquake",
                  "drought" = "Drought")

texreg::screenreg(list(mat.mort.mod, und5.mort.mod, inf.mort.mod, neonat.mort.mod), 
                  # Should confidence intervals be used instead of the default standard errors and p-values
                  ci.force = TRUE, 
                  # select, omit, rename, and reorder coefficients
                  custom.coef.map = tablevars, 
                  custom.model.names = c("Maternal mortality", 
                                         "Under-5 mortality",
                                         "Infant mortality", 
                                         "Neonatal mortality"), 
                  caption = "Panel regression models results Complete datasets")

# Use the mice package to multiply impute the final data with 𝑚 = 10 imputations.

library(mice)

#Use 2l.pan to impute all continuous level-1 variables.

# Imputation model: variables to include: all variables that included into the model + 
#   region; 
remove.vars <- c("country_name", "OECD2023", "popdens", "GDP")
rfa.MI <- as.data.frame(rfa)[, !(names(rfa) %in% remove.vars)]; str(rfa.MI)
rfa.MI$ISO <- as.numeric(as.factor(rfa.MI$ISO))

summary(rfa.MI)

# Continuous level 1 variables with missing values
miss.l1.cont <- c("popdens100", "GDP1000", "urban", "male_edu", "temp", 
                  "mat.mort", "inf.mort", "neonat.mort", "und5.mort")

mice.multi.out1 <- mice(rfa.MI, seed = 1, m = 1, maxit = 0, print = F)
meth <- mice.multi.out1$meth
pred <- mice.multi.out1$predictorMatrix

# 2l.pan to impute all continuous level-1 variables.
# ISO as level 2 group variable
pred[miss.l1.cont, "ISO"] <- -2
meth[miss.l1.cont] <- "2l.pan"

mice.multi.out  <- mice(rfa.MI, seed = 100, m = 10, maxit = 20,
                  method = meth,
                  predictorMatrix = pred, print = F)

## fit analysis model and pool results
ds.imp <- complete(mice.multi.out, "all") 
#saveRDS(ds.imp, paste0(here, "/data/ImputedDS.Rds"))

preds.MI <- as.formula(~ confl.IND + 
                      GDP1000 + OECD + popdens100 + urban + agedep + male_edu +
                      temp + earthquake + drought + 
                      as.factor(ISO) + as.factor(year))

mat.mort.imp <- lapply(ds.imp, lm, formula = update.formula(preds.MI, mat.mort ~ .)) %>% pool()
inf.mort.imp <- lapply(ds.imp, lm, formula = update.formula(preds.MI, inf.mort ~ .)) %>% pool()
neonat.mort.imp <- lapply(ds.imp, lm, formula = update.formula(preds.MI, neonat.mort ~ .)) %>% pool()
und5.mort.imp <- lapply(ds.imp, lm, formula = update.formula(preds.MI, und5.mort ~ .)) %>% pool()

# Compare the results from MI to complete case data for each outcome.
tablevars <- list("confl.IND" = "Armed conflict", 
                  "GDP1000" = "GDP",
                  "OECD" = "OEDC membership", 
                  "popdens100" = "population density", 
                  "urban" = "Urban",
                  "agedep" = "Age dependency",
                   "male_edu" = "Male education",
                   "temp" = "Average temperature",
                  "earthquake" = "Earthquake",
                  "drought" = "Drought")

texreg::screenreg(list(mat.mort.mod, mat.mort.imp), 
                  ci.force = TRUE, 
                  custom.coef.map = tablevars, 
                  custom.model.names = c("Complete cases", 
                                         "Imputed"), 
                  caption = "Maternal mortality models")

texreg::screenreg(list(inf.mort.mod, inf.mort.imp), 
                  ci.force = TRUE, 
                  custom.coef.map = tablevars, 
                  custom.model.names = c("Complete cases", 
                                         "Imputed"), 
                  caption = "Infant mortality models")

texreg::screenreg(list(neonat.mort.mod, neonat.mort.imp), 
                  ci.force = TRUE, 
                  custom.coef.map = tablevars, 
                  custom.model.names = c("Complete cases", 
                                         "Imputed"), 
                  caption = "Neonatal mortality models")

texreg::screenreg(list(und5.mort.mod, und5.mort.imp), 
                  ci.force = TRUE, 
                  custom.coef.map = tablevars, 
                  custom.model.names = c("Complete cases", 
                                         "Imputed"), 
                  caption = "Under 5 mortality models")

