# Week 10 In-class Assignment
# differences in median (with the BCa boot- strap 95% confidence intervals) maternal, infant, neonatal, and under-5 mortality between the countries exposed 
# to versus not exposed to armed conflict for the year 2017


library(tidyverse)
library(here)
library(boot)
# filter() and lag() are functions that exist in both R packages dplyr and stats
# make default choice the functions from the dplyr package
#filter <- dplyr::filter
#lag <- dplyr::lag

# rfa = ready for analysis
rfa <- read.csv(paste0(here(), "/data/final_dat_300923.csv"), stringsAsFactors = FALSE)

set.seed(231)

matmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(MatMor) & armconf1 == 1) |>
  dplyr::select(ISO, MatMor)
matmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(MatMor) & armconf1 == 0) |>
  dplyr::select(ISO, MatMor)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resamp.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$MatMor) - median(resamp.arm0$MatMor)
}
head(resamp.arm1, 12)


library(boot)

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$MatMor, sample_data$armconf1, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout

# other packages (functions) np.boot

# The observed value of the statistic applied to data.
bootout$t0

# A matrix with sum(R) rows each of which is a bootstrap replicate of the result of calling statistic.
head(bootout$t)

# Bootstrap standard error estimate
sd(bootout$t)

# Percentile bootstrap CI
quantile(bootout$t, probs = c(0.025, 0.975))

# The basic (or reverse percentile) method 
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)
2 * bootout$t0 - quantile(bootout$t, probs = 0.025)

boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))
