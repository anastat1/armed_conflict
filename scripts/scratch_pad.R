

getmeddiff.new <- function(data, indices, outcome) {
  dat <- data[!is.na(data[, outcome]), ]
  sample_data <- dat[indices, ]
  group_meds <- tapply(sample_data[, outcome], sample_data[, "confl.IND"], FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}




library(tidyverse)
library(here)
library(ggplot2)
# filter() and lag() are functions that exist in both R packages dplyr and stats
# make default choice the functions from the dplyr package
#filter <- dplyr::filter
#lag <- dplyr::lag

# rfa = ready for analysis
rfa <- read.csv(paste0(here(), "/data/final_dat_300923.csv"), stringsAsFactors = FALSE)

Hmisc::describe(rfa$mat.mort)
Hmisc::describe(rfa$confl.IND)

check <- rfa |> filter(year == 2017) 
check |> finalfit::missing_compare(dependent = "mat.mort", explanatory = "confl.IND")
check1 <- check |> filter(!is.na(mat.mort))
table(check1$confl.IND)
tapply(check1$mat.mort, check1$confl.IND, median)
164-37.5
check1 |> filter(confl.IND ==1) |> summarise(median(mat.mort))
check1 |> filter(confl.IND ==0) |> summarise(median(mat.mort))



stats <- data.frame(player=c('A', 'B', 'C', 'D'), 
                 runs=c(100, 200, 408, NA), 
                 wickets=c(17, 20, NA, 5)) 
# find variables containing missing values 
var_with_miss_fn <- function(dat){
  names(dat)[(apply(dat, 2, function(x) sum(is.na(x))>0))]
}
var_with_miss_fn(stats)

names(stats)[(apply(stats, 2, function(x) sum(is.na(x))>0))]
stats[unlist(apply(stats, 2, function(x) which(is.na(x))) )]

# This is just a scratch pad file for different things ==========================

library(boot) 
library(table1)

melanoma2 <- melanoma
 
# Factor the basic variables that
# we're interested in
melanoma2$status <- factor(melanoma2$status, levels=c(2,1,3),
                           labels=c("Alive", # Reference
                                    "Melanoma death", "Non-melanoma death"))

table1(~ factor(sex) + age + factor(ulcer) + thickness | status, data=melanoma2)

melanoma2$sex <- factor(melanoma2$sex, levels=c(1,0),
                        labels=c("Male", "Female"))
 
melanoma2$ulcer <- 
  factor(melanoma2$ulcer, levels=c(0,1),
         labels=c("Absent", 
                  "Present"))

label(melanoma2$sex)       <- "Sex"
label(melanoma2$age)       <- "Age"
label(melanoma2$ulcer)     <- "Ulceration"
label(melanoma2$thickness) <- "Thicknessᵃ"

units(melanoma2$age)       <- "years"
units(melanoma2$thickness) <- "mm"

library(tidyverse)
library(here)

rfa <- read.csv(paste0(here(), "/data/final_dat.csv"), stringsAsFactors = FALSE)
names(rfa)
# subset only observations from year 2000
check <- rfa |> select(ISO, year, OECD) |> pivot_wider(names_from = year, values_from = OECD) %>%
  drop(ISO) |>
        rowwise |>
        mutate(mean()) %>%
        ungroup()
?rowwise




caption  <- "Basic stats"
footnote <- "ᵃ Also known as Breslow thickness"

table1(~ sex + age + ulcer + thickness | status, data=melanoma2,
    overall=c(left="Total"), caption=caption, footnote=footnote)

labels <- list(
    variables=list(sex="Sex",
                   age="Age (years)",
                   ulcer="Ulceration",
                   thickness="Thicknessᵃ (mm)"),
    groups=list("", "", "Death"))
#########!!!!!!GOOD ONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
strata <- c(list(Total=melanoma2), split(melanoma2, melanoma2$status))
str(strata)
table1(strata, labels, groupspan=c(1, 1, 2)) 

# Remove the word "death" from the labels, since it now appears above
levels(melanoma2$status) <- c("Alive", "Melanoma", "Non-melanoma")

library(usethis)
usethis::use_git_config(user.name = "anastat", user.email = "anastasia.teterina@gmail.com")
usethis::git_sitrep()

usethis::use_git()

usethis::use_github() 
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)






