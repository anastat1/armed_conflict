

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






