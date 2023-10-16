
finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)
finaldata |>
  dplyr::select(country_name, ISO, year, MatMor) |>
  dplyr::filter(year < 2018) |>
  arrange(ISO, year) |>
  group_by(ISO) |>
  mutate(diffmatmor = MatMor - MatMor[1L]) 

# This is just a scratch pad file for different things ==========================

library(usethis)
usethis::use_git_config(user.name = "anastat", user.email = "anastasia.teterina@gmail.com")
usethis::git_sitrep()

usethis::use_git()

usethis::use_github() 
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)






