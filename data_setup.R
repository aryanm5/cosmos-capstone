library(tidyverse)

data <- readr::read_csv("alzheimer_data.csv")


data <- data %>%
  filter(games >= 0 & games < 4) %>%
  filter(taxes >= 0 & taxes < 4) %>%
  filter(motsev >= 0 & motsev < 4) %>%
  filter(hallsev >= 0 & hallsev < 4) %>%
  filter(travel >= 0 & travel < 4) %>%
  filter(traila >= 0 & traila <= 150) %>%
  filter(trailb >= 0 & trailb <= 300) %>%
  mutate(bin_diag = as.integer(diagnosis != 0)) %>%
  mutate(bin_games = as.integer(games != 0)) %>%
  mutate(bin_taxes = as.integer(taxes != 0)) %>%
  mutate(bin_motsev = as.integer(motsev != 0)) %>%
  mutate(bin_hallsev = as.integer(hallsev != 0)) %>%
  mutate(bin_travel = as.integer(travel != 0))
  

write.table(
  data,
  "alzheimer_data.csv",
  quote = FALSE,
  eol = "\n\n",
  row.names = FALSE,
  sep = ","
)