# ARYAN'S FILE

library(tidyverse)

data <- readr::read_csv("alzheimer_data.csv")
attach(data)

# Percentage of bad chess with a diagnosis: 96.58

data %>%
  filter(games > 0 & games < 4 & diagnosis > 0) %>%
  nrow() /
  data %>%
  filter(games > 0 & games < 4) %>%
  nrow()

# Number of bad chess samples: 438

data %>%
  filter(games > 0 & games < 4) %>%
  nrow()

# Percentage of TRAILA >= 90 with a diagnosis: 91.89%

data %>%
  filter(traila >= 90 & traila <= 150 & diagnosis > 0) %>%
  nrow() /
  data %>%
  filter(traila >= 90 & traila <= 150) %>%
  nrow()

# Percentage of TRAILB >= 250 with a diagnosis: 91.77%

data %>%
  filter(trailb >= 250 & trailb <= 300 & diagnosis > 0) %>%
  nrow() /
  data %>%
  filter(trailb >= 250 & trailb <= 300) %>%
  nrow()