library(tidyverse)

data <- readr::read_csv("alzheimer_data.csv")
attach(data)

# PROPORTION TESTING

# Percentage of bad chess with a diagnosis: 96.93

data %>%
  filter(games > 0 & games < 4 & diagnosis > 0) %>%
  nrow() /
  data %>%
  filter(games > 0 & games < 4) %>%
  nrow()

# Number of bad chess samples: 358

data %>%
  filter(games > 0 & games < 4) %>%
  nrow()

# Percentage of TRAILA >= 90 with a diagnosis: 92.86%

data %>%
  filter(traila >= 90 & traila <= 150 & diagnosis > 0) %>%
  nrow() /
  data %>%
  filter(traila >= 90 & traila <= 150) %>%
  nrow()

# Percentage of TRAILB >= 260 with a diagnosis: 90.86%

data %>%
  filter(trailb >= 260 & trailb <= 300 & diagnosis > 0) %>%
  nrow() /
  data %>%
  filter(trailb >= 260 & trailb <= 300) %>%
  nrow()