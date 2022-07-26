# ARYAN'S FILE

library(tidyverse)

data <- readr::read_csv("alzheimer_data.csv")
attach(data)

# Add bin_diag column

# data <- data %>%
#   mutate(bin_diag = as.integer(diagnosis > 0))
# attach(data)
# 
# write.table(data, "alzheimer_data.csv", quote=FALSE, eol="\n\n", row.names=FALSE, sep=",")


# GLM TESTING

library(glmnet)

# glm(games ~ bin_diag, "binomial", data)



# PROPORTION TESTING

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

