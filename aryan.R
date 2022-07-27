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

# data <- data %>%
#  filter(games >= 0 & games < 4)

#glm(bin_diag ~ games, "binomial", data)
#?glm

test_range <- 540
test_data <- head(data, test_range)
data <- tail(data, -test_range)

xfactors <- model.matrix(bin_diag ~ as.factor(games) + as.factor(taxes) + as.factor(motsev) + as.factor(hallsev) + as.factor(travel))[, -1]
x        <- as.matrix(data.frame(traila, trailb, csfvol, xfactors))

# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
glmmod <- glmnet(x, y=as.factor(bin_diag), alpha=1, family="binomial")

# Plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod, xvar="lambda")

coef(glmmod)[,10]

cv.glmmod <- cv.glmnet(x, y=bin_diag, alpha=1)
plot(cv.glmmod)

(best.lambda <- cv.glmmod$lambda.min)


test_xfactors <- model.matrix(test_data$bin_diag ~ as.factor(test_data$games) + as.factor(test_data$taxes) + as.factor(test_data$motsev) + as.factor(test_data$hallsev) + as.factor(test_data$travel))[, -1]
test_x        <- as.matrix(data.frame(test_data$traila, test_data$trailb, test_data$csfvol, test_xfactors))



actual <- test_data$bin_diag[1:test_range]
pred <- predict(cv.glmmod, newx = test_x)

cutoff <- 0.5
pred <- replace(pred, pred<cutoff, 0)
pred <- replace(pred, pred>cutoff, 1)

plot(actual, lwd=1, type="o", col="blue", ylab="Actual vs. Predicted")

lines(pred, lwd=2, type="o", col="red")

# Calculate accuracy
count <- 0
for (i in 1:test_range) {
  if(actual[i] == pred[i]) {
    count <- count + 1
  }
}

accuracy <- count/test_range*100
accuracy


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

