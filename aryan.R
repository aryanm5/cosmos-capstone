# ARYAN'S FILE

library(tidyverse)

data <- readr::read_csv("alzheimer_data.csv")
attach(data)

# MODEL

library(glmnet)

test_range <- 438 # Length of testing dataset (438 = 20% of training dataset)
test_data <- head(data, test_range) # Get first 438 rows
data <- tail(data, -test_range) # Delete first 438 rows from training data

# xfactors for categorical variables, x for numerical
xfactors <- model.matrix(bin_diag ~ as.factor(games) + as.factor(taxes) + as.factor(motsev) + as.factor(hallsev) + as.factor(travel))[, -1]
x        <- as.matrix(data.frame(traila, trailb, csfvol, lhippo, rhippo, frcort, lparcort, rparcort, ltempcor, rtempcor, lcac, rcac, lent, rent, lparhip, rparhip, lposcin, rposcin, xfactors))

# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
glmmod <- glmnet(x, y=as.factor(bin_diag), alpha=1, family="binomial")


# Plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod, xvar="lambda")


coef(glmmod)[,10] # Get coefficients (beta variables)

cv.glmmod <- cv.glmnet(x, y=bin_diag, alpha=1)


plot(cv.glmmod)

model <- glm(bin_diag ~ x) # this is the model with no lasso

(best.lambda <- cv.glmmod$lambda.min) # Best lambda value? Not sure

# Inputs to predict from
test_xfactors <- model.matrix(test_data$bin_diag ~ as.factor(test_data$games) + as.factor(test_data$taxes) + as.factor(test_data$motsev) + as.factor(test_data$hallsev) + as.factor(test_data$travel))[, -1]
test_x        <- as.matrix(data.frame(test_data$traila, test_data$trailb, test_data$csfvol, test_data$lhippo, test_data$rhippo, test_data$frcort, test_data$lparcort, test_data$rparcort, test_data$ltempcor, test_data$rtempcor, test_data$lcac, test_data$rcac, test_data$lent, test_data$rent, test_data$lparhip, test_data$rparhip, test_data$lposcin, test_data$rposcin, test_xfactors))

actual <- test_data$bin_diag # Actual values to compare to prediction
pred <- predict(cv.glmmod, newx = test_x) # Run prediction

cutoff <- 0.5 # Prediction is > or < than 0.5.
pred <- replace(pred, pred < cutoff, 0) # Replace predictions < 0.5 with 0
pred <- replace(pred, pred >= cutoff, 1) # Replace predictions >= 0.5 with 1

#plot(actual, lwd=1, type="o", col="blue", ylab="Actual vs. Predicted") # Plot actual values

#lines(pred, lwd=2, type="o", col="red") # Plot predicted values

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

