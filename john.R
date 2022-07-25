alz <- read.csv("alzheimer_data.csv")
attach(alz)
library(car)
library(ggplot2)
#scatter plot matrix of question data 
ran <- sample(1:2700,2700)
new_data <- alz[ran,]

pairs(~diagnosis +shopping+bills+taxes+stove+mealprep+travel, data = new_data, main="Simple Scatterplot Matrix")


#
?ggplot
ggplot(data = alz, aes(x = shopping, fill = diagnosis)) + 
  geom_bar() + coord_flip()
