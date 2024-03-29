alz <- read.csv("alzheimer_data.csv")
attach(alz)
library(car)
library(ggplot2)
#scatter plot matrix of question data 
ran <- sample(1:2700,2700)
new_data <- alz[ran,]

pairs(~diagnosis +shopping+bills+taxes+stove+mealprep+travel, data = new_data, main="Simple Scatterplot Matrix")


#barplots relating diagnosis with certain question variables
shop_graph = ggplot(data = alz, aes(x = as.factor(shopping), fill = as.factor(diagnosis))) + geom_bar()
shop_graph

bills_graph = ggplot(data = alz, aes(x = as.factor(bills), fill = as.factor(diagnosis))) + geom_bar()
bills_graph

taxes_graph = ggplot(data = alz, aes(x = as.factor(taxes), fill = as.factor(diagnosis))) + geom_bar()
taxes_graph

stove_graph = ggplot(data = alz, aes(x = as.factor(stove), fill = as.factor(diagnosis))) + geom_bar()
stove_graph

mealprep_graph = ggplot(data = alz, aes(x = as.factor(mealprep), fill = as.factor(diagnosis))) + geom_bar()
mealprep_graph

travel_graph =  ggplot(data = alz, aes(x = as.factor(travel), fill = as.factor(diagnosis))) + geom_bar()
travel_graph

chess_graph =  ggplot(data = alz, aes(x = as.factor(games), fill = as.factor(diagnosis))) + geom_bar()
chess_graph


shop_table = table(diagnosis,shopping)
shop_chisquaretest = chisq.test(shop_table)
shop_chisquaretest

bills_table = table(diagnosis,bills)
bills_chisquaretest = chisq.test(bills_table)
bills_chisquaretest

taxes_table = table(diagnosis,taxes)
taxes_chisquaretest = chisq.test(taxes_table)
taxes_chisquaretest

stove_table = table(diagnosis,stove)
stove_chisquaretest = chisq.test(stove_table)
stove_chisquaretest

meal_table = table(diagnosis,mealprep)
meal_chisquaretest = chisq.test(meal_table)
meal_chisquaretest

travel_table = table(diagnosis,travel)
travel_chisquaretest = chisq.test(travel_table)
travel_chisquaretest

chess_table = table(diagnosis,games)
chess_chisquaretest = chisq.test(chess_table)
chess_chisquaretest
