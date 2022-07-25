#setup
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

alzheimer_data = read_csv("alzheimer_data.csv")
alz = alzheimer_data
attach(alz)

#game plots
alz_game_mod = alz %>% filter(games > -1 & games < 4)

alz_game_mod %>% 
  ggplot(aes(x = traila, y = games, color = as.factor(diagnosis))) + geom_point()

alz_game_mod %>% 
  ggplot(aes(x = as.factor(games), fill = as.factor(diagnosis))) + geom_bar()

#digif plots
alz_digif_mod = alz %>% filter(digif < 13)

alz_digif_mod %>% 
  ggplot(aes(x = as.factor(digif), fill = as.factor(diagnosis))) + geom_bar()

#shopping, bills, stove and taxes plots
alz_shop_mod = alz %>% filter(shopping > -1 & shopping < 4)

alz_shop_mod %>% 
  ggplot(aes(x = as.factor(shopping), fill = as.factor(diagnosis))) + geom_bar()


alz %>% ggplot(aes(x = as.factor(hallsev), fill = as.factor(diagnosis))) + geom_bar()

#hallsev diagnosis percentages for hallsev > 0
alz_hallsev_non0 = filter(alz, hallsev > 0 & hallsev < 4)
hallsev_perc = nrow(filter(alz_hallsev_non0, diagnosis > 0)) / nrow(alz_hallsev_non0)

nrow(alz_hallsev_non0)

#motsev diagnosis percentages for motsev > 0
alz_motsev_non0 = filter(alz, motsev > 0 & motsev < 4)
motsev_perc = nrow(filter(alz_motsev_non0, diagnosis > 0)) / nrow(alz_motsev_non0)

nrow(alz_motsev_non0)
