library(Lahman)
library(tidyverse)


#find Arod :( player ID
Master %>% 
  filter(nameLast == "Rodriguez", nameFirst == "Alex") %>% 
  pull(playerID) -> arod.id

Batting %>% 
  filter(playerID == arod.id)

#get_stats (pg 180)

#this function is in Chapter8_functions.R
source("Chapter8_functions.R")

get_stats(arod.id) -> ARod

ARod %>% 
  ggplot(aes(x = Age, y = OPS)) +
  geom_point() +
  labs("ARod :(")
