library(Lahman)
library(tidyverse)


#find Arod :( player ID
Master %>% 
  filter(nameLast == "Rodriguez", nameFirst == "Alex") %>% 
  pull(playerID) -> arod.id

# Other playerIDs we might be interested in.
player.ids <- c("rodrial01", "ortizda01", "jeterde01", "pujolal01")

#get_stats (pg 180)

#this function is in Chapter8_functions.R
source("Chapter8_functions.R")

get_stats(arod.id) -> ARod

# plot data

# fit model and extract coefficients

# What is A (Poll)?

# Compare to other players

# Which player has the largest C in magnitude (poll)

# Breaking these models is pretty easy (see Ortiz)



           

