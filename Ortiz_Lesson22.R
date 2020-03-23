library(tidyverse)
library(Lahman)

#find Big Papi's playerID
Master %>% 
  filter(nameLast == "Ortiz", nameFirst == "David") %>% 
  pull(playerID)-> ortiz.id

# Other playerIDs we might be interested in.
player.ids <- c("rodrial01", "ortizda01", "jeterde01", "pujolal01")

#get Ortiz's stats by age using get_stats
source("Chapter8_functions.R")

Ortiz.stats <- get_stats(ortiz.id)

# plot data

# fit model and extract coefficients

# What is A (Poll)?

# Compare to other players

# Which player has the largest C in magnitude (poll)

# Breaking these models is pretty easy (see Ortiz)


