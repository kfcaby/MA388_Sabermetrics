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

Ortiz.stats %>% 
  ggplot(aes(x = Age, y = OPS)) +
  geom_point() + labs(title = "Ortiz") +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              se = FALSE)

# fit model and extract coefficients

fit_model(Ortiz.stats)

# What is A (Poll)?

# Compare to other players

player.ids %>% 
  map_df(get_stats) %>% 
  left_join(Master %>% select(nameLast, playerID)) -> player.stats

player.stats %>% 
  ggplot(aes(x = Age, y = OPS)) +
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              se = FALSE) +
  facet_wrap( ~ nameLast)

# Which player has the largest C in magnitude (poll)

player.stats %>% 
  split(pull(.,playerID)) %>% 
  map(fit_model)
#Arod C = -0.0030

# Breaking these models is pretty easy (see Ortiz)
Ortiz.stats %>% filter(Age < 30) -> Ortiz.young

fit_model(Ortiz.young)

Ortiz.stats %>% 
  mutate(OPS_predict = 1.23 + 0.15 * (Age - 30) + 0.0159*(Age - 30)^2) -> Ortiz.stats

Ortiz.stats %>% 
  ggplot(aes(x = Age, y = OPS_predict)) +
  geom_line() +
  geom_point(aes(Age,OPS))
  

