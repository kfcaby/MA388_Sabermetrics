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

ARod %>% 
  ggplot(aes(x = Age, y = OPS)) +
  geom_point() + labs("ARod") +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              se = FALSE)

fit_model(ARod)

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
  split(pull(.,nameLast)) %>% 
  map(fit_model)

#Arod  C = -0.003

# Breaking these models is pretty easy (see Ortiz)

# Usually, we would use these to predict future performance

get_stats("ortizda01") -> Ortiz

#fit the quadratic model to Big Papi at 29 years old

Ortiz %>% filter(Age < 30) -> Ortiz.young

fit_model(Ortiz.young)

#problem--- Ortiz has a positive C!!!!!

Ortiz %>% 
  mutate(OPS_predict = 1.2323 + 0.1532*(Age-30) + 0.01059*(Age-30)^2) -> Ortiz

Ortiz %>% 
  ggplot(aes(x = Age, y = OPS_predict)) +
  geom_line() +
  geom_point(aes(x = Age, y = OPS))


