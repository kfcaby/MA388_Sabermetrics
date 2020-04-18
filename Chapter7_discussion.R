library(tidyverse)
library(lubridate)
library(baseballr)
library(mgcv)

source("kzoneplot.R")

#Is there a relationship between Count (Balls - Strikes) and called strike percentage on taken pitches

pitches <- read_csv(file = "data/pitches2016.csv")

pitches %>% 
  filter(type %in% c("B","C")) -> taken

# B - Ball
# C - Called Strike

#Other versions of the data, "S" is called strike.

# model for called strikes as a percent of taken pitches
model.count = gam(type == "C" ~ count, data = taken, family = "binomial")
summary(model.count)

# What are some conclusion we might make from this model?

# called strikes are less frequent on 0-2 - probably because pitchers are trying to get batters to chase
# called strikes are more frequent on 3-2 - probably because pitchers are tyring to get a pitch over the plate.

# interpreting estimates
# estimates are log odds ratios
# a positive log odds ratio indicates the probability is higher (not that the probability is 29%)

# Sometimes it helps to look at probabilities

# Here are predicted strike probabilities for 0-0, 0-2, 3-0
predict(model.count, newdata = data.frame(count = c("0-0","0-2","3-0")),
        type = "response")

# p(Called Strike | pitch was taken and count was "0-0") = 0.3759693

# p(Called Strike | pitch was taken and count was "0-2") = 0.1214286

# p(Called Strike | pitch was taken and count was "3-0") = 0.4473310 


#BUT THIS IS PRIMARY BECAUSE PITCHERS THROW VERY DIFFERENTLY ON 3-0,
# not because the umpire behaves differently.

k_zone_plot %+% filter(taken, count %in% c("0-0","0-2","3-0")) + aes(color = type) +
  geom_point() +
  facet_wrap( ~ count) +
  scale_color_manual(values = c("blue", "black"))


# What variables are related to both the explanatory and response variables?

# How strong are those relationships?

#If we want to investigate umpire decisions, then we should adjust for pitch location.

model.count.pos_adj = gam(type == "C" ~ s(px,pz) + count, data = taken, family = "binomial")
summary(model.count.pos_adj)

#What conclusions would me make here?

#Once again, converting to probabilities can help.

predict(model.count.pos_adj, newdata = data.frame(px = -1, pz = 2.5,count = c("0-0","0-2","3-0")),
        type = "response")






