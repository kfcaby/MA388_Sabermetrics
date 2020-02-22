
library(tidyverse)

pitches <- read_csv("data/pitches2016.csv")

taken <- pitches %>% filter(type %in% c("B","C"))

seq_x <- seq(-1.4,1.4, by = 0.4)
seq_z <- seq(1.1,3.9, by = 0.4)

taken %>% 
  mutate(plate_x = cut(px, seq_x),
         plate_z = cut(pz, seq_z)) -> taken

#find the percent of called strikes within each zone
taken %>% 
  group_by(plate_x,plate_z,type) %>% 
  count() %>% 
  drop_na() %>% 
  group_by(plate_x,plate_z) %>% 
  mutate(percent = n/sum(n)) %>% 
  filter(type == "C") -> summary

#create a grid to plot it on.
key <- tibble(px = seq_x - 0.2,
              pz = seq_z - 0.2) %>% 
  mutate(plate_x = cut(px, seq_x),
         plate_z = cut(pz, seq_z))

library(modelr)
grid <- key %>% 
  data_grid(plate_x, plate_z) %>% 
  left_join(key %>% select(px, plate_x), by = "plate_x") %>% 
  left_join(key %>% select(pz, plate_z), by = "plate_z") %>% 
  left_join(summary, by = c("plate_x", "plate_z")) %>% 
  drop_na()
  
source("kzoneplot.R")

k_zone_plot %+% grid + geom_tile(aes(fill = percent),
                                 alpha = 0.7) +
  scale_fill_gradient(low = "gray92", high = "blue")
k_zone_plot
