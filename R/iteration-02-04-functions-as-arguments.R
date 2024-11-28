library("conflicted")
library("palmerpenguins")
library("tidyverse")
library("here")

## Functions as arguments

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  scale_color_discrete(labels = tolower) # tolower is a function


