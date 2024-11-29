library("conflicted")
library("palmerpenguins")
library("tidyverse")

## Functions as arguments

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  scale_color_discrete(labels = tolower) # tolower is a function

tolower(c("Emma", "Ian"))

ggplot(penguins, aes(x = bill_length_mm, color = species)) +
  stat_ecdf()

## Our turn: Labeller

# percent_labeller is a function
# scales::label_percent() is a function factory
percent_labeller <- scales::label_percent(accuracy = 1)

percent_labeller(c(0, 0.1, 0.234, 1))

## Your turn

# add `scale_y_continuous()` to ecdf plot, using the `labels` argument
ggplot(penguins, aes(x = bill_length_mm, color = species)) +
  stat_ecdf() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))

