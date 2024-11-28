library("conflicted")
library("tidyverse")

conflicts_prefer(dplyr::filter)

mtcars |> filter(cyl == 6)
