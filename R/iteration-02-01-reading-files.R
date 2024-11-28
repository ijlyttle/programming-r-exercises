library("conflicted")
library("tidyverse")
library("readxl")
library("here")
library("fs")

## Aside
here("data/gapminder/1952.xlsx")

## Our turn
data1952 <- read_excel(here("data/gapminder/1952.xlsx"))
data1957 <- read_excel(here("data/gapminder/1957.xlsx"))
data1962 <- read_excel(here("data/gapminder/1952.xlsx"))
data1967 <- read_excel(here("data/gapminder/1967.xlsx"))

data_manual <- bind_rows(data1952, data1957, data1962, data1967)

# What problems do you see so far?
# (I see two "real" problems, one philosophical problem)

# ?basename(), ?str_extract()
get_year <- function(x) {

}

get_year("taylor/swift/1989.txt")

# ?as.list(), ?set_names()
paths <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder")) |>
  # convert to list
  # extract the year as names
  print()

# ?read_excel(), ?list_rbind(), ?parse_number()
data <-
  paths |>
  # read each file from excel, into data frame
  # keep only non-null elements
  # set list-names as column `year`
  # bind into single data-frame
  # convert year to number
  print()

