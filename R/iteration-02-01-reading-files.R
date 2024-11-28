library("conflicted")
library("tidyverse")
library("readxl")
library("here")
library("fs")

## Aside
here("data/file.csv")

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

paths_party <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder_party")) |>
  # convert to list
  as.list() |>
  # extract the year as names
  set_names(get_year) |>
  print()

# possibly
poss_read_csv <- possibly(read_csv, otherwise = NULL, quiet = FALSE)

poss_read_csv("not/a/file.csv")

poss_read_csv(I("a, b\n 1, 2"), col_types = "dd")

# modify read-function to return NULL, rather than throw error
poss_read_excel <- possibly() # we do the rest

data_party <-
  data_party <-
  paths_party |>
  # read each file from excel
  map(read_excel) |>
  # keep only non-null elements
  # set list-names as column `year`
  # bind into single data-frame
  list_rbind(names_to = "year") |>
  # convert year to number
  mutate(year = parse_number(year)) |>
  print()

# intermediate step - see which one failed
paths_party |>
  map(poss_read_excel) |>
  keep(is.null)

identical(data_party, data)

## Fundamental operators
num <- 1:4

num |> map_int(\(x) x + 1)

num |> keep(\(x) x %% 2 == 0)

num |> reduce(\(acc, x) acc + x)

## Horrible example

# keep only non-null elements
# set list-names as column `year`
# bind into single data-frame
list_rbind2 <- function(df, names_to) {
  df |>
    purrr::keep(\(x) !is.null(x)) |>
    purrr::imap(\(d, name) dplyr::mutate(d, "{names_to}" := name)) |>
    purrr::reduce(rbind)
}

data_horrible <-
  paths |>
  map(read_excel) |>
  list_rbind2(names_to = "year") |>
  mutate(year = parse_number(year))

identical(
  data_horrible |> select(year, everything()),
  data
)
