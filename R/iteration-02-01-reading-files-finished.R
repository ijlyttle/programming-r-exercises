library("tidyverse")
library("readxl")
library("here")
library("fs")

## Aside
here("data/gapminder/1952.xlsx")

## Our turn
data1952 <- readxl::read_excel(here("data/gapminder/1952.xlsx"))
data1957 <- readxl::read_excel(here("data/gapminder/1957.xlsx"))
data1962 <- readxl::read_excel(here("data/gapminder/1962.xlsx"))
data1967 <- readxl::read_excel(here("data/gapminder/1967.xlsx"))

data_manual <- bind_rows(data1952, data1957, data1962, data1967)

# What problems do you see so far?
# (I see two "real" problems, one philosophical problem)

# ?basename(), ?str_extract()
get_year <- function(x) {
  # ^\\d+ - starts with one or more digits
  x |> basename() |> str_extract("^\\d+")
}

get_year("taylor/swift/1989.txt")

# ?set_names(), ?as.list()
paths <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder")) |>
  # extract the year as names
  set_names(get_year) |>
  # convert to list
  as.list() |>
  print()

data <-
  paths |>
  # read each file from excel
  map(read_excel) |>
  # keep only non-null elements
  # set list-names as column `year`
  # bind into single data-frame
  list_rbind(names_to = "year") |>
  # convert year to number
  mutate(year = parse_number(year)) |>
  print()

paths_party <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder_party")) |>
  # convert to list
  as.list() |>
  # extract the year as names
  set_names(get_year) |>
  print()

# modify read-function to return NULL, rather than throw error
poss_read_excel <- possibly(read_excel, otherwise = NULL)

data_party <-
  paths_party |>
  # read each file from excel
  map(poss_read_excel) |>
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
  mutate(year = parse_number(year)) |>
  print()

identical(
  data_horrible |> select(year, everything()),
  data
)
