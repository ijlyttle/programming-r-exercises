library("conflicted")
library("tidyverse")
library("readxl")
library("here")
library("fs")

# ?basename(), ?str_extract()
get_year <- function(x) {
  # ^\\d+ - starts with one or more digits
  x |> basename() |> str_extract("^\\d+")
}

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


## Horrible example

# keep only non-null elements
# set list-names as column `year`
# bind into single data-frame

data_horrible <-
  paths |>
  map(read_excel) |>
  keep(negate(is.null)) |>
  lmap(\(df, name) mutate(df, "year" := parse_number(name))) |>
  reduce(rbind)
