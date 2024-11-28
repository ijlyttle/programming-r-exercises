library("conflicted")
library("tidyverse")
library("readxl")
library("here")
library("fs")

## Adverb
poss_read_csv <- possibly(read_csv, otherwise = NULL, quiet = FALSE)

poss_read_csv("not/a/file.csv")

poss_read_csv(I("a, b\n 1, 2"), col_types = "dd")

## File-reading example: handling failure

get_year <- function(x) {
  # ^\\d+ - starts with one or more digits
  x |> basename() |> str_extract("^\\d+")
}

paths_party <-
  fs::dir_ls(here("data/gapminder_party")) |>
  as.list() |>
  set_names(get_year) |>
  print()

# modify read-function to return NULL, rather than throw error
poss_read_excel <- possibly(read_excel, otherwise = NULL, quiet = FALSE)

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

## Implement `list_rbind()` ourselves

# keep only non-null elements
# set list-names as column `year`
# bind into single data-frame

data_horrible <-
  paths_party |>
  map(poss_read_excel) |>
  keep(negate(is.null)) |>
  imap(\(df, name) mutate(df, "year" := parse_number(name))) |>
  reduce(rbind) |>
  print()
