## dplyr using purrr (if time permits)

library("conflicted")
library("palmerpenguins")
library("dplyr")
library("purrr")

# simplify penguins (Sorry Allison!)
penguins_local <-
  penguins |>
  mutate(across(where(is.factor), as.character)) |> # use strings, not factors
  select(species, island, body_mass_g, sex) |>      # fewer columns
  print()

#' @param .data  data frame or tibble
#' @return unnamed list of named lists
dpurrr_to_list <- function(.data) {
  .data |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE)
}

#' @param .x  unnamed list of named lists
#' @return tibble
dpurrr_to_tibble <- function(.x) {
  .x |>
    purrr::list_transpose() |>
    tibble::as_tibble()
}

# experiment with helpers
penguins_local |>
  head(2) |>
  dpurrr_to_list() |>
  # dpurrr_to_tibble() |>
  str()

# filter can be a purrr::keep()
penguins_local |>
  dpurrr_to_list() |>
  keep(\(d) d$sex == "female" && !is.na(d$sex)) |>
  dpurrr_to_tibble() |>
  print()

# mutate is purrr::map(), with a little extra to keep current elements of list
dpurrr_mutate <- function(.x, mapper) {
  .x |> purrr::map(\(d) modifyList(d, mapper(d)))
}

penguins |>
  dpurrr_to_list() |>
  dpurrr_mutate(\(d) list(body_mass_kg = d$body_mass_g / 1000)) |>
  dpurrr_to_tibble() |>
  print()

# summarise is reduce, but result wrapped in a list
dpurrr_summarise <- function(.x, reducer) {
  .x |> purrr::reduce(reducer) |> list()
}

penguins |>
  dpurrr_to_list() |>
  dpurrr_summarise(
    \(acc, d) list(
      body_mass_g_min = min(acc$body_mass_g_min, d$body_mass_g, na.rm = TRUE),
      body_mass_g_max = max(acc$body_mass_g_max, d$body_mass_g, na.rm = TRUE)
    )
  ) |>
  dpurrr_to_tibble() |>
  print()

# extract the reducer-function to make things more concise
body_mass_g_min_max <- function(acc, d) {
  list(
    body_mass_g_min = min(acc$body_mass_g_min, d$body_mass_g, na.rm = TRUE),
    body_mass_g_max = max(acc$body_mass_g_max, d$body_mass_g, na.rm = TRUE)
  )
}

penguins |>
  dpurrr_to_list() |>
  dpurrr_summarise(body_mass_g_min_max) |>
  dpurrr_to_tibble() |>
  print()

# add in a group-by
penguins |>
  split(penguins$species) |>
  imap(
    function(.data, name) {
      .data |>
        dpurrr_to_list() |>
        dpurrr_summarise(body_mass_g_min_max) |>
        dpurrr_mutate(\(d) list(species = name)) |>
        dpurrr_to_tibble()
    }
  ) |>
  reduce(rbind) |>
  print()






