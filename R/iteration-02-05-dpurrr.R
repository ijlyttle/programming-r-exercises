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

#' @param .data data frame or tibble
#' @return unnamed list of named lists, i.e. transposed data frame
dpurrr_to_list <- function(.data) {
  .data |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE)
}

#' @param .d unnamed list of named lists, i.e. transposed data frame
#' @return tibble
dpurrr_to_tibble <- function(.d) {
  .d |>
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

#' @param .d unnamed list of named lists, i.e. transposed data frame
#' @param mapper function applied to each member of `.d`
#' @return unnamed list of named lists, i.e. transposed data frame
dpurrr_mutate <- function(.d, mapper) {
  # modifyList() used to keep current elements
  .d |> purrr::map(\(d) modifyList(d, mapper(d)))
}

penguins_local |>
  dpurrr_to_list() |>
  dpurrr_mutate(\(d) list(body_mass_kg = d$body_mass_g / 1000)) |>
  dpurrr_to_tibble() |>
  print()

#' @param .d unnamed list of named lists, i.e. transposed data frame
#' @param reducer function applied accumulator and to each member of `.d`
#' @param .init initial value of accumulator, if empty: first element of `.d`
#' @param ... other arguments passed to `purrr::reduce()`
#' @return unnamed list of named lists, i.e. transposed data frame
dpurrr_summarise <- function(.d, reducer, .init, ...) {
  # wrap result in a list, to return a transposed data frame
  .d |> purrr::reduce(reducer, .init = .init, ...) |> list()
}

penguins_local |>
  dpurrr_to_list() |>
  dpurrr_summarise(
    \(acc, d) list(
      body_mass_g_min = min(acc$body_mass_g_min, d$body_mass_g, na.rm = TRUE),
      body_mass_g_max = max(acc$body_mass_g_max, d$body_mass_g, na.rm = TRUE)
    )
  ) |>
  dpurrr_to_tibble() |>
  print()

penguins_local |>
  dpurrr_to_list() |>
  dpurrr_summarise(
    \(acc, d) list(count = acc$count + 1),
    .init = list(count = 0)
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

penguins_local |>
  dpurrr_to_list() |>
  dpurrr_summarise(body_mass_g_min_max) |>
  dpurrr_to_tibble() |>
  print()

#' @param .d unnamed list of named lists, i.e. transposed data frame
#' @param name string, name of variable on which to split
#' @return named list of transposed data frames, names: values of split variable
dpurrr_split <- function(.d, name) {

  # get unique values of .d[[name]]
  d_name <- .d |> purrr::map(\(d) d[[name]]) |> unique()
  names(d_name) <- d_name

  # for each element of name, a collection of .d rows that "contain" the name
  d_name |>
    purrr::map(\(x) .d |> purrr::keep(\(d) d[[name]] == x))
}

penguins_local |>
  dpurrr_to_list() |>
  dpurrr_split("species") |>
  imap(
    function(.d, name) {
      .d |>
        dpurrr_summarise(body_mass_g_min_max) |>
        dpurrr_mutate(\(d) list(species = name))
    }
  ) |>
  reduce(c) |>
  dpurrr_to_tibble() |>
  print()

penguins_local |>
  dpurrr_to_list() |>
  dpurrr_split("species") |>
  imap(
    function(.d, name) {
      .d |>
        dpurrr_summarise(
          \(acc, d) list(count = acc$count + 1),
          .init = list(count = 0)
        ) |>
        dpurrr_mutate(\(d) list(species = name))
    }
  ) |>
  reduce(c) |>
  dpurrr_to_tibble() |>
  print()






