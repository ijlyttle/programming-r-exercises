## dplyr using purrr (if time permits)

library("purrr")

# start with a couple of helpers
dpurrr_to_list <- function(.data) {
  .data |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE)
}

dpurrr_to_tibble <- function(.x) {
  .x |>
    purrr::list_transpose() |>
    tibble::as_tibble()
}

# filter can be a purrr::keep()
mtcars |>
  dpurrr_to_list() |>
  keep(\(d) d$gear == 3) |>
  dpurrr_to_tibble() |>
  print()

# mutate is purrr::map(), with a little extra to keep current elements of list
dpurrr_mutate <- function(.x, mapper) {
  .x |> purrr::map(\(d) modifyList(d, mapper(d)))
}

mtcars |>
  dpurrr_to_list() |>
  dpurrr_mutate(\(d) list(wt_kg = d$wt * 1000 / 2.2)) |>
  dpurrr_to_tibble() |>
  print()

# summarise is reduce, but result wrapped in a list
dpurrr_summarise <- function(.x, reducer) {
  .x |> purrr::reduce(reducer) |> list()
}

mtcars |>
  dpurrr_to_list() |>
  dpurrr_summarise(
    \(acc, d) list(
      wt_min = min(acc$wt_min, d$wt),
      wt_max = max(acc$wt_max, d$wt)
    )
  ) |>
  dpurrr_to_tibble() |>
  print()

# extract the reducer-function to make things more concise
wt_min_max <- function(acc, d) {
  list(
    wt_min = min(acc$wt_min, d$wt),
    wt_max = max(acc$wt_max, d$wt)
  )
}

mtcars |>
  dpurrr_to_list() |>
  dpurrr_summarise(wt_min_max) |>
  dpurrr_to_tibble() |>
  print()

# add in a group-by
mtcars |>
  split(mtcars$gear) |>
  imap(
    function(.x, name) {
      .x |>
        dpurrr_to_list() |>
        dpurrr_summarise(wt_min_max) |>
        dpurrr_mutate(\(d) list(gear = as.integer(name))) |>
        dpurrr_to_tibble()
    }
  ) |>
  reduce(rbind) |>
  print()






