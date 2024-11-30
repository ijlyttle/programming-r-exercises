## dplyr using purrr (if time permits)

library("purrr")

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

mtcars |>
  dpurrr_to_list() |>
  keep(\(d) d$gear == 3) |>
  dpurrr_to_tibble() |>
  print()

dpurrr_mutate <- function(.x, mapper) {
  .x |> purrr::map(\(d) c(d, mapper(d)))
}

mtcars |>
  dpurrr_to_list() |>
  dpurrr_mutate(\(d) list(wt_kg = d$wt * 1000 / 2.2)) |>
  dpurrr_to_tibble() |>
  print()

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






