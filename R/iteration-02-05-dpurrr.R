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
  dpurrr_to_tibble()

mtcars |>
  dpurrr_to_list() |>
  reduce(
    \(acc, val) list(
      wt_min = min(acc$wt_min, val$wt),
      wt_max = max(acc$wt_max, val$wt)
    ),
    .init = list(wt_min = Inf, wt_max = -Inf)
  ) |>
  list() |>
  dpurrr_to_tibble()

mtcars |>
  split(mtcars$gear) |>
  map(
    \(.data) {
      .data |>
        dpurrr_to_list() |>
        reduce(
          \(acc, val) list(
            wt_min = min(acc$wt_min, val$wt),
            wt_max = max(acc$wt_max, val$wt)
          ),
          .init = list(wt_min = Inf, wt_max = -Inf)
        ) |>
        list() |>
        dpurrr_to_tibble()
    }
  ) |>
  imap(
    \(.data, name) {
      .data |>
        dpurrr_to_list() |>
        dpurrr_mutate(\(d) list(gear = as.integer(name))) |>
        dpurrr_to_data_frame()
    }
  ) |>
  reduce(rbind) |>
  print()






