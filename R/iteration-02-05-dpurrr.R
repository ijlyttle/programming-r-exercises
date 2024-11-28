## dplyr using purrr (if time permits)

dpurrr_filter <- function(df, predicate) {
  df |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE) |>
    purrr::keep(predicate) |>
    purrr::list_transpose() |>
    as.data.frame()
}

dpurrr_filter(mtcars, \(d) d$gear == 3) |> head()

dpurrr_mutate <- function(df, mapper) {
  df |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE) |>
    purrr::map(\(d) c(d, mapper(d))) |>
    purrr::list_transpose() |>
    as.data.frame()
}

mtcars |>
  dpurrr_mutate(\(d) list(wt_kg = d$wt * 1000 / 2.2)) |>
  head()

dpurrr_summarise <- function(df, reducer, .init) {
  df |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE) |>
    purrr::reduce(reducer, .init = .init) |>
    as.data.frame()
}

mtcars |>
  dpurrr_summarise(
    reducer = \(acc, val) list(
      wt_min = min(acc$wt_min, val$wt),
      wt_max = max(acc$wt_max, val$wt)
    ),
    .init = list(wt_min = Inf, wt_max = -Inf)
  )

## With grouping

ireduce <- function(x, reducer, .init) {
  purrr::reduce2(x, names(x), reducer, .init = .init)
}

summariser <- purrr::partial(
  dpurrr_summarise,
  reducer = \(acc, val) list(
    wt_min = min(acc$wt_min, val$wt),
    wt_max = max(acc$wt_max, val$wt)
  ),
  .init = list(wt_min = Inf, wt_max = -Inf)
)

mtcars |>
  split(mtcars$gear) |>
  purrr::map(summariser) |>
  ireduce(
    reducer = \(acc, x, y) rbind(acc, c(list(gear = y), x)),
    .init = data.frame()
  )
