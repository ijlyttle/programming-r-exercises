library("purrr")

## Fundamental paradigms
num <- 1:4

num |> map(\(x) x + 1)

num |> keep(\(x) x %% 2 == 0)

num |> reduce(\(acc, x) acc + x)
