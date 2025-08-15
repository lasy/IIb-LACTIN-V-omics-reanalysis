complete_cases <- function(inputs){
  keys <- map(inputs, ~.x |> rownames()) |> purrr::reduce(.f = intersect)
  inputs <- map(inputs, ~.x[keys,])
  inputs
}