#' FUNCTION: summarize_reduced_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants.
#' @export

summarize_reduced_time <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::summarise(reduced_time = sum(time))
  return(dt)
}

#' FUNCTION: summarize_original_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants.
#' @export

summarize_original_time <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::summarise(original_time = sum(time))
  return(dt)
}
