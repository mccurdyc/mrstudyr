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

#' FUNCTION: summarize_operator_percentage_kept
#'
#' Calculate the percentage of mutants that are kept for each operator.
#' @export

summarize_operator_percentage_kept <- function(d) {
  dt <- d %>% dplyr::summarise(operator_count = n()) %>%
    dplyr::summarise(keep_count = collect_ignore_data(d))
  dt <- d %>% collect_trial_operator_data() %>% dplyr::summarise(percentage_kept = count(time))
  return(dt)
}
