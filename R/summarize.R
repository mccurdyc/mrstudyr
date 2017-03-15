#' FUNCTION: summarize_reduced_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants.
#' @export

summarize_reduced_time <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::summarize(reduced_time = sum(time))
  return(dt)
}

#' FUNCTION: summarize_original_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants.
#' @export

summarize_original_time <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::summarize(original_time = sum(time))
  return(dt)
}

#' FUNCTION: summarize_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants.
#' @export

summarize_time <- function(d) {
  dt <- d %>% dplyr::group_by(dbms) %>% dplyr::summarize(original_time = sum(time))
  return(dt)
}

#' FUNCTION: summarize_operator_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants
#' on a per-operator basis.
#' @export

summarize_operator_time <- function(d) {
  dt <- d %>% collect_operator_data() %>% dplyr::summarize(operator_time = sum(time))
  return(dt)
}

#' FUNCTION: summarize_schema_operator_time
#'
#' Calculate the total time (in ms) of performing mutation analysis with the given set of mutants
#' on a per-schema, per-operator basis.
#' @export

summarize_schema_operator_time <- function(d) {
  dt <- d %>% collect_schema_operator_data() %>% dplyr::summarize(operator_time = sum(time))
  return(dt)
}

#' FUNCTION: summarize_count
#'
#' Calculate the total number of mutants per dbms.
#' @export

summarize_count <- function(d) {
  dt <- d %>% dplyr::group_by(dbms) %>% dplyr::summarize(count = n())
  return(dt)
}

#' FUNCTION: summarize_schema_count
#'
#' Calculate the total number of mutants per dbms, per schema.
#' @export

summarize_schema_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::summarize(count = n())
  return(dt)
}

#' FUNCTION: summarize_operator_frequencies
#'
#' Count the frequencies per operator.
#' @export

summarize_operator_frequencies <- function(d) {
  dt <- d %>% collect_operator_data() %>% dplyr::summarize(operator_frequencies = n())
  return(dt)
}

#' FUNCTION: summarize_schema_operator_frequencies
#'
#' Count the frequencies per operator on a per-schema basis.
#' @export

summarize_schema_operator_frequencies <- function(d) {
  dt <- d %>% collect_schema_operator_data() %>% dplyr::summarize(operator_frequencies = n())
  return(dt)
}

#' FUNCTION: summarize_operator_percentage_kept
#'
#' Calculate the percentage of mutants that are kept for each operator.
#' @export

summarize_operator_percentage_kept <- function(d) {
  dt <- d %>% dplyr::summarize(operator_count = n()) %>%
    dplyr::summarize(keep_count = collect_ignore_data(d))
  dt <- d %>% collect_trial_operator_data() %>% dplyr::summarize(percentage_kept = count(time))
  return(dt)
}

#' FUNCTION: summarize_average_percent_kept
#'
#' Calculate the average percent of mutants kept per-operator across 30 trials.
# @export

summarize_average_percent_kept <- function(d) {
  dt <- d %>% dplyr::summarize(average_percent_kept = mean(percent_kept))
  return(dt)
}

#' FUNCTION: summarize_average_percent_ignored
#'
#' Calculate the average percent of mutants kept per-operator across 30 trials.
# @export

summarize_average_percent_ignored <- function(d) {
  dt <- d %>% dplyr::summarize(average_percent_ignored = mean(percent_ignored))
  return(dt)
}
