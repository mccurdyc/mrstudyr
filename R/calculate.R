#' FUNCTION: calculate_per_trial_percentage_effectiveness
#'
#' Calculate the effectiveness of a reduction technique on a per-trial, per-percentage basis.
#' @export

calculate_per_trial_percentage_effectiveness <- function(d) {
  d <- d %>% dplyr::ungroup() %>% collect_trial_data()
  ds <- split(d, list(d$dbms, d$trial, d$percentage))
  dt <- ds %>% parallel::mclapply(transform_add_correlation) %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  return(dt)
}

#' FUNCTION: calculate_per_trial_effectiveness
#'
#' Calculate the effectiveness of a reduction technique on a per-trial basis.
#' @export

calculate_per_trial_effectiveness <- function(d) {
  d <- d %>% collect_trial_data()
  ds <- split(d, d$trial)
  dt <- ds %>% parallel::mclapply(transform_add_correlation) %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  return(dt)
}

#' FUNCTION: calculate_fractional_operator_costs
#'
#' Calculate the fractional costs of mutants per operator.
#' @export

calculate_fractional_operator_costs <- function(d) {
  da <- d %>% summarize_operator_time()
  db <- d %>% summarize_time()
  dc <- join_operator(da, db)
  dt <- dc %>% transform_fractional_operator_cost()
  return(dt)
}

#' FUNCTION: calculate_per_schema_fractional_operator_costs
#'
#' Calculate the fractional costs of mutants per operator on a per-schema basis.
#' @export

calculate_per_schema_fractional_operator_costs <- function(d) {
  da <- d %>% summarize_schema_operator_time()
  db <- d %>% summarize_original_time()
  dc <- join_schema_operator(da, db)
  dt <- dc %>% transform_fractional_operator_cost()
  return(dt)
}

#' FUNCTION: calculate_fractional_operator_frequencies
#'
#' Calculate the fractional frequencies of mutants per operator.
#' @export

calculate_fractional_operator_frequencies <- function(d) {
  da <- d %>% summarize_operator_frequencies()
  db <- d %>% summarize_count()
  dc <- join_operator(da, db)
  dt <- dc %>% transform_fractional_operator_frequencies()
  return(dt)
}

#' FUNCTION: calculate_per_schema_fractional_operator_frequencies
#'
#' Calculate the fractional frequencies of mutants per operator on a per-schema basis.
#' @export

calculate_per_schema_fractional_operator_frequencies <- function(d) {
  da <- d %>% summarize_schema_operator_frequencies()
  db <- d %>% summarize_schema_count()
  dc <- join_schema_operator(da, db)
  dt <- dc %>% transform_fractional_operator_frequencies()
  return(dt)
}

#' FUNCTION: calculate_correlation
#'
#' This function will calculate the correlation between the reduced and the original
#' mutation score for a given percent. This is a helper function for the transform_correlation function
#' @export

calculate_correlation <- function(d) {
  x <- d[['reduced_mutation_score']]
  y <- d[['original_mutation_score']]

  model <- cor.test(x, y, method = "kendall", use = "pairwise")
  dt <- model %>% broom::tidy() %>% transform_replace_correlation()
  return(dt[['correlation']]) # return just correlation
}

#' FUNCTION: calculate_neighborhood_size
#'
#' This function extracts the size of the neighborhood from the data.
#' @export

calculate_neighborhood_size <- function(d) {
  dt <- (d$mutant_count[[1]] / d$step_size[[1]]) %>% round()
  return(dt)
}

#' FUNCTION: calculate_per_dbms_mutation_scores
#'
#' In an attempt to explain why RS is so effective, calculate the mutation scores of data for each dbms.
#' @export

calculate_per_dbms_mutation_scores <- function(d) {
  dt <- d %>% evaluate_original_data()
  return(dt)
}
