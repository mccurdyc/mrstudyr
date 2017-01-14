#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualizations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  random_sampling_data <- d %>% analyze_random_sampling()
  # will probably need to rename this (it currently only adds error calculations)
  random_sampling_calculations <- random_sampling_data %>% calculate_percent_effectiveness()
  dplyr::glimpse(random_sampling_calculations)

  visualize_random_sampling_mutation_scores(random_sampling_data)
  visualize_random_sampling_error(random_sampling_data)
  visualize_random_sampling_mae(random_sampling_calculations)
  visualize_random_sampling_rmse(random_sampling_calculations)
}

#' FUNCTION: create_selective_random_graphs
#'
#' Create all of the visualizations associated with performing random sampling and selective mutation
#' (selecting a set of operators). In other words, perform random sampling over a select set of operators.
#' @export

create_selective_random_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  o <- c("FKCColumnPairE", "NNCA", "UCColumnA", "FKCColumnPairR", "PKCColumnA", "PKCColumnR", "PKCColumnE", "NNCR", "CCNullifier", "CCRelationalExpressionOperatorE", "UCColumnR", "UCColumnE", "CCInExpressionRHSListExpressionElementR")
  selective_random_data <- d %>% analyze_selective_random(o)
  selective_random_calculations <- selective_random_data %>% calculate_percent_effectiveness()
  dplyr::glimpse(selective_random_calculations)

  visualize_selective_random_mutation_scores(selective_random_data)
  visualize_selective_random_error(selective_random_data)
  visualize_random_sampling_mae(selective_random_calculations)
  visualize_random_sampling_rmse(selective_random_calculations)
}
