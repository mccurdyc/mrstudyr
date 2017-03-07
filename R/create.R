#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualizations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  a <- d %>% analyze_random_sampling()
  # will probably need to rename this (it currently only adds error calculations)
  b <- a %>% calculate_effectiveness(p=TRUE)

  # visualize_random_sampling_mutation_scores(a)
  # visualize_random_sampling_error(a)
  # visualize_random_sampling_mae(b)
  # visualize_random_sampling_rmse(b)
  return(b)
}

#' FUNCTION: create_selective_random_graphs
#'
#' Create all of the visualizations associated with performing random sampling and selective mutation
#' (selecting a set of operators). In other words, perform random sampling over a select set of operators.
#' @export

create_selective_random_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  o <- c("FKCColumnPairE",
         # "NNCA",
         # "UCColumnA",
         # "FKCColumnPairR",
         # "PKCColumnA",
         # "PKCColumnR",
         "PKCColumnE",
         "NNCR",
         "CCNullifier",
         # "CCRelationalExpressionOperatorE",
         # "UCColumnR",
         "UCColumnE",
         "CCInExpressionRHSListExpressionElementR")
  a <- d %>% analyze_selective_random(o)
  b <- a %>% calculate_effectiveness(p=TRUE)

  # visualize_selective_random_mutation_scores(a)
  # visualize_selective_random_error(a)
  # visualize_random_sampling_mae(b)
  # visualize_random_sampling_rmse(b)
  return(b)
}

# #' FUNCTION: create_incremental_graphs
# #'
# #' Create all of the visualizations associated with the incremental reduction technique
# #' @export
#
# create_incremental_graphs <- function() {
#
#   d <- read_sqlite_avmdefaults() %>% collect_normal_data()
#   a <- d %>% analyze_incremental()
#   # b <- a %>% calculate_effectiveness()
#   dplyr::glimpse(a)
# }

#' FUNCTION: create_incremental_across_schema_graphs
#'
#' Create all of the visualizations associated with the incremental reduction technique using a model
#' from across-schema reduction.
#' @export

create_incremental_across_schema_graphs <- function() {
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  dt <- d %>% analyze_incremental_across_schemas(step_size=0.1, corr_threshold=0.05, cost_threshold=0.09)
  # dt <- dt %>% calculate_per_trial_effectiveness()
  return(dt)
}
