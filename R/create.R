#' FUNCTION: create_data_overview_graphs
#'
#' Create visualizations associated just with the original data prior to reduction techniques.
#' @export

create_data_overview_graphs <- function() {
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  da <- d %>% calculate_fractional_operator_costs()
  db <- d %>% calculate_per_schema_fractional_operator_costs()
  visualize_fractional_operator_mutant_costs(da)
  visualize_fractional_operator_mutant_costs_per_schema(db)
}

#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualizations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  da <- d %>% analyze_random_sampling()
  db <- da %>% calculate_per_trial_percentage_effectiveness()
  visualize_random_sampling_correlation(db)
  visualize_random_sampling_cost_reduction(db)
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
  step_size_small <- 0.1
  step_size_medium <- 0.2
  step_size_large <- 0.3
  ts <- d %>% analyze_incremental_across_schemas(step_size_small, 0.05, 0.09) %>% calculate_per_trial_effectiveness()
  tm <- d %>% analyze_incremental_across_schemas(step_size_medium, 0.05, 0.19) %>% calculate_per_trial_effectiveness()
  tl <- d %>% analyze_incremental_across_schemas(step_size_large, 0.05, 0.29) %>% calculate_per_trial_effectiveness()
  dt <- join_hill_climbing_size_data(ts, tm, tl, (step_size_small*100), (step_size_medium*100), (step_size_large*100))
  return(dt)
}
