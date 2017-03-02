#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualizations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  a <- d %>% analyze_random_sampling()
  # will probably need to rename this (it currently only adds error calculations)
  b <- a %>% calculate_effectiveness(p=TRUE)
  dplyr::glimpse(b)

  # visualize_random_sampling_mutation_scores(a)
  # visualize_random_sampling_error(a)
  # visualize_random_sampling_mae(b)
  # visualize_random_sampling_rmse(b)
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
         "PKCColumnA",
         "PKCColumnR",
         "PKCColumnE",
         # "NNCR",
         # "CCNullifier",
         # "CCRelationalExpressionOperatorE",
         # "UCColumnR",
         "UCColumnE",
         "CCInExpressionRHSListExpressionElementR")
  a <- d %>% analyze_selective_random(o)
  b <- a %>% calculate_effectiveness(p=TRUE)
  dplyr::glimpse(b)

  # visualize_selective_random_mutation_scores(a)
  # visualize_selective_random_error(a)
  # visualize_random_sampling_mae(b)
  # visualize_random_sampling_rmse(b)
}

#' FUNCTION: create_incremental_graphs
#'
#' Create all of the visualizations associated with the incremental reduction technique
#' @export

create_incremental_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  a <- d %>% analyze_incremental()
  # b <- a %>% calculate_effectiveness()
  dplyr::glimpse(a)
}

#' FUNCTION: create_incremental_across_schema_graphs
#'
#' Create all of the visualizations associated with the incremental reduction technique using a model
#' from across-schema reduction.
#' @export

create_incremental_across_schema_graphs <- function() {
  dcd <- data.frame()
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  schemas <- d %>% select_all_schemas()
  for (s in schemas[[1]]) {
    print(paste("current excluded schema: ", s))
    ds <- d %>% exclude_schema(s)
    excluded_schema <- d %>% select_schema_data(s)
    # current best correlation in the hill-climbing algorithm cannot be worse than 5% lower than previous best
    # while cost must be reduced by more than 9% than previous cost reduction
    small <- ds %>% analyze_incremental_across_schemas(step_size=0.1, corr_threshold=0.05, cost_threshold=0.09)
    # medium <- ds %>% analyze_incremental_across_schemas(step_size=0.2, corr_threshold=0.05, cost_threshold=0.09)
    # large <- ds %>% analyze_incremental_across_schemas(step_size=0.4, corr_threshold=0.05, cost_threshold=0.09)
    model <- small %>% generate_operator_model()
    # 30 times to account for randomness
    for (j in 1:30) {
      dt <- excluded_schema %>% apply_operator_model(model) %>% transform_add_trial(j) %>% as.data.frame()
      dcd <- rbind(dcd, dt)
      dcd %>% dplyr::glimpse()
    }
  }
  return(dcd)
}
