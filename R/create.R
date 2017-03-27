#' FUNCTION: create_data_overview_graphs
#'
#' Create visualizations associated just with the original data prior to reduction techniques.
#' @export

create_data_overview_graphs <- function() {
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  dca <- d %>% calculate_fractional_operator_costs()
  dcb <- d %>% calculate_per_schema_fractional_operator_costs()
  dfa <- d %>% calculate_fractional_operator_frequencies()
  dfb <- d %>% calculate_per_schema_fractional_operator_frequencies()
  visualize_fractional_operator_mutant_costs(dca)
  visualize_fractional_operator_mutant_costs_per_schema(dcb)
  visualize_fractional_operator_mutant_frequencies(dfa)
  visualize_fractional_operator_mutant_frequencies_per_schema(dfb)
}

#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualizations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  da <- d %>% analyze_random_sampling()
  db <- da %>% calculate_per_trial_percentage_effectiveness() %>% transform_add_technique("random sampling")
  visualize_random_sampling_correlation(db)
  visualize_random_sampling_cost_reduction(db)
}

#' FUNCTION: create_selective_random_graphs
#'
#' Create all of the visualizations associated with performing random sampling and selective mutation
#' (selecting a set of operators). In other words, perform random sampling over a select set of operators.
#' @export

create_selective_random_graphs <- function() {
  df <- data.frame()
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  o <- c("FKCColumnPairE",
         "NNCA",
         "UCColumnA",
         "FKCColumnPairR",
         "PKCColumnA",
         "PKCColumnR",
         "PKCColumnE",
         "NNCR",
         "CCNullifier",
         "CCRelationalExpressionOperatorE",
         "UCColumnR",
         "UCColumnE",
         "CCInExpressionRHSListExpressionElementR")
  # remove groups of one of two operators
  for (i in 1:length(o)) {
    for (j in 1:length(o)) {
      # data after removing the operator(s)
      print(paste("Removed: ", o[i], " ", o[j]))
      r <- d %>% remove_operators(o[i]) %>% remove_operators(o[j])
      # operator list after removing chosen operators
      ro <- r %>% dplyr::ungroup() %>% select_all_operators() %>% as.vector()
      # print(ro) # debugging
      # perform the random sampling, per operator, per schema
      da <- r %>% analyze_selective_random(ro)
      db <- da %>% calculate_per_trial_percentage_effectiveness() %>% transform_add_omitted_operators(o[i], o[j]) %>%
        transform_add_technique("selective random sampling")
      db %>% dplyr::glimpse()
      df <- rbind(df, db)
    }
  }
  path <- ("selective.feather")
  feather::write_feather(df, path)
  return(df)
}

#' FUNCTION: create_selective_mutation_graphs
#'
#' Create all of the visualizations associated with performing selective mutation
#' (selecting a set of operators).
#' @export

create_selective_mutation_graphs <- function() {
  df <- data.frame()
  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  o <- c("FKCColumnPairE",
         "NNCA",
         "UCColumnA",
         "FKCColumnPairR",
         "PKCColumnA",
         "PKCColumnR",
         "PKCColumnE",
         "NNCR",
         "CCNullifier",
         "CCRelationalExpressionOperatorE",
         "UCColumnR",
         "UCColumnE",
         "CCInExpressionRHSListExpressionElementR")
  # remove groups of one of two operators
  for (i in 1:length(o)) {
    for (j in 1:length(o)) {
      # data after removing the operator(s)
      print(paste("Removed: ", o[i], " ", o[j]))
      r <- d %>% remove_operators(o[i]) %>% remove_operators(o[j])
      # operator list after removing chosen operators
        # ro <- r %>% dplyr::ungroup() %>% select_all_operators() %>% as.vector()
        # print(ro) # debugging
      # perform the random sampling, per operator, per schema
      da <- evaluate_reduction_technique(d, r) %>%
        transform_add_percentage_trial(100, 1) %>% as.data.frame()
      db <- da %>% calculate_per_trial_percentage_effectiveness() %>% transform_add_omitted_operators(o[i], o[j]) %>%
        transform_add_technique("selective mutation")
      db %>% dplyr::glimpse()
      df <- rbind(df, db)
    }
  }
  path <- ("selectivemutation.feather")
  feather::write_feather(df, path)
  return(df)
}

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
  dt <- join_hill_climbing_size_data(ts, tm, tl, (step_size_small*100), (step_size_medium*100), (step_size_large*100)) %>%
    transform_add_technique("hill climbing")
}

#' FUNCTION: create_pairwise_wilcoxon_rank_sum_test
#'
#' Create the graphs associated with the pairwise Wilcoxon ranked-sum test.
#' The input to this function is the combined technique data.
#' @export

create_pairwise_wilcoxon_rank_sum_test <- function(d) {
  dt <- d %>% perform_pairwise_wilcoxon_rank_sum_test()
}

#' FUNCTION: create_effect_size_test
#'
#' Create the graphs associated with the pairwise effect size calculations.
#' @export

create_effect_size_test <- function(d) {
}
#'
#' Create the graphs associated with the pairwise Wilcoxon ranked-sum test.
#' The input to this function is the combined technique data.
#' @export

create_pairwise_wilcoxon_rank_sum_test <- function(d) {
  dt <- d %>% perform_pairwise_wilcoxon_rank_sum_test()
}
