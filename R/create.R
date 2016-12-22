#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualizations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {

  d <- read_sqlite_avmdefaults() %>% collect_normal_data()
  random_sampling_data <- d %>% analyze_random_sampling()
  # random_sampling_calculations <- random_sampling_data %>% analyze_percent_calculations()
  return(random_sampling_data)
  # return(random_sampling_calculations)
}

#' FUNCTION: create_selective_random_mutation_graphs
#'
#' Create all of the visualizations associated with performing random sampling and selective mutation
#' (selecting a set of operators). In other words, perform random sampling over a select set of operators.
#' @export

create_selective_random_mutation_graphs <- function() {

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
  selective_random_mutation_data <- d %>% analyze_selective_random_mutation(o)
  selective_random_mutation_calculations <- selective_random_mutation_data %>% analyze_percent_calculations()
  dplyr::glimpse(selective_random_mutation_calculations)
}
