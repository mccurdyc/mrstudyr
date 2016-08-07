#' FUNCTION: create_mutation_score_graphs
#'
#' This function will call the necessary function to create the mutation score graphs
#' @export

create_mutation_score_graphs <- function() {

  # read the sqlite-avmdefaults data and specifically filter for the schemas described in the paper
  # if you want to analyse different data, call a different read function
  d <- read_sqlite_avmdefaults() %>% collect_study_schemas()

  # 1a. Perform RANDOM SAMPLING
  random_sampling_data <- d %>% analyse_random_sampling()
  dplyr::glimpse(random_sampling_data)

  # 1b. Perform OPERATOR SAMPLING
  operator_sampling_data <- d %>% analyse_across_operators()
  dplyr::glimpse(operator_sampling_data)
}
