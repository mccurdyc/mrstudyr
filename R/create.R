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
  rs_calculations <- random_sampling_data %>% analyse_calculations()
  dplyr::glimpse(rs_calculations)

  # 1b. Perform OPERATOR SAMPLING
  # operator_sampling_data <- d %>% analyse_across_operators()
  # os_calculations <- operator_sampling_data %>% analyse_calculations()
  # dplyr::glimpse(operator_sampling_data)
}
