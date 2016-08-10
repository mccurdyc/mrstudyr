#' FUNCTION: create_random_sampling_graphs
#'
#' Create all of the visualisations associated with the random sampling reduction technique
#' @export

create_random_sampling_graphs <- function() {

  # read the sqlite-avmdefaults data and specifically filter for the schemas described in the paper
  # if you want to analyse different data, call a different read function
  d <- read_sqlite_avmdefaults() %>% collect_study_schemas()

  random_sampling_data <- d %>% analyse_random_sampling()
  filtered_percents_data <- random_sampling_data %>% collect_chosen_percent_data(c(1, 10, 20, 40))
  visualise_random_sampling_mutation_scores(filtered_percents_data)

  # calculate Kendall's Tau_B correlation coefficient, MAE, RMSE
  random_sampling_calculations <- random_sampling_data %>% analyse_calculations()
  dplyr::glimpse(random_sampling_calculations)
}

#' FUNCTION: create_operator_sampling_graphs
#'
#' Create all of the visualisations associated with the operator sampling reduction technique
#' @export

create_operator_sampling_graphs <- function() {

  # read the sqlite-avmdefaults data and specifically filter for the schemas described in the paper
  # if you want to analyse different data, call a different read function
  d <- read_sqlite_avmdefaults() %>% collect_study_schemas()

  operator_sampling_data <- d %>% analyse_across_operators()
  filtered_percents_data <- random_sampling_data %>% collect_chosen_percent_data(c(1, 10, 20, 40))
  visualise_operator_sampling_mutation_scores(filtered_percents_data)

  # calculate Kendall's Tau_B correlation coefficient, MAE, RMSE
  operator_sampling_calculations <- operator_sampling_data %>% analyse_calculations()
  dplyr::glimpse(operator_sampling_calculations)
}
