#' FUNCTION: analyse_random_sampling
#'
#' This function will perform random sampling.
#' @export

analyse_random_sampling <- function(d) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()
  # df <- data.frame("dbms" = character(),
  #                  "schema" = character(),
  #                  "reduced_numerator" = integer(),
  #                  "reduced_denominator" = integer(),
  #                  "original_numerator" = integer(),
  #                  "original_denominator" = integer(),
  #                  "reduced_time" = integer(),
  #                  "original_time" = integer(),
  #                  "cost_reduction" = double(),
  #                  "reduced_mutation_score" = double(),
  #                  "original_mutation_score" = double(),
  #                  "percentage" = integer(),
  #                  "trial" = integer())

  for(i in percentages) {
    print(paste("Currently analysing x =", (i*100), "percent ..."))
    for(j in 1:30) {
      dt <- random_sampling(d, i, j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: random_sampling
#'
#' Perform random sampling
#' @export

random_sampling <- function(d, i, j) {
  original_data <- d %>% dplyr::group_by(dbms, schema)
  random_sample_data <- original_data %>% select_x_percent(i)
  reduced_numerator <- random_sample_data %>% transform_reduced_killed_count()
  reduced_denominator <- random_sample_data %>% transform_reduced_total_count()
  original_numerator <-  original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- random_sample_data %>% summarise_reduced_time()
  original_time <- original_data %>% summarise_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}
