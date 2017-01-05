#' FUNCTION: analyze_random_sampling
#'
#' This function will perform random sampling.
#' @export

analyze_random_sampling <- function(d) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  for(i in percentages) {
    print(paste("RANDOM SAMPLING: Currently analyzing x =", (i*100), "percent ..."))
    for(j in 1:30) {
      dt <- random_sampling(d, i, j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: random_sampling
#'
#' Perform random sampling, also referred to as uniform random sampling
#' @export

random_sampling <- function(d, i, j) {
  original_data <- d %>% collect_schema_data()
  random_sample_data <- original_data %>% select_x_percent(i)
  reduced_numerator <- random_sample_data %>% transform_reduced_killed_count()
  reduced_denominator <- random_sample_data %>% transform_reduced_total_count()
  original_numerator <-  original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- random_sample_data %>% summarize_reduced_time()
  original_time <- original_data %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_error() %>%
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_selective_random
#'
#' This function will analyze a select set of operators over percentages
#' This function can also be used to perform traditional selective mutation, i.e. mutation using a
#' reduced set of operators by choosing a select set and analyzing at 100%.
#' @export

analyze_selective_random <- function(d, o) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  print("OPERATORS SELECTED:")
  print(o)
  for(i in percentages) {
    print(paste("SELECTIVE RANDOM: Currently analyzing x =", (i*100), "percent ..."))
    for(j in 1:30) {
      dt <- selective_random(d, o, i, j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: selective_random
#'
#' Perform selective mutation with random sampling per set
#' @export

selective_random <- function(d, o, i, j) {
  original_data <- d %>% collect_schema_data()
  selective_random_data <- original_data %>% select_operators(o) %>% select_x_percent_across_operators(i)
  reduced_numerator <- selective_random_data %>% transform_reduced_killed_count()
  reduced_denominator <- selective_random_data %>% transform_reduced_total_count()
  original_numerator <-  original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- selective_random_data %>% summarize_reduced_time()
  original_time <- original_data %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_error() %>%
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_percent_calculations
#'
#' Calculate the effectiveness of a reduction technique on a per-schema per-percentage basis.
#' @export

analyze_percent_calculations <- function(d) {
  d <- d %>% collect_schema_percent_data()
  dt <- d %>% transform_mae() %>% transform_rmse()
  return(dt)
}

#' FUNCTION: analyze_summary_percent_calculations
#'
#' Calculate the effectiveness of a reduction technique.
#' The metrics to determine effectiveness will be Kendall's tau_b correlation coefficient,
#' mae and rmse.
#' @export

analyze_summary_percent_calculations <- function(d) {
  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    df <- data.frame()
    for(i in percentages) {
      percent_data <- d %>% dplyr::filter(percentage == (i*100))
      percent <- (i*100)
      corr <- percent_data %>% transform_correlation()
      dt <- data.frame(percent, corr[1])
      df <- rbind(df, dt)
  }
  return(df)
}

#' FUNCTION: analyze_keep
#'
#' @export

analyze_keep <- function(d) {
  df <- data.frame()

    for(j in 1:30) {
    print(paste("KEEP: Currently on trial ", j, " ..."))
      dt <- reduce_keep(d, j) %>% as.data.frame()
      df <- rbind(df, dt)
  }
  return(df)
}

#' FUNCTION: reduce_keep
#'
#' Using some keep column calculate the reduced set's mutation score
#' @export

reduce_keep <- function(d, j) {
  original_data <- d %>% transform_keep_all() %>% collect_schema_data()
  keep_data <- d %>% transform_keep() %>% collect_keep_data()
  reduced_numerator <- keep_data %>% transform_reduced_killed_count()
  reduced_denominator <- keep_data %>% transform_reduced_total_count()
  original_numerator <-  original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- keep_data %>% summarize_reduced_time()
  original_time <- original_data %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_error() %>%
        transform_add_trial(j)
  return(dt)
}
