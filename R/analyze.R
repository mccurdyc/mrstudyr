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
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_selective_random_mutation
#'
#' This function will analyze a select set of operators over percentages
#' This function can also be used to perform traditional selective mutation, i.e. mutation using a
#' reduced set of operators by choosing a select set and analyzing at 100%.
#' @export

analyze_selective_random_mutation <- function(d, o) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  for(i in percentages) {
    print(paste("OPERATOR SAMPLING: Currently analyzing x =", (i*100), "percent ..."))
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
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_percent_calculations
#'
#' Calculate the effectiveness of a reduction technique across schemas.
#' The metrics to determine effectiveness will be Kendall's tau_b correlation coefficient,
#' mae and rmse.
#' @export

analyze_percent_calculations <- function(d) {
  # d <- d %>% dplyr::filter(original_mutation_score != 100.00000)
  # percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  # schemas <- d %>% select_all_schemas()
  d <- d %>% collect_percent_data()
  # return(schemas)

  # df <- data.frame()
  # for(s in schemas) {
    # for(i in percentages) {
      # a <- d %>% dplyr::filter(schema == s[1], percentage == (i*100))
      # current_schema <- s
      # percent <- (i*100)
      # corr <- a %>% analyze_correlation()
      # error <- d %>% analyze_error() %>%
      dt <- d %>% transform_error() %>% transform_mae() %>% transform_rmse()

      # dt <- data.frame(current_schema, percent) %>%
      # dt <- data.frame(current_schema, percent, corr[1]) %>%
            # transform_mae() %>% transform_rmse()
      # df <- rbind(df, dt)
          return(dt)
    # }
  # }
  # return(df)
}
