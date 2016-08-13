#' FUNCTION: analyze_random_sampling
#'
#' This function will perform random sampling.
#' @export

analyze_random_sampling <- function(d) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  for(i in percentages) {
    print(paste("RANDOM SAMPLING: Currently analysing x =", (i*100), "percent ..."))
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

#' FUNCTION: analyze_across_operators
#'
#' This function will perform sampling across all operators --- this is not operators selection as alll
#' operators will be present.
#' @export

analyze_across_operators <- function(d) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  for(i in percentages) {
    print(paste("ACROSS OPERATORS: Currently analysing x =", (i*100), "percent ..."))
    for(j in 1:30) {
      dt <- across_operators(d, i, j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: across_operators
#'
#' Perform random sampling across operators
#' @export

across_operators <- function(d, i, j) {
  original_data <- d %>% collect_schema_data()
  across_operator_data <- original_data %>% select_x_percent_across_operators(i)
  reduced_numerator <- across_operator_data %>% transform_reduced_killed_count()
  reduced_denominator <- across_operator_data %>% transform_reduced_total_count()
  original_numerator <-  original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- across_operator_data %>% summarize_reduced_time()
  original_time <- original_data %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_calculations
#'
#' Calculate the effectiveness of a reduction technique.
#' The metrics to determine effectiveness will be Kendall's tau_b correlation coefficient,
#' mae and rmse.
#' @export

analyze_calculations <- function(d) {
  # percentages <- c(0.01)
  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()
  for(i in percentages) {
    percent_data <- d %>% dplyr::filter(percentage == (i*100))
    percent <- (i*100)
    corr <- percent_data %>% analyze_correlation()
    error <- percent_data %>% analyze_error()
    # dplyr::glimpse(error)
    dt <- data.frame(percent, corr[1])
    dt <- dt %>% transform_mae(error) %>% transform_rmse(error)
    df <- rbind(df, dt)
  }
  return(df)
}

#' FUNCTION: calculate_correlation
#'
#' This function will calculate the correlation between the reduced and the original
#' mutation score for a given percent.
#' @export

calculate_correlation <- function(x, y) {
  model <- cor.test(x, y, method = "kendall", use = "pairwise")
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}

#' FUNCTION: analyze_correlation
#'
#' Calculate Kendall's tau_b correlation coefficient between the reduced and original mutation score
#' @export

analyze_correlation <- function(d) {
  # print(head(d, n = 9))
  x <- d %>% dplyr::select(reduced_mutation_score) %>% unlist() %>% as.numeric()
  y <- d %>% dplyr::select(original_mutation_score) %>% unlist() %>% as.numeric()
  dt <- calculate_correlation(x, y)
  return(dt)
}

#' FUNCTION: analyze_error
#'
#' Calculate the error between two sets of mutation scores
#' @export

analyze_error <- function(d) {
  x <- d %>% dplyr::select(reduced_mutation_score)
  y <- d %>% dplyr::select(original_mutation_score)
  dt <- (y - x)
  return(dt)
}
