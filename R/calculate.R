#' FUNCTION: calculate_effectiveness
#'
#' Calculate the effectiveness of a reduction technique on a per-schema (optionall, per-percentage) basis.
#' @export

calculate_effectiveness <- function(d, p=FALSE) {
  if ( p == TRUE) {
    d <- d %>% collect_schema_percent_data()
  } else {
    d <- d %>% collect_schema_data()
  }
  dt <- d %>% transform_mae() %>% transform_rmse()
  return(dt)
}

#' FUNCTION: calculate_correlation
#'
#' This function will calculate the correlation between the reduced and the original
#' mutation score for a given percent. This is a helper function for the transform_correlation function
#' @export

calculate_correlation <- function(d) {
  x <- d[['reduced_mutation_score']]
  y <- d[['original_mutation_score']]

  model <- cor.test(x, y, method = "kendall", use = "pairwise")
  dt <- model %>% broom::tidy() %>% transform_replace_correlation()
  return(dt[['correlation']]) # return just correlation
}

#' FUNCTION: calculate_percent_summary
#'
#' Calculate the effectiveness of a reduction technique.
#' The metrics to determine effectiveness will be Kendall's tau_b correlation coefficient,
#' mae and rmse.
#' @export

calculate_percent_summary <- function(d) {
  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    df <- data.frame()
    for(i in percentages) {
      percent_data <- d %>% dplyr::filter(percentage == (i * 100))
      percent <- (i * 100)
      corr <- percent_data %>% transform_correlation()
      dt <- data.frame(percent, corr[1])
      df <- rbind(df, dt)
  }
  return(df)
}

#' FUNCTION: calculate_best_fit
#'
#' Determine which step is best fit for a given schema
#' @export

calculate_best_fit <- function(d) {
  d %>% collect_schema_data() %>% dplyr::mutate(best_fit = max(fitness))
}

#' FUNCTION: calculate_summary
#'
#' Calculate the effectiveness of a reduction technique.
#' The metrics to determine effectiveness will be Kendall's tau_b correlation coefficient,
#' mae and rmse.
#' @export

calculate_summary <- function(d) {
    df <- data.frame()
    for(i in percentages) {
      percent_data <- d %>% dplyr::filter(percentage == (i * 100))
      percent <- (i * 100)
      corr <- percent_data %>% transform_correlation()
      dt <- data.frame(percent, corr[1])
      df <- rbind(df, dt)
  }
  return(df)
}

#' FUNCTION: calculate_neighborhood_size
#'
#' This function extracts the size of the neighborhood from the data.
#' @export

calculate_neighborhood_size <- function(d) {
  dt <- d %>% dplyr::select(step) %>% dplyr::distinct() %>% max()
  return(dt)
}
