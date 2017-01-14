#' FUNCTION: calculate_percent_effectiveness
#'
#' Calculate the effectiveness of a reduction technique on a per-schema per-percentage basis.
#' @export

calculate_percent_effectiveness <- function(d) {
  d <- d %>% collect_schema_percent_data()
  dt <- d %>% transform_mae() %>% transform_rmse()
  return(dt)
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

