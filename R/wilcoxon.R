#' FUNCTION: ranked_sum_interpret
#'
#' Interpret the Wilcoxon Ranked-Sum Test using.
#' @export

ranked_sum_interpret <- function(v) {
  if (v < 0.05) {
    dt <- TRUE
  }
  else {
    dt <- FALSE
  }
  return(dt)
}

#' FUNCTION: perform_pairwise_wilcoxon_rank_sum_test
#'
#' Perform a statistical analysis of the correlation for each mutant reduction technique.
#' @export

perform_pairwise_wilcoxon_rank_sum_test <- function(d) {
  model <- pairwise.wilcox.test(d$correlation, d$technique)
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}
