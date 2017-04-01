#' FUNCTION: ranked_sum_interpret
#'
#' Interpret the Wilcoxon Ranked-Sum Test using.
#' @export

ranked_sum_interpret <- function(v) {
  if (v >= 0.05) {
    dt <- FALSE
  }
  else {
    dt <- TRUE
  }
  return(dt)
}

#' FUNCTION: perform_pairwise_wilcoxon_rank_sum_test
#'
#' Perform a statistical analysis of the correlation for each mutant reduction technique.
#' @export

perform_pairwise_wilcoxon_rank_sum_test <- function(d) {
  model <- pairwise.wilcox.test(d$correlation, d$technique, paired=TRUE)
  tidy_model <- model %>% broom::tidy()
  dt <- tidy_model %>% transform_add_significance()
  return(dt)
}

#' FUNCTION: perform_wilcoxon_accurate
#'
#' Calculate the wilcoxon ranked sum for all pairs of techniques
#' @export

perform_wilcoxon_accurate <- function(d) {
  df <- data.frame()
  ds <- split(d, list(d$technique))
  len <- length(ds)

  for (i in 1:len) {
    for (j in 1:len) {
      t1 <- ds[[i]]$technique %>% unique()
      t2 <- ds[[j]]$technique %>% unique()
      print(paste("comparing ", t1, " to ", t2))

      model <- wilcox.test(ds[[i]]$correlation, ds[[j]]$correlation)
      tidy_model <- model %>% broom::tidy()
      dt <- tidy_model %>% dplyr::mutate(group1 = t1, group2 = t2)
      df <- rbind(df, dt)
    }
  }
  return(df)
}

