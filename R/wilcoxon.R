#' FUNCTION: ranked_sum_interpret
#'
#' Interpret the Wilcoxon Ranked-Sum Test using.
#' @export

ranked_sum_interpret <- function(v) {

  if (is.nan(v)) {
    significant <- "none"
  }

  else if (v < 0.05) {
    significant <- "true"
  }

  else if (v >= 0.05) {
    significant <- "false"
  }

  return(significant)
}

#' FUNCTION: perform_wilcoxon_accurate
#'
#' Calculate the wilcoxon ranked sum for all pairs of techniques
#' @export

perform_wilcoxon_accurate <- function(d, m) {
  df <- data.frame()
  ds <- split(d, list(d$technique))
  len <- length(ds)

  for (i in 1:len) {
    for (j in 1:len) {
      t1 <- ds[[i]]$technique %>% unique()
      t2 <- ds[[j]]$technique %>% unique()
      print(paste("comparing ", t1, " to ", t2))

      if (m == "correlation") {
        print("correlation")
        model <- wilcox.test(ds[[i]]$correlation, ds[[j]]$correlation)
        tidy_model <- model %>% broom::tidy() %>% transform_add_significance()
        dt <- tidy_model %>% dplyr::mutate(group1 = t1, group2 = t2)
        df <- rbind(df, dt)
      }

      else if (m == "cost reduction") {
        print("cost reduction")
        model <- wilcox.test(ds[[i]]$cost_reduction, ds[[j]]$cost_reduction)
        tidy_model <- model %>% broom::tidy() %>% transform_add_significance()
        dt <- tidy_model %>% dplyr::mutate(group1 = t1, group2 = t2)
        df <- rbind(df, dt)
      }

      else {
        print("WARNING: Please provide a metric")
      }
    }
  }
  return(df)
}

