#' FUNCTION: analyze_random_sampling
#'
#' This function will perform random sampling.
#' @export

analyze_random_sampling <- function(d) {
  o <- d %>% collect_schema_data()

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

  df <- data.frame()

  for(i in percentages) {
    print(paste("RANDOM SAMPLING: Currently analyzing x =", (i*100), "percent ..."))
    for(j in 1:30) {
      r <- o %>% select_x_percent(i)
      dt <- evaluate_reduction_technique(o, r) %>% transform_add_percentage_trial((i * 100), j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: analyze_selective_random
#'
#' This function will analyze a select set of operators over percentages
#' This function can also be used to perform traditional selective mutation, i.e. mutation using a
#' reduced set of operators by choosing a select set and analyzing at 100%.
#' @export

analyze_selective_random <- function(d, operators) {
  o <- d %>% collect_schema_data()

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  print("OPERATORS SELECTED:")
  print(operators)
  for(i in percentages) {
    print(paste("SELECTIVE RANDOM: Currently analyzing x =", (i * 100), "percent ..."))
    for(j in 1:30) {
      r <- o %>% select_operators(operators) %>% select_x_percent_across_operators(i)
      dt <- evaluate_reduction_technique(o, r) %>% transform_add_percentage_trial((i * 100), j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: analyze_incremental
#'
#' Analyze how reducing the set incrementally effects the error between MS and MS'
#' @export

analyze_incremental <- function(d, partition_size=1) {
  df <- data.frame()
  schemas <- d %>% dplyr::select(schema) %>% dplyr::distinct()
  for(s in schemas[[1]]) {
    print(paste("CURRENT SCHEMA: ", s))
    o <- d %>% dplyr::filter(schema == s) %>% transform_keep()
    dt <- helper_incremental(o, partition_size)
    df <- rbind(df, dt)
  }
  return(df)
}

