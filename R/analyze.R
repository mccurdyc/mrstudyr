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
  outside_step <- 1
  o <- d %>% transform_keep()
  g <- o
  temp <- data.frame()
  current_best_fit <- evaluate_reduction_technique(o, o) %>% transform_fitness(0.5, 0.5) %>% transform_add_step_number(0) %>% calculate_best_fit() %>% collect_schema_data()

  # while schema$fitness < schema$best_fit (is this the same as checking identical? no! could decrease) || certain number of rounds / time / ...
  # while (!identical(g, temp)) {
  for (s )
  while ((current_best_fit$best_fit == current_best_fit$fitness) && !identical(g, temp)) {
    step_number <- partition_size
    dk <- data.frame()
    df <- data.frame()
    print(paste("outside step number: ", outside_step))
    while (step_number <= 300) {
      # while (step_number <= nrow(g)) {
      # we keep this so that we can show which mutants to ignore (instead of only the ones to keep)
      k <- g %>% helper_bitflip_keep(step_number, partition_size) %>% transform_add_step_number(step_number) %>% as.data.frame()
      r <- k %>% collect_keep_data()
      da <- evaluate_reduction_technique(o, r) %>% transform_fitness(0.5, 0.5) %>% transform_add_step_number(step_number) %>% as.data.frame()
      dk <- rbind(dk, k)
      df <- rbind(df, da)
      step_number <- step_number + partition_size
    }

    b <- df %>% calculate_best_fit() %>% collect_best_fit_data()
    current_best_fit <- b[!duplicated(b$schema), ]
    temp <- g
    g <- helper_gather_keep_data(current_best_fit, dk)
    outside_step <- outside_step + 1
    dplyr::glimpse(current_best_fit %>% dplyr::filter(schema == "BankAccount"))
  }

  # return(g)
  # return(dk) # all of the keep data, not filtered like 'g'
  return(current_best_fit) # just the actual best_fit values and their respective step for each schema
}
