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
  dd <- data.frame()
  schemas <- d %>% dplyr::select(schema) %>% dplyr::distinct()
  for(s in schemas[[1]]) {
    print(paste("current schema: ", s))
    outside_step <- 1
    o <- d %>% dplyr::filter(schema == s) %>% transform_keep()
    g <- o
    # initialized to set temp_best_fit to fitness of original data
    current_best_fit <- evaluate_reduction_technique(o, o) %>%
      transform_fitness(0.5, 0.5) %>%
      transform_add_position(0) %>%
      calculate_best_fit() %>%
      collect_schema_data()

    # for(j in 1:1) {
      start_position <- 106
      # start_position <- o %>% dplyr::count() %>% select_random_start_position()
      while (TRUE) {

        position <- start_position
        dk <- data.frame() # hold the keep data
        df <- data.frame() # hold the best_fit data
        print(paste("outside step number: ", outside_step))

        while (TRUE) {
          k <- g %>% helper_bitflip_keep(position, partition_size) %>%
            transform_add_position(position) %>%
            as.data.frame()
          r <- k %>% collect_keep_data() # only data that was 'kept'
          da <- evaluate_reduction_technique(o, r) %>%
            transform_fitness(0.5, 0.5) %>%
            transform_add_position(position) %>%
            as.data.frame()
          dk <- rbind(dk, k) # keep data
          df <- rbind(df, da) # best_fit data

          if ((position + partition_size) > nrow(g)) {
            position <- (position + partition_size) - nrow(d)
          }
            position <- position + partition_size

          if (position == start_position) {
            break
          }
        }

        temp_best_fit <- current_best_fit %>% as.data.frame()
        b <- df %>% calculate_best_fit() %>% collect_best_fit_data()
        current_best_fit <- b[!duplicated(b$schema), ] # if ties, only keep one per schema
        g <- helper_gather_keep_data(current_best_fit, dk)
        outside_step <- outside_step + 1
        current_best_fit %>% dplyr::glimpse()

        # we stop if it is equal because then we are no longer climbing, we have plateaued
        if ((current_best_fit$best_fit <= temp_best_fit$best_fit)) {
          dd <- rbind(dd, temp_best_fit)
          break
        }
      }
    }
  # return(k) # the final reduced keep data telling you which mutants to keep and ignore
  return(dd) # just the actual best_fit values and their respective step for each schema
}
