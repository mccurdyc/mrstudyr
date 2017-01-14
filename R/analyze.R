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
        transform_error() %>%
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_selective_random
#'
#' This function will analyze a select set of operators over percentages
#' This function can also be used to perform traditional selective mutation, i.e. mutation using a
#' reduced set of operators by choosing a select set and analyzing at 100%.
#' @export

analyze_selective_random <- function(d, o) {

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  print("OPERATORS SELECTED:")
  print(o)
  for(i in percentages) {
    print(paste("SELECTIVE RANDOM: Currently analyzing x =", (i*100), "percent ..."))
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
        transform_error() %>%
        transform_add_percentage_trial((i * 100), j)
  return(dt)
}

#' FUNCTION: analyze_percent_calculations
#'
#' Calculate the effectiveness of a reduction technique on a per-schema per-percentage basis.
#' @export

analyze_percent_calculations <- function(d) {
  d <- d %>% collect_schema_percent_data()
  dt <- d %>% transform_mae() %>% transform_rmse()
  return(dt)
}

#' FUNCTION: analyze_summary_percent_calculations
#'
#' Calculate the effectiveness of a reduction technique.
#' The metrics to determine effectiveness will be Kendall's tau_b correlation coefficient,
#' mae and rmse.
#' @export

analyze_summary_percent_calculations <- function(d) {
  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    df <- data.frame()
    for(i in percentages) {
      percent_data <- d %>% dplyr::filter(percentage == (i*100))
      percent <- (i*100)
      corr <- percent_data %>% transform_correlation()
      dt <- data.frame(percent, corr[1])
      df <- rbind(df, dt)
  }
  return(df)
}

#' FUNCTION: analyze_incremental
#'
#' Analyze how reducing the set incrementally effects the error between MS and MS'
#' @export

analyze_incremental <- function(d, partition_size = 1) {
  step_number <- 1
  step_data <- d %>% transform_keep()
  df <- data.frame()

  repeat {
    if (step_number > 10) {
    # if (step_number > nrow(f)) {
      break
    }

    print(paste("Current step number is: ", step_number))
    if (step_number <= 10) {
    # if (step_number <= nrow(f)) {
      step_data <- step_data %>% bitflip_keep(step_number, partition_size)
      # step_data %>% dplyr::filter(row_number() == step_number) %>% dplyr::glimpse()
      da <- step_data %>% reduce_incremental(step_number) %>% as.data.frame()
      df <- rbind(df, da)
    }
    step_number <- step_number + 1
  }
  return(df)
}

#' FUNCTION: reduce_incremental
#'
#' Using keep column, calculate the original and reduced sets mutation score, error and execution time.
#' @export

reduce_incremental <- function(d, step_number) {
  original_data <- d %>% transform_keep()
  reduced_data <- d %>% collect_keep_data()
  reduced_numerator <- reduced_data %>% transform_reduced_killed_count()
  reduced_denominator <- reduced_data %>% transform_reduced_total_count()
  original_numerator <- original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- reduced_data %>% summarize_reduced_time()
  original_time <- original_data %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_error() %>%
        transform_add_step_number(step_number)
  return(dt)
}

#' FUNCTION: analyze_keep
#'
#' This function performs a hill-climbing reduction technique.
#' @export

analyze_keep <- function(d, partition_size = 1) {
  start_keep <- data.frame()
  start_calculations <- data.frame()
  end_calculations <- data.frame()

  # this generates and evaluates the starting neighborhood
  for (j in 1:1) {
  # for (j in 1:30) {
    print(paste("Generating neighborhood, on trial ", j))
    neighborhood <- d %>% generate_keep(j)
    dt <- neighborhood %>% reduce_keep(j) %>% as.data.frame
    start_keep <- rbind(start_keep, neighborhood)
    start_calculations <- rbind(start_calculations, dt)
  }

  start_calculations_rmse_mae <- start_calculations %>% analyze_keep_calculations() %>% as.data.frame()
  start_calculations_rmse_mae %>% dplyr::glimpse()
  print("Calculated start calculations")

  # while counter < # keeps, bitflip, evaluate, compare
  step <- start_keep
  step_number <- 1

  repeat {
    if (step_number > 800) {
      # if (step_number > nrow(f)) {
      break
    }

    print(paste("Stepping POSITION, at position ", step_number))

    for (j in 1:1) {
    # for (j in 1:30) {

      print(paste("Stepping TRIAL, on trial ", j))

      # each trial is a different neighborhood, so we only want to step at each neighborhood
      f <- step %>% collect_chosen_trial_data(j)

      if (step_number <= 800) {
      # if (step_number <= nrow(f)) {
        f <- f %>% bitflip_keep(step_number, partition_size)
        step <- f
        da <- f %>% reduce_keep(j) %>% as.data.frame()
        # da %>% dplyr::glimpse()
        end_calculations <- rbind(end_calculations, da)
      }
    }

    end_calculations_rmse_mae <- end_calculations %>% analyze_keep_calculations() %>% as.data.frame()
    end_calculations_rmse_mae %>% dplyr::glimpse()
    step_number <- step_number + 1
  }
  print("Calculated end calculations")
  # if (RMSE > start$RMSE) {
  # break
  # }
  # }
  return(end_calculations_rmse_mae)
}

#' FUNCTION: generate_keep
#'
#' This function generates a data frame consisting of a starting neighborhood that has been randomly-generated.
#' @export

generate_keep <- function(d, trial) {
  dt <- d %>% transform_keep() %>% transform_add_trial(trial)
  return(dt)
}

#' FUNCTION: reduce_keep
#'
#' Using keep column, calculate the original and reduced sets' mutation score, error and execution time.
#' @export

reduce_keep <- function(d, trial) {
  trial_data <- d %>% collect_chosen_trial_data(trial)
  original_data <- trial_data %>% collect_schema_data()
  keep_data <- trial_data %>% collect_keep_data() %>% collect_schema_data()
  reduced_numerator <- keep_data %>% transform_reduced_killed_count()
  reduced_denominator <- keep_data %>% transform_reduced_total_count()
  original_numerator <-  original_data %>% transform_original_killed_count()
  original_denominator <- original_data %>% transform_original_total_count()
  reduced_time <- keep_data %>% summarize_reduced_time()
  original_time <- original_data %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
    transform_reduced_mutation_score() %>%
    transform_original_mutation_score() %>%
    transform_error() %>%
    transform_add_trial(trial)
  return(dt)
}

#' FUNCTION: bitflip_keep
#'
#' Currently, this function negates boolean values i.e., TRUE -> FALSE, FALSE -> TRUE. Position is
#' used to idicate the current position to bitflip and partition_size is the number of subsequent
#' positions to also flip --- this could be useful if we wanted to try different sizes to reduce time
#' of HC by increasing step size. **We could add another parameter 'group_by' so that instead of just
#' flipping consecutive values, we could flip based on some group (e.g., operators).
#' @export

bitflip_keep <- function(d, position, partition_size=1) {
  d <- d %>% collect_schema_data()
  df <- data.frame()

  if (position == 1) {
    m <- d %>% dplyr::filter(row_number() <= ((position + partition_size) - 1))
    r <- d %>% dplyr::filter(row_number() > ((position + partition_size) - 1))
    u <- m %>% dplyr::mutate(keep = !keep)
    df <- rbind(u, r)
  } else {
    b <- d %>% dplyr::filter(row_number() < position)
    m <- d %>% dplyr::filter(row_number() == ((position + partition_size) - 1))
    r <- d %>% dplyr::filter(row_number() > ((position + partition_size) - 1))
    u <- m %>% dplyr::mutate(keep = !keep)
    df <- rbind(b, u, r)
  }
  return(df)
}

#' FUNCTION: analyze_keep_calculations
#'
#' Calculate the effectiveness of a reduction technique across trials i.e., there is a single MAE, RMSE, CORR value across all trials.
#' @export

analyze_keep_calculations <- function(d) {
  # d <- d %>% dplyr::ungroup()
  # d <- d %>% dplyr::ungroup() %>% collect_chosen_trial_data(trial)
  # dt <- d %>% transform_mae() %>% transform_rmse()
  dt <- d %>% transform_mae() %>% transform_rmse() %>% transform_keep_correlation()
  return(dt)
}
