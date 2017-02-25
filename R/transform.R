#' FUNCTION: transform_replace_correlation
#'
#' DESCRIPTION: Make sure that the correlation coeffient calculated by the 'Kendall' R package is renamed
#' from 'estimate' to 'correlation'
#' @export

transform_replace_correlation <- function(d) {
  dt <- d %>% dplyr::rename(correlation = estimate)
}

#' FUNCTION: transform_add_trial
#'
#' Add the trial
#' @export

transform_add_trial <- function(d, t) {
  dt <- d %>% dplyr::mutate(trial = t)
  return(dt)
}

#' FUNCTION: transform_add_position
#'
#' Add the position column
#' @export

transform_add_position <- function(d, t) {
  dt <- d %>% dplyr::mutate(position = t)
  return(dt)
}

#' FUNCTION: transform_add_start_position
#'
#' Add the start position column
#' @export

transform_add_start_position <- function(d, t) {
  dt <- d %>% dplyr::mutate(start_position = t)
  return(dt)
}

#' FUNCTION: transform_add_start_and_step
#'
#' Append the randomly-generated start position and appropriate step size
#' @export

transform_add_start_and_step <- function(d, s, f) {
  dt <- d %>% do(dplyr::mutate(., position = select_start_position(., f))) %>%
    do(dplyr::mutate(., start_position = select_start_position(., f))) %>%
    do(dplyr::mutate(., step_size = select_step_size(., s))) %>%
    dplyr::ungroup() # has to be separate from first mutate; causes errors otherwise
  return(dt)
}

#' FUNCTION: transform_update_position
#'
#' Update the position after the flip
#' @export

transform_update_position <- function(d, p) {
  dt <- d %>% do(dplyr::mutate(., position = p))
  return(dt)
}

#' FUNCTION: transform_add_percentage_trial
#'
#' Add the analyzed percentage and current trial to data frame
#' @export

transform_add_percentage_trial <- function(d, p, t) {
  dt <- d %>% dplyr::mutate(percentage = p)
  dt <- dt %>% dplyr::mutate(trial = t)
  return(dt)
}

#' FUNCTION: transform_reduced_killed_count
#'
#' Count the number of killed mutants
#' @export

transform_reduced_killed_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(reduced_numerator = n)
  return(dt)
}

#' FUNCTION: transform_reduced_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_reduced_total_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(reduced_denominator = n)
  return(dt)
}

#' FUNCTION: transform_original_killed_count
#'
#' Count the number of killed mutants
#' @export

transform_original_killed_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_numerator = n)
  return(dt)
}

#' FUNCTION: transform_original_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_original_total_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_denominator = n)
  return(dt)
}

#' FUNCTION: transform_mutant_count
#'
#' Count the total number of mutants associated with a schema
#' @export

transform_mutant_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(mutant_count = n())
  return(dt)
}

#' FUNCTION: transform_cost_reduction
#'
#' Calculate the reduction in time for performing mutation analysis on the reduced set compared to the original set

transform_cost_reduction <- function(d) {
    dt <- d %>% dplyr::mutate(cost_reduction = ((original_time - reduced_time) / (original_time)))
    return(dt)
}

#' FUNCTION: transform_reduced_mutation_score
#'
#' Calculate the mutation score for a given set (number of killed mutants / total number of mutants)
#' @export

transform_reduced_mutation_score <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(reduced_mutation_score = ((reduced_numerator / reduced_denominator)))
  return(dt)
}

#' FUNCTION: transform_original_mutation_score
#'
#' Calculate the mutation score for the original set (number of killed mutants / total number of mutants)
#' @export

transform_original_mutation_score <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(original_mutation_score = ((original_numerator / original_denominator)))
  return(dt)
}

#' FUNCTION: transform_mutation_score
#'
#' Calculate the mutation score for a set (number of killed mutants / total number of mutants)
#' @export

transform_mutation_score <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(mutation_score = ((original_numerator / original_denominator)))
  return(dt)
}

#' FUNCTION: transform_error
#'
#' Calculate the error --- the difference --- between the original mutation score and the reduced mutation score.
#' @export

transform_error <- function(d) {
  dt <- d %>% dplyr::mutate(error = abs((original_mutation_score - reduced_mutation_score)))
  return(dt)
}

#' FUNCTION: transform_mae
#'
#' Calculate Mean Absolute Error (mae)
#' @export

transform_mae <- function(d) {
  dt <- d %>% dplyr::mutate(mae = mean(abs(error)))
  return(dt)
}

#' FUNCTION: transform_rmse
#'
#' Calculate Root Mean Squared Error (rmse)
#' @export

transform_rmse <- function(d) {
  dt <- d %>% dplyr::mutate(rmse = sqrt(mean((error)^2)))
  return(dt)
}

#' FUNCTION: transform_keep
#'
#' This column has values representing whether or not a mutant is considered when calculating mutation score
#' as well as other metrics.
#' @export

transform_keep <- function(d, all=TRUE) {
  s <- c(TRUE, FALSE)

  if (all == TRUE) {
    dt <- d %>% dplyr::mutate(keep = TRUE)
  } else {
    dt <- d %>% dplyr::mutate(keep = sample(s, 1, size = nrow(d)))
  }
  return(dt)
}

#' FUNCTION: transform_fitness
#'
#' Calculate the "fitness" of a step.
#' @export

transform_fitness <- function(d, error_weight, cost_weight) {
  d <- d %>% collect_schema_data()
  dt <- d %>% dplyr::mutate(fitness = ((cost_weight * cost_reduction) - (error_weight * error)))
  return(dt)
}

#' FUNCTION: transform_add_correlation
#'
#' Calculate the correlation of a step.
#' @export

transform_add_correlation <- function(d) {
  dt <- d %>% dplyr::ungroup() %>% dplyr::mutate(correlation = calculate_correlation(d))
  return(dt)
}

#' FUNCTION: transform_add_step
#'
#' Simply add the current step to the dataframe
#' @export

transform_add_step <- function(d, s) {
  dt <- d %>% dplyr::ungroup() %>% dplyr::mutate(step = s)
  return(dt)
}
