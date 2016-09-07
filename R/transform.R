#' FUNCTION: transform_replace_correlation
#'
#' DESCRIPTION: Make sure that the correlation coeffient calculated by the 'Kendall' R package is renamed
#' from 'estimate' to 'correlation'
#' @export

transform_replace_correlation <- function(d) {
  dt <- d %>% dplyr::rename(correlation = estimate)
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
  # dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(original_numerator = dplyr::filter(o, killed %in% c("true")) %>% dplyr::count())
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_numerator = n)
  return(dt)
}

#' FUNCTION: transform_original_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_original_total_count <- function(d) {
  # dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(original_denominator = dplyr::filter(o, killed %in% c("true", "false")) %>% dplyr::count())
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_denominator = n)
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
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(reduced_mutation_score = ((reduced_numerator / reduced_denominator) * 100))
  return(dt)
}

#' FUNCTION: transform_original_mutation_score
#'
#' Calculate the mutation score for the original set (number of killed mutants / total number of mutants)
#' @export

transform_original_mutation_score <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(original_mutation_score = ((original_numerator / original_denominator) * 100))
  return(dt)
}

#' FUNCTION: transform_mae
#'
#' Calculate Mean Absolute Error (mae)
#' @export

transform_mae <- function(d, e) {
  # e <- d %>% dplyr::select(error)
  e <- e %>% unlist() %>% as.numeric()
  dt <- d %>% dplyr::mutate(mae = mean(abs(e)))
  return(dt)
}

#' FUNCTION: transform_rmse
#'
#' Calculate Root Mean Squared Error (rmse)
#' @export

transform_rmse <- function(d, e) {
  # e <- d %>% dplyr::select(error)
  dt <- d %>% dplyr::mutate(rmse = sqrt(mean((e)^2)))
  return(dt)
}
