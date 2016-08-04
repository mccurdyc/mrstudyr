#' FUNCTION: transform_add_percentage_trial
#'
#' Add the analysed percentage and current trial to data frame
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
  # dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(reduced_numerator = summarise_reduced_killed_count(r))
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(reduced_numerator = n)
  return(dt)
}

#' FUNCTION: transform_reduced_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_reduced_total_count <- function(d) {
  # dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(reduced_denominator = dplyr::filter(d, killed %in% c("true", "false")) %>% dplyr::count())
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(reduced_denominator = n)
  return(dt)
}

#' FUNCTION: transform_original_killed_count
#'
#' Count the number of killed mutants
#' @export

transform_original_killed_count <- function(d) {
  # dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(original_numerator = dplyr::filter(o, killed %in% c("true")) %>% dplyr::count())
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_numerator = n)
  return(dt)
}

#' FUNCTION: transform_original_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_original_total_count <- function(d) {
  # dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(original_denominator = dplyr::filter(o, killed %in% c("true", "false")) %>% dplyr::count())
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
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
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(reduced_mutation_score = (reduced_numerator / reduced_denominator))
  return(dt)
}

#' FUNCTION: transform_original_mutation_score
#'
#' Calculate the mutation score for the original set (number of killed mutants / total number of mutants)
#' @export

transform_original_mutation_score <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::mutate(original_mutation_score = (original_numerator / original_denominator))
  return(dt)
}
