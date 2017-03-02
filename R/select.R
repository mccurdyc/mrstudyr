#' FUNCTION: select_x_percent
#'
#' This function will be used to look at a select percentage of the data provided.
#' This is referred to as uniform random sampling.
#' @export

select_x_percent <- function(d, x) {
  dt <- d %>% collect_schema_data() %>% dplyr::sample_frac(x)
  return(dt)
}

#' FUNCTION: select_x_percent_across_operators
#'
#' Select a uniform percentage of data for each operator.
#' @export

select_x_percent_across_operators <- function(d, x) {
  dt <- d %>% collect_schema_operator_data() %>% select_x_percent(x)
  return(dt)
}

#' FUNCTION: select_operators
#'
#' This function will be used to look at a select set of operators.
#' This is referred to as selective mutation
#' @export

select_operators <- function(d, o) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(operator %in% o)
  return(dt)
}

#' FUNCTION: select_all_operators
#'
#' This function returns all available operators
#' @export

select_all_operators <- function(d) {
  dt <- d %>% dplyr::select(operator) %>% dplyr::distinct()
  return(dt)
}

#' FUNCTION: select_schema_data
#'
#' Select the data for only a specified schema.
#' @export

select_schema_data <- function(d, s) {
  dt <- d %>% dplyr::filter(schema %in% s)
  return(dt)
}

#' FUNCTION: select_all_schemas
#'
#' This function returns all available schemas
#' @export

select_all_schemas <- function(d) {
  dt <- d %>% dplyr::select(schema) %>% dplyr::distinct()
  return(dt)
}

#' FUNCTION: select_random_start_position
#'
#' For the hill climbing approach a start position needs to be randomly chosen.
#' This function randomly selects a start position between 1 and the number of observations associated
#' with a schema (i.e., the number of rows or number of mutants).
#' @export

select_random_start_position <- function(x) {
  return(sample(1:x[[1]], 1, TRUE))
}

#' FUNCTION: select_random_percent
#'
#' Might merge into other similar function, still thinking
#' @export

select_random_percent <- function() {
  dt <- (sample(1:100[[1]], 1, TRUE)/100)
  return(dt)
}

#' FUNCTION: select_start_position
#'
#' Might merge into other similar function, still thinking
#' @export

select_start_position <- function(d, r) {
  dt <- (r * nrow(d)) %>% round()
  # indexing of mutants starts at 1; don't let it start at 0
  if (dt == 0) {
    dt <- 1
  }
  # print(paste("position: ", dt))
  return(dt)
}

#' FUNCTION: select_step_size
#'
#' @export

select_step_size <- function(d, s) {
  dt <- (s * nrow(d)) %>% round()
  # don't let step size be 0
  if (dt == 0) {
    dt <- 1
  }
  # print(paste("step size: ", dt))
  return(dt)
}

#' FUNCTION: select_current_position
#'
#' Get the current position (a single value), not a column
#' @export

select_current_position <- function(d) {
  dt <- d %>% dplyr::select(position) %>% dplyr::distinct()
  return(dt)
}

#' FUNCTION: select_current_step_size
#'
#' Get the current step size (a single value), not a column
#' @export

select_current_step_size <- function(d) {
  dt <- d %>% dplyr::select(step_size) %>% dplyr::distinct()
  return(dt)
}

#' FUNCTION: select_current_best_correlation
#'
#' Get the highest correlation from a list of correlation values and return a single value
#' @export

select_current_best_correlation <- function(d) {
  dt <- d %>% dplyr::ungroup() %>%
    dplyr::select(highest_correlation) %>%
    dplyr::distinct()
  return(dt)
}
