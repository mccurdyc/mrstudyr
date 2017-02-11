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
  dt <- d %>% collect_operator_data() %>% select_x_percent(x)
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

#' FUNCTION: select_random_start_position_frac
#'
#' Might merge into other similar function, still thinking
#' @export

select_random_start_position_frac <- function(d) {
  r <- (sample(1:100[[1]], 1, TRUE)/100)
  print(r)
  dt <- (r * nrow(d)) %>% round()
  return(dt)
}
