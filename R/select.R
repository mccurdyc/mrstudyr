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
