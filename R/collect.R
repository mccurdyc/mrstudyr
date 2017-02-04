#' FUNCTION: collect_study_schemas
#'
#' This function will reduce the data to only analyze the schemas discussed
#' and presented in the accompanying tool paper.
#' (CoffeeOrders, Employee, Inventory, Iso3166, JWhoisServer, MozillaPermissions, NistWeather, Person, Products)
#' @export

collect_study_schemas <- function(d) {
  schemas <- c("CoffeeOrders", "Employee", "Inventory", "Iso3166", "JWhoisServer", "MozillaPermissions", "NistWeather", "Person", "Products")
  dt <- d %>% dplyr::filter(schema %in% schemas)
  return(dt)
}

#' FUNCTION: collect_normal_data
#'
#' This function pulls all data with type equal to NORMAL from the original data.
#' @export

collect_normal_data <- function(d) {
  dt <- d %>% dplyr::filter(type %in% "NORMAL")
  return(dt)
}

#' FUNCTION: collect_schema_data
#'
#' Group data by dbms and schema
#' @export

collect_schema_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema)
  return(dt)
}

#' FUNCTION: collect_percent_data
#'
#' Group data by dbms and percentage
#' @export

collect_percent_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, percentage)
  return(dt)
}

#' FUNCTION: collect_operator_data
#'
#' Group data by dbms, schema and operator
#' @export

collect_operator_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema, operator)
  return(dt)
}


#' FUNCTION: collect_schema_percent_data
#'
#' Group data by dbms, schema and percentage
#' @export

collect_schema_percent_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema, percentage)
  return(dt)
}

#' FUNCTION: collect_schema_trial_data
#'
#' Group data by dbms, schema and trial
#' @export

collect_schema_trial_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema, trial)
  return(dt)
}

#' FUNCTION: collect_chosen_percent_data
#'
#' Collect the data for a user-specified percentage
#' @export

collect_chosen_percent_data <- function(d, i) {
  dt <- d %>% dplyr::filter(percentage %in% i)
  return(dt)
}

#' FUNCTION: collect_chosen_trial_data
#'
#' Collect the data for a user-specified trial
#' @export

collect_chosen_trial_data <- function(d, t) {
  dt <- d %>% dplyr::filter(trial %in% t)
  return(dt)
}

#' FUNCTION: collect_keep_data
#'
#' Filter data to only include data that is 'kept' i.e., a boolean value of 'TRUE' in the keep column.
#' @export

collect_keep_data <- function(d) {
  dt <- d %>% dplyr::filter(keep == TRUE)
  return(dt)
}

#' FUNCTION: collect_best_fit_data
#'
#' Filter the data where fitness equals the best fitness for a given schema.
#' @export

collect_best_fit_data <- function(d) {
  dt <- d %>% dplyr::filter(best_fit == fitness)
  return(dt)
}

#' FUNCTION: collect_best_keep_data
#'
#' Filter the collected keep data from all positions and filter where position was best fit.
#' @export

collect_best_keep_data <- function(b, k) {
  dt <- k %>% dplyr::filter(position == b$position)
  return(dt)
}

#' FUNCTION: collect_schema_step_data <- function()
#'
#' Given the best-fit data, gather the keep data for the respective position
#' for a given schema.
#' @export

collect_schema_position_data <- function(b, k) {
  dt <- k %>% dplyr::filter(schema == b$schema, position == b$position)
  return(dt)
}
