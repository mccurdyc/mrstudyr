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

#' FUNCTION: collect_schema_operator_data
#'
#' Group data by dbms, schema and operator
#' @export

collect_schema_operator_data <- function(d) {
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

#' FUNCTION: collect_trial_data
#'
#' Group data by trial.
#' @export

collect_trial_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, trial)
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

#' FUNCTION: collect_ignore_data
#'
#' Filter data to only include data that is 'ignored' i.e., a boolean value of 'FALSE' in the keep column.
#' @export

collect_ignore_data <- function(d) {
  dt <- d %>% dplyr::filter(keep == FALSE)
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

#' FUNCTION: collect_highest_correlation_data
#'
#' Filter the data where correlation equals highest correlation
#' @export

collect_highest_correlation_data <- function(d) {
  dt <- d %>% dplyr::filter(correlation == highest_correlation)
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

#' FUNCTION: collect_best_step_data
#'
#' Filter the collected keep data from all steps and filter where step had highest correlation.
#' @export

collect_best_step_data <- function(d, h) {
  dt <- d %>% dplyr::filter(step == h$step)
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

#' FUNCTION: collect_trial_operator_data
#'
#' Group data by dbms, trial and operator (not schema because this is across-schema) for generating a generalized model.
#' @export

collect_trial_operator_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, trial, operator)
  return(dt)
}

#' FUNCTION: collect_operator_data
#'
#' Group data by dbms and operator
#' @export

collect_operator_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, operator)
  return(dt)
}

#' FUNCTION: collect_technique_data
#'
#' Group by technique for summarization.
#' @export

collect_technique_data <- function(d) {
  dt <- d %>% dplyr::group_by(technique)
  return(dt)
}

#' FUNCTION: collect_technique_trial_data
#'
#' Group by technique and trial for summarization across schemas.
#' @export

collect_technique_trial_data <- function(d) {
  dt <- d %>% dplyr::group_by(technique, trial)
  return(dt)
}
