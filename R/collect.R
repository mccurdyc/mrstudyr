#' FUNCTION: collect_study_schemas
#'
#' This function will reduce the data to only analyse the schemas discussed
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
  dt <- d %>% dplyr::filter(type == "NORMAL")
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

#' FUNCTION: collect_operator_data
#'
#' Group data by dbms, schema and operator
#' @export

collect_operator_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema, operator)
  return(dt)
}

#' FUNCTION: collect_percent_data
#'
#' Group data by dbms, schema and percentage
#' @export

collect_percent_data <- function(d) {
  dt <- d %>% dplyr::group_by(dbms, schema, percentage)
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
