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
