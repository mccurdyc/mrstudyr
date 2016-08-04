#' FUNCTION: select_empirical_study_schemas
#'
#' This function will reduce the data to only analyse the schemas discussed
#' and presented in the accompanying tool paper.
#' (CoffeeOrders, Employee, Inventory, Iso3166, JWhoisServer, MozillaPermissions, NistWeather, Person, Products)
#' @export

select_empirical_study_schemas <- function(d) {
  schemas <- c("CoffeeOrders", "Employee", "Inventory", "Iso3166", "JWhoisServer", "MozillaPermissions", "NistWeather", "Person", "Products")
  dt <- d %>% dplyr::filter(schema %in% schemas)
  return(dt)
}

#' FUNCTION: select_x_percent
#'
#' This function will be used to look at a select percentage of the data provided.
#' This is referred to as uniform random sampling.
#' @export

select_x_percent <- function(d, x) {
  dt <- d %>% dplyr::group_by(dbms, schema) %>% dplyr::sample_frac(x)
  return(dt)
}

#' FUNCTION: select_distinct_dbms_schema
#'
#' Select only unique combinations of dbms and schema from sample data
#' @export

select_distinct_dbms_schema <- function(d) {
  dt <- d %>% dplyr::select(dbms, schema) %>% dplyr::distinct()
  return(dt)
}
