#' FUNCTION: select_k_percent
#'
#' This function will be used to look at k% of all data.
#' This is referred to as uniform random sampling.
#'
#' @export

select_k_percent <- function(data, k) {
    frac_data <- dplyr::sample_frac(data, k)
    return(frac_data)
}
#' FUNCTION: select_percentage_across_operators
#'
#' This function will be used to generate a sequence of values between 0.01 and 1.00
#' that will be used to test the mutation score of each fraction of reduced set of mutants
#' on a per-operator basis. Additionally, we will use this data to calculate mutation scores
#' and costs across all operators for each percentage.
#'
#' @export

select_percentage_across_operators <- function(data, i) {
            # initiallize empty data frame
            d <- data.frame("identifier" = character(),
                            "dbms" = character(),
                            "schema" = character(),
                            "operator" = character(),
                            "type" = character(),
                            "killed" = character(),
                            "time" = integer())
            names(d) <- c("identifier",
                          "dbms",
                          "schema",
                          "operator",
                          "type",
                          "killed",
                          "time")

        # get the schema under observation
        sc <- select_unique_schemas(data)

        # for each operator
        operators <- select_individual_schema_data(data, sc) %>% select_unique_operators()
            # for each operator
            for(o in operators[[1]]) {
                # get data with specific schema and operator
                operator_data <- dplyr::filter(data, operator == o)
                # get a percentage of operator data
                reduced_operator_data <- select_k_percent(operator_data, i)

                # append observation (row) to end of data frame 'd'
                d <- rbind(d, reduced_operator_data)
                }
    return(d)
}

#' FUNCTION: select_unique_schemas
#'
#' This function will create a data frame of all of the unique schemas.
#' Given a input data set, it will return all unique schemas contained in the
#' original data frame.
#'
#' @export

select_unique_schemas <- function(data) {
        schemas <- dplyr::distinct(dplyr::select(data, schema))
    return(schemas)
}

#' FUNCTION: select_unique_percentages
#'
#' This function will create a data frame of all of the unique percentages.
#' It should return 1-100. I created this function because passing a sequence
#' to a function was not working. THIS COULD PROBABLY BE DELETED NOW.
#'
#' @export

select_unique_percentages <- function(data) {
    p <- dplyr::distinct(dplyr::select(data, percentage))
    return(p)
}

#' FUNCTION: select_unique_methods
#'
#' This function returns all unique values in the "method" column
#'
#' @export

select_unique_methods <- function(data) {
    p <- dplyr::distinct(dplyr::select(data, method))
    return(p)
}

#' FUNCTION: select_unique_operators
#'
#' This function returns all unique values in the "operator" column
#'
#' @export

select_unique_operators <- function(data) {
    p <- dplyr::distinct(dplyr::select(data, operator))
    return(p)
}

#' FUNCTION: select_empirical_study_schemas
#'
#' This function will reduce the data to only analyse the schemas discussed
#' and presented in the accompanying tool paper.
#' (CoffeeOrders, Employee, Inventory, Iso3166, JWhoisServer, MozillaPermissions, NistWeather, Person, Products)
#'
#' @export

select_empirical_study_schemas <- function(data) {
    schemas <- c("CoffeeOrders", "Employee", "Inventory", "Iso3166", "JWhoisServer", "MozillaPermissions",
                 "NistWeather", "Person", "Products")
    d <- dplyr::filter(data, schema %in% schemas)
    return(d)
}

#' FUNCTION: select_normal_data
#'
#' This function pulls all data with type equal to NORMAL from the original
#' data provided.
#'
#' @export

select_normal_data <- function(data) {
    d <- dplyr::filter(data, type == "NORMAL")
    return(d)
}

#' FUNCTION: select_individual_schema_data
#'
#' This function will create a data frame containing only the data
#' for a specific schema. The schema name will be passed as input
#' and the function will return only data for that particular schema.
#'
#' @export

select_individual_schema_data <- function(data, s) {
    d <- dplyr::filter(data, s == schema)
    return(d)
}

#' FUNCTION: select_individual_percent_data
#'
#' This function will create a data frame containing only the data
#' for a specific percentage. This function will be used in combination
#' with select_indiv_schema_data to overall create a data frame for each schema
#' and for each set of percentages. We will then use this to find the average of the
#' trials
#'
#' @export

select_individual_percent_data <- function(data, k) {
    d <- dplyr::filter(data, k  == percentage)
    return(d)
}

#' FUNCTION: select_individual_operator_data
#'
#' This function will create a data frame containing only the data
#' for a specific operator. This function will be used in combination
#' with other functions to test the effectiveness of random sampling over
#' mutation operators.
#'
#' @export

select_individual_operator_data <- function(data, op) {
    d <- dplyr::filter(data, op == operator)
    return(d)
}
