#' FUNCTION: create_results_tables_per_schema
#'
#' Create LaTeX tables of data for each schema.
#' @export

create_results_tables_per_schema <- function(d) {
  ds <- split(d, list(d$schema))
  dt <- ds %>% lapply(print_table)
}

#' FUNCTION: print_table
#'
#' The function that actually prints the table with the appropriate name to a file.
#' @export

print_table <- function(d) {
  dt <- d %>% xtable
  schema <- d$schema %>% unique() %>% toString()
  print(dt, include.colnames=FALSE, include.rownames=FALSE, row.names = FALSE, file = paste("../tables/", schema, "-results.tex", sep=""))
}
