#' FUNCTION: read_sqlite_avmdefaults
#'
#' Read in a data file containing all of the mutation analysis data for sqlite avmdefaults. This is
#' the file containing all of the mutation scores and timings for all schemas
#' and databases.
#' @export

read_sqlite_avmdefaults <- function(dat) {
  f <- system.file("extdata", "sqlite-avmdefaults.dat", package="mrstudyr")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}

#' FUNCTION: read_hypersql_avmdefault
#'
#' Read in a data file containing all of the mutation analysis data for the hypersql dbms.
#' @export

read_hypersql_avmdefaults <- function(dat) {
  f <- system.file("extdata", "hypersql-avmdefaults.dat", package="mrstudyr")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}

#' FUNCTION: read_postgres_avmdefault
#'
#' Read in a data file containing all of the mutation analysis data for the postgres dbms.
#' @export

read_postgres_avmdefaults <- function(dat) {
  f <- system.file("extdata", "postgres-avmdefaults.dat", package="mrstudyr")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}
