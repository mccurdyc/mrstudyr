#' FUNCTION: read_sqlite_avmdefaults
#'
#' Read in a data file containing all of the mutation analysis data for sqlite avm defaults. This is
#' the file containing all of the mutation scores and timings for all schemas
#' and databases. THIS IS JUST SAMPLE DATA AND A SAMPLE READ FUNCTION.
#'
#' @export

read_sqlite_avmdefaults <- function(dat) {
  f <- system.file("extdata", "sqlite-avmdefaults.dat", package="mrstudyr")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}
