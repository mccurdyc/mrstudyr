#' FUNCTION: read_data
#'
#' Read in a data file containing all of the mutation analysis data. This is
#' the file containing all of the mutation scores and timings for all schemas
#' and databases.
#'
#' @export

read_data <- function(dat) {
  f <- system.file("extdata", dat, package="selectms")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}
