#' FUNCTION: join_numerator_denominator_time_data
#'
#' DESCRIPTION: Join together six tables containing the numerator and denominator counts as well as the mutation analysis time for the reduced and original mutant sets
#' @export

join_numerator_denominator_time_data <- function(rn, rd, on, od, rt, ot) {
  rnd <- dplyr::left_join(rn, rd, by = c("dbms" = "dbms", "schema" = "schema"))
  ron <- dplyr::left_join(rnd, on, by = c("dbms" = "dbms", "schema" = "schema"))
  rond <- dplyr::left_join(ron, od, by = c("dbms" = "dbms", "schema" = "schema"))
  rondr <- dplyr::left_join(rond, rt, by = c("dbms" = "dbms", "schema" = "schema"))
  rondt <- dplyr::left_join(rondr, ot, by = c("dbms" = "dbms", "schema" = "schema"))
  return(rondt)
}
