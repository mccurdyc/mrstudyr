#' FUNCTION: join_numerator_denominator_time_data
#'
#' Join together six tables containing the numerator and denominator counts as well as the mutation analysis time for the reduced and original mutant sets
#' @export

join_numerator_denominator_time_data <- function(rn, rd, on, od, rt, ot) {
  rnd <- dplyr::left_join(rn, rd, by = c("dbms" = "dbms", "schema" = "schema"))
  ron <- dplyr::left_join(rnd, on, by = c("dbms" = "dbms", "schema" = "schema"))
  rond <- dplyr::left_join(ron, od, by = c("dbms" = "dbms", "schema" = "schema"))
  rondr <- dplyr::left_join(rond, rt, by = c("dbms" = "dbms", "schema" = "schema"))
  rondt <- dplyr::left_join(rondr, ot, by = c("dbms" = "dbms", "schema" = "schema"))
  return(rondt)
}

#' FUNCTION: join_total_keep_ignore_data
#'
#' Join together the total, keep and ignore mutant data after performing across-schema hill-climbing.
#' @export

join_total_keep_ignore_data <- function(tc, kc, ic) {
  dt <- dplyr::left_join(tc, kc, by = c("dbms" = "dbms", "trial" = "trial", "operator" = "operator"))
  dt <- dplyr::left_join(dt, ic, by = c("dbms" = "dbms", "trial" = "trial", "operator" = "operator"))
  return(dt)
}

#' FUNCTION: join_summarized_keep_ignore_data
#'
#' Join the summarized, model data for percentage of mutants kept and ignored per-operator.
#' @export

join_summarized_keep_ignore_data <- function(a, b) {
  dt <- dplyr::left_join(a, b, by = c("dbms" = "dbms",  "operator" = "operator"))
  return(dt)
}
