#' FUNCTION: exclude_schema
#'
#' Exclude a single schema's data. This is for building the model and then applying it.
#' @export

exclude_schema <- function(d, s) {
  dt <- d %>% dplyr::filter(!(schema %in% s))
  return(dt)
}
