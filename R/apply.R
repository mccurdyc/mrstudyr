#' FUNCTION: apply_operator_model
#'
#' Apply the model generated from performing hill climbing to a schema.
#' @export

apply_operator_model <- function(d, model) {
  dt <- d %>% collect_operator_data() %>% helper_apply_operator_model(model)
  dd <- evaluate_reduction_technique(d, dt)
  return(dd)
}
