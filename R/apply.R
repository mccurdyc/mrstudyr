#' FUNCTION: apply_operator_model
#'
#' Apply the model generated from performing hill climbing to a schema.
#' @export

apply_operator_model <- function(d, model) {
  dt <- d %>% collect_operator_data() %>% helper_apply_operator_model(model)
  dd <- evaluate_reduction_technique(d, dt)
  return(dd)
}

#' FUNCTION: apply_operator_model_per_schema
#'
#' Apply the model generated from performing hill climbing to each schema.
#' @export

apply_operator_model_per_schema <- function(d, model) {
  df <- data.frame()
  count <- 1
  schemas <- d %>% select_all_schemas()
  for (s in schemas[[1]]) {
    print(paste("current schema ", count, ": ", s))
    excluded_schema_data <- d %>% select_schema_data(s) # select only the data for the excluded schema
    for (j in 1:30) {
      # apply the model informing how many mutants to keep per operator from original data set
      dt <- excluded_schema_data %>% apply_operator_model(model) %>% transform_add_trial(j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
    count <- count + 1
  }
  return(df)
}
