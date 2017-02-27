#' FUNCTION: generate_operator_model
#'
#' This model describes which operators should be kept versus removed to stay strongly correlated
#' while reducing the mutant set.
#' @export

generate_operator_model <- function(d) {
  d <- d %>% collect_trial_operator_data()
  keep_count <- d %>% collect_keep_data() %>% dplyr::count() %>% transform_rename_keep()
  ignore_count <- d %>% collect_ignore_data() %>% dplyr::count() %>% transform_rename_ignore()
  total_count <- d %>% dplyr::count() %>% transform_rename_count()
  dt <- join_total_keep_ignore_data(total_count, keep_count, ignore_count)
  dt[is.na(dt)] <- 0
  dt <- dt %>% transform_add_percent_kept() %>% transform_add_percent_ignored()
  return(dt)
}

