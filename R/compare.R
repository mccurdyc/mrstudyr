#' FUNCTION: compare_technique_group_head_to_head
#'
#' This function is responsible to comparing two technique groups head-to-head.
#' @export

compare_technique_group_head_to_head <- function(d, a, b) {
  # sub <- d %>% select_head_to_head_technique_groups(a, b) %>% collect_technique_data()
  # mean_corr <- sub %>% summarize_mean_correlation()
  # mean_cost_reduction <- sub %>% summarize_mean_cost_reduction()
  # joined_mean_corr_cost_reduction <- join_by_technique(mean_corr, mean_cost_reduction)
  #
  # visualize_mean_corr_cost_reduction_head_to_head(joined_mean_corr_cost_reduction)

  sub <- d %>% select_head_to_head_technique_groups(a, b) %>% collect_technique_data()
  mean_corr <- sub %>% summarize_mean_correlation()
  mean_cost_reduction <- sub %>% summarize_mean_cost_reduction()
  joined_mean_corr_cost_reduction <- join_by_technique(mean_corr, mean_cost_reduction)
  ratio <- joined_mean_corr_cost_reduction %>% transform_add_mean_correlation_cost_reduction_ratio()
  # visualize_ratio_head_to_head(ratio)
  return(ratio)
}

