#' FUNCTION: evaluate_reduction_technique
#'
#' Evaluate the effectiveness of a reduction technique given the original (pre-reduction data) and the reduced data 
#' @export

evaluate_reduction_technique <- function(o, r) {
  reduced_numerator <- r %>% transform_reduced_killed_count()
  reduced_denominator <- r %>% transform_reduced_total_count()
  original_numerator <- o %>% transform_original_killed_count()
  original_denominator <- o %>% transform_original_total_count()
  reduced_time <- r %>% summarize_reduced_time()
  original_time <- o %>% summarize_original_time()
  dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
  dt <- dt %>% transform_cost_reduction() %>%
        transform_reduced_mutation_score() %>%
        transform_original_mutation_score() %>%
        transform_error()
  return(dt)
}

