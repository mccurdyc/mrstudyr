# THESE FUNCTIONS HAVE BEEN COMBINED TO evaluate_reduction_technique
# #' FUNCTION: evaluate_random_sampling
# #'
# #' Evaluate the effectiveness of random sampling at a given percent threshold.
# #' @export
#
# evaluate_random_sampling <- function(d, i, j) {
#   original_data <- d %>% collect_schema_data()
#   reduced_data <- original_data %>% select_x_percent(i)
#
#   reduced_numerator <- reduced_data %>% transform_reduced_killed_count()
#   reduced_denominator <- reduced_data %>% transform_reduced_total_count()
#   original_numerator <-  original_data %>% transform_original_killed_count()
#   original_denominator <- original_data %>% transform_original_total_count()
#   reduced_time <- reduced_data %>% summarize_reduced_time()
#   original_time <- original_data %>% summarize_original_time()
#   dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
#   dt <- dt %>% transform_cost_reduction() %>%
#         transform_reduced_mutation_score() %>%
#         transform_original_mutation_score() %>%
#         transform_error() %>%
#         transform_add_percentage_trial((i * 100), j)
#   return(dt)
# }
#
# #' FUNCTION: evaluate_selective_random
# #'
# #' Evaluate the effectiveness of selective random at a given percent threshold (i), for a given set of operators, (o).
# #' @export
#
# evaluate_selective_random <- function(d, o, i, j) {
#   original_data <- d %>% collect_schema_data()
#   reduced_data <- original_data %>% select_operators(o) %>% select_x_percent_across_operators(i)
#
#   reduced_numerator <- reduced_data %>% transform_reduced_killed_count()
#   reduced_denominator <- reduced_data %>% transform_reduced_total_count()
#   original_numerator <-  original_data %>% transform_original_killed_count()
#   original_denominator <- original_data %>% transform_original_total_count()
#   reduced_time <- reduced_data %>% summarize_reduced_time()
#   original_time <- original_data %>% summarize_original_time()
#   dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
#   dt <- dt %>% transform_cost_reduction() %>%
#         transform_reduced_mutation_score() %>%
#         transform_original_mutation_score() %>%
#         transform_error() %>%
#         transform_add_percentage_trial((i * 100), j)
#   return(dt)
# }
#
# #' FUNCTION: evaluate_incremental
# #'
# #' Using keep column, calculate the original and reduced sets mutation score, error and execution time.
# #' @export
#
# evaluate_incremental <- function(d, step_number) {
#   original_data <- d %>% transform_keep()
#   reduced_data <- d %>% collect_keep_data()
#
#   reduced_numerator <- reduced_data %>% transform_reduced_killed_count()
#   reduced_denominator <- reduced_data %>% transform_reduced_total_count()
#   original_numerator <- original_data %>% transform_original_killed_count()
#   original_denominator <- original_data %>% transform_original_total_count()
#   reduced_time <- reduced_data %>% summarize_reduced_time()
#   original_time <- original_data %>% summarize_original_time()
#   dt <- join_numerator_denominator_time_data(reduced_numerator, reduced_denominator, original_numerator, original_denominator, reduced_time, original_time)
#   dt <- dt %>% transform_cost_reduction() %>%
#         transform_reduced_mutation_score() %>%
#         transform_original_mutation_score() %>%
#         transform_error() %>%
#         transform_add_step_number(step_number)
#   return(dt)
# }
#
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

