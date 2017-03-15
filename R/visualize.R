# #' FUNCTION: visualize_random_sampling_mutation_scores
# #'
# #' Visualize mutation scores for the random sampling reduction technique
# #' @export
#
# visualize_random_sampling_mutation_scores <- function(d) {
#   p <- d %>% visualize_plot_mutation_score()
#   name <- "../graphics/from-data/mutation_score_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_selective_random_mutation_scores
# #'
# #' Visualize mutation scores for the selective random reduction technique
# #' @export
#
# visualize_selective_random_mutation_scores <- function(d) {
#   p <- d %>% visualize_plot_mutation_score()
#   name <- "../graphics/from-data/mutation_score_selective_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_random_sampling_error
# #'
# #' Visualize error between original and reduced mutation scores for the random sampling reduction technique
# #' @export
#
# visualize_random_sampling_error <- function(d) {
#   p <- d %>% visualize_plot_error()
#   name <- "../graphics/from-data/error_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_selective_random_error
# #'
# #' Visualize error between original and reduced mutation scores for the selective random reduction technique
# #' @export
#
# visualize_selective_random_error <- function(d) {
#   p <- d %>% visualize_plot_error()
#   name <- "../graphics/from-data/error_selective_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_random_sampling_mae
# #'
# #' Visualize mean absolute error between original and reduced mutation scores for the random sampling reduction technique
# #' across thirty trials
# #' @export
#
# visualize_random_sampling_mae <- function(d) {
#   p <- d %>% visualize_plot_mae()
#   name <- "../graphics/from-data/mae_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_selective_random_mae
# #'
# #' Visualize the mean absolute error between original and reduced mutation scores for the selective random reduction technique
# #' across thirty trials
# #' @export
#
# visualize_selective_random_mae <- function(d) {
#   p <- d %>% visualize_plot_mae()
#   name <- "../graphics/from-data/mae_selective_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_random_sampling_rmse
# #'
# #' Visualize root mean squared error between original and reduced mutation scores for the random sampling reduction technique
# #' across thirty trials
# #' @export
#
# visualize_random_sampling_rmse <- function(d) {
#   p <- d %>% visualize_plot_rmse()
#   name <- "../graphics/from-data/rmse_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
# #' FUNCTION: visualize_selective_random_rmse
# #'
# #' Visualize the root mean squared error between original and reduced mutation scores for the selective random reduction technique
# #' across thirty trials
# #' @export
#
# visualize_selective_random_rmse <- function(d) {
#   p <- d %>% visualize_plot_rmse()
#   name <- "../graphics/from-data/rmse_selective_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }
#
#' FUNCTION: visualize_random_sampling_correlation
#'
#' Visualize correlation between original and reduced mutation scores for the random sampling reduction technique
#' across schemas across thirty trials.
#' @export

visualize_random_sampling_correlation <- function(d) {
  p <- d %>% visualize_plot_percentage_correlation()
  name <- "../graphics/from-data/correlation_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_random_sampling_cost_reduction
#'
#' Visualize cost reduction for the random sampling reduction technique at each percentage
#' across schemas across thirty trials.
#' @export

visualize_random_sampling_cost_reduction <- function(d) {
  p <- d %>% visualize_plot_percentage_cost_reduction()
  name <- "../graphics/from-data/cost_reduction_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

# #' FUNCTION: visualize_random_sampling_cost
# #'
# #' Visualize cost for the hill climbing reduction technique at each percentage
# #' across schemas across thirty trials.
# #' @export
#
# visualize_random_sampling_cost <- function(d) {
#   p <- d %>% visualize_plot_percentage_cost()
#   name <- "../graphics/from-data/cost_random_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }

#' FUNCTION: visualize_hill_climbing_correlation
#'
#' Visualize correlation between original and reduced mutation scores for the hill climbing reduction technique
#' across schemas across thirty trials.
#' @export

visualize_hill_climbing_correlation <- function(d) {
  p <- d %>% visualize_plot_correlation()
  name <- "../graphics/from-data/correlation_hill_climbing_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_hill_climbing_cost_reduction
#'
#' Visualize cost reduction for the hill climbing reduction technique at each percentage
#' across schemas across thirty trials.
#' @export

visualize_hill_climbing_cost_reduction <- function(d) {
  p <- d %>% visualize_plot_cost_reduction()
  name <- "../graphics/from-data/cost_reduction_hill_climbing_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

# #' FUNCTION: visualize_hill_climbing_cost
# #'
# #' Visualize cost for the hill climbing reduction technique at each percentage
# #' across schemas across thirty trials.
# #' @export
#
# visualize_hill_climbing_cost <- function(d) {
#   p <- d %>% visualize_plot_cost()
#   name <- "../graphics/from-data/cost_hill_climbing_plot.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }

#' FUNCTION: visualize_operator_mutant_costs_per_schema
#'
#' In a box-and-whisker plot, display the cost of all of the mutants for each operator facetted by schema.
#' @export

visualize_operator_mutant_costs_per_schema <- function(d) {
  p <- d %>% visualize_plot_operator_costs_per_schema()
  name <- "../graphics/from-data/operator_mutant_costs_per_schema.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_fractional_operator_mutant_costs
#'
#' In a bar chart, display the fractional cost of all of the mutants for each operator.
#' @export

visualize_fractional_operator_mutant_costs <- function(d) {
  p <- d %>% visualize_plot_fractional_operator_costs()
  name <- "../graphics/from-data/operator_mutant_fractional_costs.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_fractional_operator_mutant_costs_per_schema
#'
#' In a bar chart, display the fractional cost of all of the mutants for each operator facetted by schema.
#' @export

visualize_fractional_operator_mutant_costs_per_schema <- function(d) {
  p <- d %>% visualize_plot_fractional_operator_costs_per_schema()
  name <- "../graphics/from-data/operator_mutant_fractional_costs_per_schema.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_fractional_operator_mutant_costs
#'
#' In a bar chart, display the fractional frequencies of all of the mutants for each operator.
#' @export

visualize_fractional_operator_mutant_frequencies <- function(d) {
  p <- d %>% visualize_plot_fractional_operator_frequencies()
  name <- "../graphics/from-data/operator_mutant_fractional_frequencies.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_fractional_operator_mutant_frequencies_per_schema
#'
#' In a bar chart, display the fractional frequencies of all of the mutants for each operator facetted by schema.
#' @export

visualize_fractional_operator_mutant_frequencies_per_schema <- function(d) {
  p <- d %>% visualize_plot_fractional_operator_frequencies_per_schema()
  name <- "../graphics/from-data/operator_mutant_fractional_frequencies_per_schema.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

# #' FUNCTION: visualize_operator_mutant_costs
# #'
# #' In a box-and-whisker plot, display the cost of all of the mutants for each operator.
# #' @export
#
# visualize_operator_mutant_costs <- function(d) {
#   p <- d %>% visualize_plot_operator_costs()
#   name <- "../graphics/from-data/operator_mutant_costs.pdf"
#   visualize_save_graphic(name, p, 8, 8)
#   return(p)
# }

#' FUNCTION: visualize_plot_mutation_score
#'
#' Produces a visualization of the reduced mutation
#' scores across all schemas at a specific percentage.
#' @export

visualize_plot_mutation_score <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = reduced_mutation_score)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
  ggplot2::scale_y_continuous(limits = c(0, 100)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Schema") +
  ggplot2::ylab("Mutation Score")
  return(p)
}

#' FUNCTION: visualize_plot_error
#'
#' Produces a visualization of the error --- the difference --- between the original and reduced mutation
#' scores.
#' @export

visualize_plot_error <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = error)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Schema") +
  ggplot2::ylab("Error")
  return(p)
}

# #' FUNCTION: visualize_plot_mae
# #'
# #' Produces a visualization of the MAE --- the mean absolute error --- between the original and reduced mutation
# #' scores across thirty trials
# #' @export
#
# visualize_plot_mae <- function(d) {
#   p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = mae)) +
#   ggplot2::geom_point() +
#   ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
#   # ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
#   ggplot2::theme_bw(base_size = 10) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::xlab("Schema") +
#   ggplot2::ylab("Mean Absolute Error (MAE)")
#   return(p)
# }
#
# #' FUNCTION: visualize_plot_rmse
# #'
# #' Produces a visualization of the RMSE --- the root mean squared error --- between the original and reduced mutation
# #' scores across thirty trials
# #' @export
#
# visualize_plot_rmse <- function(d) {
#   p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = rmse)) +
#   ggplot2::geom_point() +
#   ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
#   # ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
#   ggplot2::theme_bw(base_size = 10) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::xlab("Schema") +
#   ggplot2::ylab("Root Mean Squared Error (RMSE)")
#   return(p)
# }

#' FUNCTION: visualize_plot_correlation
#'
#' Produces a visualization of the correlation between the original and reduced mutation score across
#' schemas for thirty trials.
#' @export

visualize_plot_correlation <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = step_size, y = correlation, group = step_size)) +
  ggplot2::geom_boxplot() +
  # ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Flipped Per Neighbor") +
  ggplot2::ylab("Kendall")
  return(p)
}

#' FUNCTION: visualize_plot_percentage_correlation
#'
#' Produces a visualization of the correlation between the original and reduced mutation score across
#' schemas for thirty trials.
#' @export

visualize_plot_percentage_correlation <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = percentage, y = correlation, group = percentage)) +
  ggplot2::geom_boxplot(width=5) +
  ggplot2::scale_x_continuous(breaks = round(seq(0, max(d$percentage), by = 10), 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Sampling Ratio (%)") +
  ggplot2::ylab("Kendall")
  return(p)
}

#' FUNCTION: visualize_plot_cost_reduction
#'
#' Produces a visualization of the cost reduction from the original and reduced mutant sets across
#' schemas for thirty trials.
#' @export

visualize_plot_cost_reduction <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = step_size, y = cost_reduction, group = step_size)) +
  ggplot2::geom_boxplot() +
  # ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Flipped Per Neighbor") +
  ggplot2::ylab("Fractional Cost Reduction")
  return(p)
}

#' FUNCTION: visualize_plot_cost
#'
#' Produces a visualization of the time to evaluate the mutants chosen for the reduced set.
#' @export

visualize_plot_cost <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = step_size, y = reduced_time, group = step_size)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Flipped Per Neighbor") +
  ggplot2::ylab("Time (ms)")
  return(p)
}

#' FUNCTION: visualize_plot_fractional_operator_costs
#'
#' Produces a visualization of the time to evaluate the mutants per-operator.
#' @export

visualize_plot_fractional_operator_costs <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = operator, y = fractional_cost)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::xlab("Operator") +
  ggplot2::ylab("Fractional Cost")
  return(p)
}

#' FUNCTION: visualize_plot_fractional_operator_costs_per_schema
#'
#' Produces a visualization of the time to evaluate the mutants per-operator, per-schema.
#' @export

visualize_plot_fractional_operator_costs_per_schema <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = operator, y = fractional_cost)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::facet_wrap(~ schema, labeller = ggplot2::label_both) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::xlab("Schema") +
  ggplot2::ylab("Fractional Cost of Operator")
  return(p)
}

#' FUNCTION: visualize_plot_fractional_operator_frequencies
#'
#' Produces a visualization of the frequencies of mutants per operator.
#' @export

visualize_plot_fractional_operator_frequencies <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = operator, y = fractional_frequency)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::xlab("Operator") +
  ggplot2::ylab("Fractional Frequency")
  return(p)
}

#' FUNCTION: visualize_plot_fractional_operator_frequencies_per_schema
#'
#' Produces a visualization of the frequencies of mutants per operator, per schema.
#' @export

visualize_plot_fractional_operator_frequencies_per_schema <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = operator, y = fractional_frequency)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::facet_wrap(~ schema, labeller = ggplot2::label_both) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::xlab("Operator") +
  ggplot2::ylab("Fractional Frequency")
  return(p)
}

#' FUNCTION: visualize_plot_percentage_cost
#'
#' Produces a visualization of the time to evaluate the mutants chosen for the reduced set.
#' @export

visualize_plot_percentage_cost <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = percentage, y = reduced_time, group = percentage)) +
  ggplot2::geom_boxplot(width=5) +
  ggplot2::scale_x_continuous(breaks = round(seq(0, max(d$percentage), by = 10), 1)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Evaluated") +
  ggplot2::ylab("Time (ms)")
  return(p)
}

#' FUNCTION: visualize_plot_percentage_cost_reduction
#'
#' Produces a visualization of the amount by which a random sample percentage decreases the cost of
#' performing mutation analysis.
#' @export

visualize_plot_percentage_cost_reduction <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = percentage, y = cost_reduction, group = percentage)) +
  ggplot2::geom_boxplot(width=5) +
  ggplot2::scale_x_continuous(breaks = round(seq(0, max(d$percentage), by = 10), 1)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Evaluated") +
  ggplot2::ylab("Cost Reduction")
  return(p)
}

# #' FUNCTION: visualize_plot_operator_costs_per_schema
# #'
# #' Produces a visualization of the costs of all mutants for an operator facetted by schema.
# #' @export
#
# visualize_plot_operator_costs_per_schema <- function(d) {
#   p <- ggplot2::ggplot(d, ggplot2::aes(x = operator, y = time)) +
#   ggplot2::geom_boxplot() +
#   ggplot2::facet_wrap(~ schema, labeller = ggplot2::label_both) +
#   ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
#   ggplot2::theme_bw(base_size = 10) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::xlab("Operator") +
#   ggplot2::ylab("Analysis Time (ms)")
#   return(p)
# }
#
# #' FUNCTION: visualize_plot_operator_costs
# #'
# #' Produces a visualization of the costs of all mutants for an operator.
# #' @export
#
# visualize_plot_operator_costs <- function(d) {
#   p <- ggplot2::ggplot(d, ggplot2::aes(x = operator, y = time)) +
#   ggplot2::geom_boxplot() +
#   ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
#   ggplot2::theme_bw(base_size = 10) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
#   ggplot2::xlab("Operator") +
#   ggplot2::ylab("Analysis Time (ms)")
#   return(p)
# }

#' FUNCTION: visualize_save_graphic
#'
#' Saves the provided graphic to the provided name.
#' @export

visualize_save_graphic <- function(save_name, save_plot, w, h) {
  ggplot2::ggsave(save_name, save_plot, width = w, height = h)
}
