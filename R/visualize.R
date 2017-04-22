#' FUNCTION: visualize_original_mutation_score_per_schema
#'
#' Summary graphic showing the original mutation scores per schema.
#' @export

visualize_original_mutation_score_per_schema <- function(d) {
  p <- d %>% visualize_plot_original_mutation_score()
  name <- "../graphics/from-data/mutation_score_per_schema.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_original_mutation_score_per_dbms
#'
#' Summary graphic showing the original mutation scores per dbms.
#' @export

visualize_original_mutation_score_per_dbms <- function(d) {
  p <- d %>% visualize_plot_original_mutation_score_per_dbms()
  name <- "../graphics/from-data/mutation_score_per_dbms.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_mutant_counts_per_schema
#'
#' Summary graphic showing the mutant counts per schema.
#' @export

visualize_mutant_counts_per_schema <- function(d) {
  p1 <- d %>% visualize_plot_mutant_counts_per_schema()
  name1 <- "../graphics/from-data/mutant_counts_per_schema.pdf"
  visualize_save_graphic(name1, p1, 8, 8)

  p2 <- d %>% visualize_plot_mutant_counts_per_schema_pres()
  name2 <- "../graphics/from-data/mutant_counts_per_schema_pres.pdf"
  visualize_save_graphic(name2, p2, 12, 6)
}

#' FUNCTION: visualize_correlation_all
#'
#' Summary graphic showing the correlation for all techniques
#' @export

visualize_correlation_all <- function(d) {
  p <- d %>% visualize_plot_correlation_all_box()
  name <- "../graphics/from-data/correlation_all.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_correlation_selective_random_sampling
#'
#' Summary graphic showing the correlation for selective random sampling
#' @export

visualize_correlation_selective_random_sampling <- function(d) {
  p <- d %>% visualize_plot_correlation_all_box()
  name <- "../graphics/from-data/correlation_srs.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_correlation_selective
#'
#' Summary graphic showing the correlation for selective.
#' @export

visualize_correlation_selective <- function(d) {
  p <- d %>% visualize_plot_correlation_all_box()
  name <- "../graphics/from-data/correlation_selective.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_cost_reduction_selective_random_sampling
#'
#' Summary graphic showing the cost reduction for selective random sampling
#' @export

visualize_cost_reduction_selective_random_sampling <- function(d) {
  p <- d %>% visualize_plot_cost_reduction_all()
  name <- "../graphics/from-data/cost_reduction_srs.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_cost_reduction_selective_random_sampling
#'
#' Summary graphic showing the cost reduction for selective mutation
#' @export

visualize_cost_reduction_selective <- function(d) {
  p <- d %>% visualize_plot_cost_reduction_all()
  name <- "../graphics/from-data/cost_reduction_selective.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_correlation_all_groups
#'
#' Summary graphic comapring the aggregated techniques.
#' @export

visualize_correlation_all_groups <- function(d) {
  p <- d %>% visualize_plot_correlation_all_groups()
  name <- "../graphics/from-data/correlation_all_groups.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_cost_reduction_all_groups
#'
#' Summary graphic comapring the aggregated techniques.
#' @export

visualize_cost_reduction_all_groups <- function(d) {
  p <- d %>% visualize_plot_cost_reduction_all_groups()
  name <- "../graphics/from-data/cost_reduction_all_groups.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_random_sampling_correlation
#'
#' Visualize correlation between original and reduced mutation scores for the random sampling reduction technique
#' across schemas across thirty trials.
#' @export

visualize_random_sampling_correlation <- function(d) {
  p <- d %>% visualize_plot_percentage_correlation()
  name <- "../graphics/from-data/all_dbms_correlation_random_plot.pdf"
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

#' FUNCTION: visualize_hill_climbing_correlation_all_dbms
#'
#' Visualize correlation between original and reduced mutation scores for the hill climbing reduction technique
#' across schemas across thirty trials for all DBMSs.
#' @export

visualize_hill_climbing_correlation_all_dbms <- function(d) {
  p <- d %>% visualize_plot_correlation_multiple_dbms()
  name <- "../graphics/from-data/all_dbms_correlation_hill_climbing_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_hill_climbing_cost_reduction_all_dbms
#'
#' Visualize the cost reduction for all of the DBMSs when comparing the operator models.
#' @export

visualize_hill_climbing_cost_reduction_all_dbms <- function(d) {
  p <- d %>% visualize_plot_cost_reduction_multiple_dbms()
  name <- "../graphics/from-data/all_dbms_cost_reduction_hill_climbing_plot.pdf"
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

#' FUNCTION: visualize_effectsize
#'
#' Effect size graph comparing all reduction techniques' correlation.
#' @export

visualize_effectsize <- function(d) {
  p <- d %>% visualize_plot_effectsize()
  name <- "../graphics/from-data/14_costreduction_effectsize.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_wilcoxon
#'
#' Wilcoxon ranked sum graph comparing all reduction techniques' correlation or cost reduction.
#' @export

visualize_wilcoxon <- function(d) {
  p <- d %>% visualize_plot_wilcoxon()
  name <- "../graphics/from-data/14_correlation_wilcoxon.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_mean_corr_cost_reduction_head_to_head
#'
#' Dot plot comparing the mean correlation and cost reduction for two techniques.
#' @export

visualize_mean_corr_cost_reduction_head_to_head <- function(d) {
  p <- d %>% visualize_plot_mean_corr_cost_reduction()
  name <- "../graphics/from-data/all_dbms_mean_correlation_cost_reduction_rs_to_hc.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_corr_cost_reduction_head_to_head
#'
#' Box plot comparing the correlation and cost reduction for two techniques.
#' @export

visualize_ratio_head_to_head <- function(d) {
  p <- d %>% visualize_plot_ratio()
  name <- "../graphics/from-data/ratio_rs_to_hc.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_plot_original_mutation_score
#'
#' Produces the visualization of the original mutation scores.
#' @export

visualize_plot_original_mutation_score <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = original_mutation_score, group = interaction(dbms, schema))) +
  ggplot2::geom_bar(stat="identity", position="dodge", ggplot2::aes(fill = dbms)) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Schema") +
  ggplot2::ylab("Original Mutation Score")
  return(p)
}

#' FUNCTION: visualize_plot_original_mutation_score_per_dbms
#'
#' Produces the visualization of the original mutation scores per dbms.
#' @export

visualize_plot_original_mutation_score_per_dbms <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = dbms, y = original_mutation_score)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::xlab("DBMS") +
  ggplot2::ylab("Original Mutation Score")
  return(p)
}

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

#' FUNCTION: visualize_plot_correlation_mulitple_dbms
#'
#' Produces a visualization of the correlation between the original and reduced mutation score across
#' schemas for thirty trials.
#' @export

visualize_plot_correlation_multiple_dbms <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = step_size, y = correlation, group = interaction(dbms, step_size))) +
  ggplot2::geom_boxplot(ggplot2::aes(colour = dbms)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", ggplot2::aes(shape = dbms), size = 2, show.legend = TRUE) +
  ggplot2::scale_shape(solid = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Flipped Per Neighbor") +
  ggplot2::ylab("Kendall")
  return(p)
}

#' FUNCTION: visualize_plot_cost_reduction_mulitple_dbms
#'
#' Produces a visualization of the cost reduction values of the operator model applied to multiple DBMSs.
#' @export

visualize_plot_cost_reduction_multiple_dbms <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = step_size, y = cost_reduction, group = interaction(dbms, step_size))) +
  ggplot2::geom_boxplot(ggplot2::aes(colour = dbms)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", ggplot2::aes(shape = dbms), size = 2, show.legend = TRUE) +
  ggplot2::scale_shape(solid = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Percentage of Mutants Flipped Per Neighbor") +
  ggplot2::ylab("Fractional Cost Reduction")
  return(p)
}

#' FUNCTION: visualize_plot_correlation_all_box
#'
#' @export

visualize_plot_correlation_all_box <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = technique, y = correlation)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Reduction Technique") +
  ggplot2::ylab("Kendall")
  return(p)
}

#' FUNCTION: visualize_plot_correlation_all_groups
#'
#' @export

visualize_plot_correlation_all_groups <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = technique_group, y = correlation, group = technique_group)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Reduction Technique") +
  ggplot2::ylab("Kendall")
  return(p)
}

#' FUNCTION: visualize_plot_cost_reduction_all_groups
#'
#' @export

visualize_plot_cost_reduction_all_groups <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = technique_group, y = cost_reduction, group = technique_group)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Reduction Technique") +
  ggplot2::ylab("Fractional Cost Reduction")
  return(p)
}

#' FUNCTION: visualize_plot_correlation_all_bar
#'
#' @export

visualize_plot_correlation_all_bar <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = technique, y = correlation)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Reduction Technique") +
  ggplot2::ylab("Kendall")
  return(p)
}

#' FUNCTION: visualize_plot_percentage_correlation
#'
#' Produces a visualization of the correlation between the original and reduced mutation score across
#' schemas for thirty trials.
#' @export

visualize_plot_percentage_correlation <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = percentage, y = correlation, group = interaction(dbms, percentage))) +
  ggplot2::geom_boxplot(width=5, ggplot2::aes(colour = dbms)) +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", ggplot2::aes(shape = dbms), size = 2, show.legend = TRUE) +
  ggplot2::scale_shape(solid = FALSE) +
  ggplot2::scale_x_continuous(breaks = round(seq(0, max(d$percentage), by = 10), 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
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
  ggplot2::theme_bw(base_size = 5) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
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
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
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
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
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
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
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

#' FUNCTION: visualize_plot_cost_reduction_all
#'
#' Plot fractional cost reduction for each technique.
#' @export

visualize_plot_cost_reduction_all <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = technique, y = cost_reduction, group = technique)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Reduction Technique") +
  ggplot2::ylab("Fractional Cost Reduction")
  return(p)
}

#' FUNCTION: visualize_plot_effectsize
#'
#' Plot effect size of all pairs of reduction techniques.
#' @export

visualize_plot_effectsize <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = group1, y = group2)) +
  ggplot2::geom_tile(ggplot2::aes(fill = size)) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Technique 1") +
  ggplot2::ylab("Technique 2")
  return(p)
}

#' FUNCTION: visualize_plot_wilcoxon
#'
#' Plot ranked sum of all pairs of reduction techniques.
#' @export

visualize_plot_wilcoxon <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = group1, y = group2)) +
  ggplot2::geom_tile(ggplot2::aes(fill = significant)) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Technique 1") +
  ggplot2::ylab("Technique 2")
  return(p)
}

#' FUNCTION: visualize_plot_mean_corr_cost_reduction
#'
#' Produces a dot plot comparing the mean correlation and cost reduction for techniques.
#' @export

visualize_plot_mean_corr_cost_reduction <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = mean_correlation, y = mean_cost_reduction, color)) +
  # p <- ggplot2::ggplot(d, ggplot2::aes(x = mean_correlation, y = mean_cost_reduction, label = technique)) +
  ggplot2::geom_point(ggplot2::aes(shape = factor(dbms), colour = factor(technique))) +
  ggplot2::scale_shape(solid = FALSE) +
  # ggplot2::geom_text(vjust = -1) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Mean Kendall") +
  ggplot2::ylab("Mean Fractional Cost Reduction")
  return(p)
}

#' FUNCTION: visualize_plot_ratio
#'
#' Produces a box plot comparing ratio values
#' @export

visualize_plot_ratio <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = technique, y = ratio, group = technique)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Technique") +
  ggplot2::ylab("Mean Kendall / (1 - Mean Cost Reduction)")
  return(p)
}

#' FUNCTION: visualize_plot_mutant_counts_per_schema
#'
#' Produces a visualization for the presentation of the mutant counts, per-schema.
#' @export

visualize_plot_mutant_counts_per_schema <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = original_denominator, interaction(dbms, schema))) +
    ggplot2::theme(strip.background = element_blank(), panel.border = element_rect(colour = "black")) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::xlab("Database Schema") +
    ggplot2::ylab("Total Number of Mutants")
  return(p)
}

#' FUNCTION: visualize_plot_mutant_counts_per_schema_pres
#'
#' Produces a visualization of the mutant counts, per-schema.
#' @export

visualize_plot_mutant_counts_per_schema_pres <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = original_denominator, interaction(dbms, schema))) +
  ggplot2::geom_bar(stat="identity", fill = "#268BD2") +
  # ggplot2::geom_bar(stat="identity", ggplot2::aes(fill=(d$schema == 'iTrust'))) +
  # ggplot2::scale_fill_manual(values = c('#268BD2', '#FFA500')) +
  # ggplot2::geom_bar(stat="identity", position="dodge", ggplot2::aes(fill=dbms)) +
  ggplot2::theme_bw(base_size = 8) +
  ggplot2::theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), legend.position="none") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 15)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 15)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25)) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 25)) +
  ggplot2::theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
  ggplot2::theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
  ggplot2::xlab("Database Schema") +
  ggplot2::ylab("Total Number of Mutants")
  return(p)
}

#' FUNCTION: visualize_save_graphic
#'
#' Saves the provided graphic to the provided name.
#' @export

visualize_save_graphic <- function(save_name, save_plot, w, h) {
  ggplot2::ggsave(save_name, save_plot, width = w, height = h)
}
