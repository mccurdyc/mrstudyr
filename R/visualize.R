#' FUNCTION: visualize_random_sampling_mutation_scores
#'
#' Visualize mutation scores for the random sampling reduction technique
#' @export

visualize_random_sampling_mutation_scores <- function(d) {
  p <- d %>% visualize_plot_mutation_score()
  name <- "../graphics/from-data/mutation_score_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_selective_random_mutation_scores
#'
#' Visualize mutation scores for the selective random reduction technique
#' @export

visualize_selective_random_mutation_scores <- function(d) {
  p <- d %>% visualize_plot_mutation_score()
  name <- "../graphics/from-data/mutation_score_selective_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_random_sampling_error
#'
#' Visualize error between original and reduced mutation scores for the random sampling reduction technique
#' @export

visualize_random_sampling_error <- function(d) {
  p <- d %>% visualize_plot_error()
  name <- "../graphics/from-data/error_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_selective_random_error
#'
#' Visualize error between original and reduced mutation scores for the selective random reduction technique
#' @export

visualize_selective_random_error <- function(d) {
  p <- d %>% visualize_plot_error()
  name <- "../graphics/from-data/error_selective_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_random_sampling_mae
#'
#' Visualize mean absolute error between original and reduced mutation scores for the random sampling reduction technique
#' across thirty trials
#' @export

visualize_random_sampling_mae <- function(d) {
  p <- d %>% visualize_plot_mae()
  name <- "../graphics/from-data/mae_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_selective_random_mae
#'
#' Visualize the mean absolute error between original and reduced mutation scores for the selective random reduction technique
#' across thirty trials
#' @export

visualize_selective_random_mae <- function(d) {
  p <- d %>% visualize_plot_mae()
  name <- "../graphics/from-data/mae_selective_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_random_sampling_rmse
#'
#' Visualize root mean squared error between original and reduced mutation scores for the random sampling reduction technique
#' across thirty trials
#' @export

visualize_random_sampling_rmse <- function(d) {
  p <- d %>% visualize_plot_rmse()
  name <- "../graphics/from-data/rmse_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_selective_random_rmse
#'
#' Visualize the root mean squared error between original and reduced mutation scores for the selective random reduction technique
#' across thirty trials
#' @export

visualize_selective_random_rmse <- function(d) {
  p <- d %>% visualize_plot_rmse()
  name <- "../graphics/from-data/rmse_selective_random_plot.pdf"
  visualize_save_graphic(name, p, 8, 8)
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

#' FUNCTION: visualize_plot_mae
#'
#' Produces a visualization of the MAE --- the mean absolute error --- between the original and reduced mutation
#' scores across thirty trials
#' @export

visualize_plot_mae <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = mae)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
  # ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Schema") +
  ggplot2::ylab("Mean Absolute Error (MAE)")
  return(p)
}

#' FUNCTION: visualize_plot_rmse
#'
#' Produces a visualization of the RMSE --- the root mean squared error --- between the original and reduced mutation
#' scores across thirty trials
#' @export

visualize_plot_rmse <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = rmse)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
  # ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
  ggplot2::xlab("Schema") +
  ggplot2::ylab("Root Mean Squared Error (RMSE)")
  return(p)
}

#' FUNCTION: visualize_save_graphic
#'
#' Saves the provided graphic to the provided name.
#' @export

visualize_save_graphic <- function(save_name, save_plot, w, h) {
  ggplot2::ggsave(save_name, save_plot, width = w, height = h)
}
