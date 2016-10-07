#' FUNCTION: visualize_random_sampling_mutation_scores
#'
#' Visualize mutation scores for the random sampling reduction technique
#' @export

visualize_random_sampling_mutation_scores <- function(d, trans = FALSE) {
  p <- visualize_plot_mutation_score(d)
  if (trans != FALSE) {
    name <- "../graphics/from-data/mutation_score_random_plot_trans.pdf"
  } else {
    name <- "../graphics/from-data/mutation_score_random_plot.pdf"
  }
  visualize_save_graphic(name, p, 8, 16)
  return(p)
}

#' FUNCTION: visualize_operator_sampling_mutation_scores
#'
#' Visualize mutation scores for the operator sampling reduction technique
#' @export

visualize_operator_sampling_mutation_scores <- function(d, trans = FALSE) {
  p <- visualize_plot_mutation_score(d)
  if (trans != FALSE) {
    name <- "../graphics/from-data/mutation_score_operator_plot_trans.pdf"
  } else {
    name <- "../graphics/from-data/mutation_score_operator_plot.pdf"
  }
  visualize_save_graphic(name, p, 8, 8)
  return(p)
}

#' FUNCTION: visualize_plot_mutation_score
#'
#' Produces a visualization of the reduced mutation
#' scores across all schemas at a specific percentage.
#'
#' @export

visualize_plot_mutation_score <- function(d, trans = FALSE) {
  if (trans != FALSE) {
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
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = schema, y = reduced_mutation_score)) +
    ggplot2::geom_boxplot(colour = "#073642", fill = "transparent") +
    ggplot2::facet_wrap(~ percentage, labeller = ggplot2::label_both) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::stat_summary(fun.y = mean, fill = "white", colour = "black", geom = "point", shape = 24, size = 1, show.legend = FALSE) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 20)) +
    ggplot2::theme(panel.background = element_rect(fill = "transparent",colour = NA)) +
    ggplot2::theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
    ggplot2::theme(panel.grid.major = element_blank()) +
    ggplot2::theme(panel.grid.minor = element_blank()) +
    ggplot2::theme(axis.title = element_text(colour = "#073642")) +
    ggplot2::theme(axis.text = element_text(colour = "#073642", size = 30)) +
    ggplot2::theme(axis.ticks = element_line(colour = "#073642")) +
    ggplot2::theme(panel.border = element_rect(colour = "#073642", size = 4)) +
    ggplot2::theme(strip.background = element_rect(fill="#073642")) +
    ggplot2::theme(strip.text = element_text(colour = "white")) +
    ggplot2::xlab("Schema") +
    ggplot2::ylab("Mutation Score")
    return(p)
  }
}

#' FUNCTION: visualize_save_graphic
#'
#' Saves the provided graphic to the provided name.
#' @export

visualize_save_graphic <- function(save_name, save_plot, h, w) {
  ggplot2::ggsave(save_name, save_plot, height = h, width = w)
  # ggplot2::ggsave(save_name, save_plot, height = h, width = w, device = cairo_pdf)
}
