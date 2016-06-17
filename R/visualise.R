#' FUNCTION: visualise_k_percent_mutation_score
#'
#' Produces a visualisation comparing k_percent and mutation score.
#' This function will take in a data frame that contains only data for a one schema.
#' This means that the data will need to be filtered before being passed
#' to this function. In order to only observe data for one schema first use
#' the select_individual_schema_data(data, "schema_name")
#'
#' @export

visualise_k_percent_mutation_score <- function(data) {
    n <- dplyr::filter(data, percentage < 100)
    h <- dplyr::filter(data, percentage == 100)
    p <- ggplot2::ggplot(n, ggplot2::aes(x = percentage, y = reduced_mutation_score, group = percentage)) +
    ggplot2::geom_hline(yintercept = h$reduced_mutation_score, colour="#990000", linetype="dashed") +
    ggplot2::geom_text(data = h, ggplot2::aes(label = "100%"), vjust = 2) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_x_continuous(breaks = round(seq(0, max(data$percentage), by = 10),1)) +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::ylab("Mutation Score") +
    ggplot2::xlab("Percentage (%)") +
    ggplot2::theme(title = ggplot2::element_text(size=10), legend.position = "top")
  return(p)
}
#' FUNCTION: visualise_k_percent_mutation_score_per_schema
#'
#' Produces a visualisation comparing k_percent and mutation scores for each schema.
#' As input it expects all of the data, not just data for a specific schema.
#'
#' @export

visualise_k_percent_mutation_score_per_schema <- function(data) {
    # d <- select_individual_schema_data(data, schema)
    # schemas <- c("ArtistSimilarity","CoffeeOrder","Products","UnixUsage","iTrust","WordNet")
    # schemas_data <- data.frame()
    # for (s in schemas) {
    #     sc <- select_individual_schema_data(data, s)
    #     rbind(schemas_data, sc)
    # }
    n <- dplyr::filter(data, percentage < 100)
    h <- dplyr::filter(data, percentage == 100)
    p <- ggplot2::ggplot(n, ggplot2::aes(x = percentage, y = reduced_mutation_score)) +
    ggplot2::geom_point() +
    # ggplot2::geom_boxplot() +
    ggplot2::scale_x_continuous(breaks = round(seq(0, max(data$percentage), by = 10),1)) +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::ylab("Mutation Score") +
    ggplot2::xlab("Percentage (%)") +
    ggplot2::theme(title = ggplot2::element_text(size=10), legend.position = "top") +
    ggplot2::facet_wrap(~schema, nrow = 5)
  return(p)
}

#' FUNCTION: visualise_percent_correlations
#'
#' A function displaying Kendall's tau_b correlation
#'
#' @export

visualise_percent_correlations <- function(data) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = percentage, y = correlation, group = percentage)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ggplot2::ylab("Correlation (Kendall's tau)") +
    ggplot2::xlab("Percentage (%)")
  return(p)
}

#' FUNCTION: visualise_percent_error
#'
#' A function displaying distances. This function will be able to display both root mean squared error
#' and mean absolute error.
#'
#' @export

visualise_percent_error <- function(data) {
    n <- dplyr::filter(data, percentage < 100)
    ggplot2::ggplot(n, ggplot2::aes(percentage, y = mean_absolute_error, color = Calculation)) +
    ggplot2::geom_point(ggplot2::aes(y = mean_absolute_error, col = "mae")) +
    ggplot2::geom_point(ggplot2::aes(y = root_mean_squared_error, col = "rmse")) +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ggplot2::ylab("Error") +
    ggplot2::xlab("Percentage (%)")
  # return(p)
}

#' FUNCTION: visualise_cost_versus_correlation
#'
#' Produces a visualisations comparing reduced cost (time in ms) of the reduced
#' set of mutants versus the correlation. This visualisation will help determine
#' whether or not obtaining a higher correlation is worth paying the cost.
#'
#' @export

visualise_cost_versus_correlation <- function(data) {
    # we need the correlation data for the y-axis
    correlation_data <- analyse_percents_correlation(data)

    p <- ggplot2::ggplot(data, ggplot2::aes(x = reduced_cost, y = correlation_data$correlation)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 5)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 5)) +
    ggplot2::ylab("Correlation") +
    ggplot2::xlab("Time (ms)") +
    ggplot2::theme(title = ggplot2::element_text(size=6), legend.position = "top")
  return(p)
}

#' FUNCTION: visualise_cost_versus_mutation_score
#'
#' Produces a visualisations comparing reduced cost (time in ms) of the reduced
#' set of mutants versus the mutation scores. This visualisation will help determine
#' whether or not obtaining a higher mutation score is worth paying the cost.
#'
#' @export

visualise_cost_versus_mutation_score <- function(data, s) {
    schema <- dplyr::filter(data, schema == s)
    p <- ggplot2::ggplot(schema, ggplot2::aes(x = reduced_cost, y = reduced_mutation_score)) +
    ggplot2::geom_point() +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::ylab("Mutation Score") +
    ggplot2::xlab("Time (ms)") +
    ggplot2::theme(title = ggplot2::element_text(size=10), legend.position = "top")
    # ggplot2::theme(title = ggplot2::element_text(size=20), legend.position = "top", plot.background = ggplot2::element_rect(fill = "#fdf6e3"))
  return(p)
}

#' FUNCTION: visualise_cost_versus_mean_absolute_error
#'
#' Produces a visualisations comparing reduced cost (time in ms) of the reduced
#' set of mutants versus the mean absolute error at that cost. This visualisation will help determine
#' whether or not obtaining a lower error is worth paying the cost.
#'
#' @export

visualise_cost_versus_mean_absolute_error <- function(data, s) {
    schema <- dplyr::filter(data, schema == s)
    p <- ggplot2::ggplot(schema, ggplot2::aes(x = reduced_cost, y = error)) +
    ggplot2::geom_point() +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
    ggplot2::ylab("Mean Absolute Error (%)") +
    ggplot2::xlab("Time (ms)") +
    ggplot2::theme(title = ggplot2::element_text(size=10), legend.position = "top")
    # ggplot2::theme(title = ggplot2::element_text(size=20), legend.position = "top", plot.background = ggplot2::element_rect(fill = "#fdf6e3"))
  return(p)
}

#' FUNCTION: visualise_cost_versus_root_mean_squared_error
#'
#' Produces a visualisations comparing reduced cost (time in ms) of the reduced
#' set of mutants versus the root mean squared error at that cost. This visualisation will help determine
#' whether or not obtaining a lower error is worth paying the cost.
#'
#' @export

visualise_cost_versus_root_mean_squared_error <- function(data, s) {
    schema <- dplyr::filter(data, schema == s)
    p <- ggplot2::ggplot(schema, ggplot2::aes(x = reduced_cost, y = error)) +
    ggplot2::geom_point() +
    ggplot2::scale_shape(guide = ggplot2::guide_legend(title = ""), solid = FALSE) +
    ggplot2::theme_grey(base_size = 6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 5)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1, size = 5)) +
    ggplot2::ylab("Root Mean Squared Error (%)") +
    ggplot2::xlab("Time (ms)") +
    ggplot2::theme(title = ggplot2::element_text(size=6), legend.position = "top")
  return(p)
}
