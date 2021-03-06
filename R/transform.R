#' FUNCTION: transform_killed_count
#'
#' Count the number of killed mutants
#' @export

transform_killed_count <- function(d) {
  dt <- d %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_numerator = n)
  return(dt)
}

#' FUNCTION: transform_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_total_count <- function(d) {
  dt <- d %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_denominator = n)
  return(dt)
}
#' FUNCTION: transform_replace_correlation
#'
#' Make sure that the correlation coeffient calculated by the 'Kendall' R package is renamed
#' from 'estimate' to 'correlation'
#' @export

transform_replace_correlation <- function(d) {
  dt <- d %>% dplyr::rename(correlation = estimate)
}

#' FUNCTION: transform_replace_technique
#'
#' Update the technique column to include the configuration of the technique also.
#' This makes comparing all techniques and configurations easier.
#' @export

transform_replace_technique <- function(d) {
  technique <- d$technique[[1]]
  if (technique == "RS") {
    dt <- d %>% dplyr::mutate(technique = paste(d$technique, d$percentage))
  }
  else if (technique == "SRS") {
    dt <- d %>% dplyr::mutate(technique = paste(d$technique, d$omitted_operators, d$percentage))
  }
  else if (technique == "SM") {
    dt <- d %>% dplyr::mutate(technique = paste(d$technique, d$omitted_operators))
  }
  else if (technique == "HC") {
    dt <- d %>% dplyr::mutate(technique = paste(d$technique, d$step_size))
  }
  return(dt)
}

#' FUNCTION: transform_add_technique_group
#'
#' Add a column to the combined technique data to include the abstract group.
#' @export

transform_add_technique_group <- function(d) {
  dt <- d %>% rowwise() %>% dplyr::mutate(technique_group = strsplit(technique, " ")[[1]][1])
  return(dt)
}

#' FUNCTION: transform_add_step_size
#'
#' Append the step size for the hill climbing technique.
#' @export

transform_add_step_size <- function(d, s) {
  dt <- d %>% dplyr::mutate(step_size = s)
  return(dt)
}

#' FUNCTION: transform_rename_count
#'
#' Rename column from generic name of 'n' to count.
#' @export

transform_rename_count <- function(d) {
  dt <- d %>% dplyr::rename(count = n)
}

#' FUNCTION: transform_rename_keep
#'
#' Rename column from generic name of 'n' to keep_count.
#' @export

transform_rename_keep <- function(d) {
  dt <- d %>% dplyr::rename(keep_count = n)
}

#' FUNCTION: transform_rename_ignore
#'
#' Rename column from generic name of 'n' to ignore_count.
#' @export

transform_rename_ignore <- function(d) {
  dt <- d %>% dplyr::rename(ignore_count = n)
}

#' FUNCTION: transform_add_trial
#'
#' Add the trial
#' @export

transform_add_trial <- function(d, t) {
  dt <- d %>% dplyr::mutate(trial = t)
  return(dt)
}

#' FUNCTION: transform_add_technique
#'
#' Add the techniques used
#' @export

transform_add_technique <- function(d, t) {
  dt <- d %>% dplyr::mutate(technique = t)
  return(dt)
}

#' FUNCTION: transform_add_position
#'
#' Add the position column
#' @export

transform_add_position <- function(d, t) {
  dt <- d %>% dplyr::mutate(position = t)
  return(dt)
}

#' FUNCTION: transform_add_start_position
#'
#' Add the start position column
#' @export

transform_add_start_position <- function(d, t) {
  dt <- d %>% dplyr::mutate(start_position = t)
  return(dt)
}

#' FUNCTION: transform_add_start_and_step
#'
#' Append the randomly-generated start position and appropriate step size
#' @export

transform_add_start_and_step <- function(d, s, f) {
  dt <- d %>% do(dplyr::mutate(., position = select_start_position(., f))) %>%
    do(dplyr::mutate(., start_position = select_start_position(., f))) %>%
    do(dplyr::mutate(., step_size = select_step_size(., s))) %>%
    dplyr::ungroup() # has to be separate from first mutate; causes errors otherwise
  return(dt)
}

#' FUNCTION: transform_add_omitted_operators
#'
#' Add the operators that were omitted from selective random sampling
#' @export

transform_add_omitted_operators <- function(d, o, oo) {
  if (o == oo) {
    dt <- d %>% dplyr::mutate(omitted_operators = o)
  }
  else {
    s <- paste(o, oo)
    dt <- d %>% dplyr::mutate(omitted_operators = s)
  }
  return(dt)
}

#' FUNCTION: transform_update_position
#'
#' Update the position after the flip
#' @export

transform_update_position <- function(d, p) {
  dt <- d %>% do(dplyr::mutate(., position = p))
  return(dt)
}

#' FUNCTION: transform_add_percentage_trial
#'
#' Add the analyzed percentage and current trial to data frame
#' @export

transform_add_percentage_trial <- function(d, p, t) {
  dt <- d %>% dplyr::mutate(percentage = p)
  dt <- dt %>% dplyr::mutate(trial = t)
  return(dt)
}

#' FUNCTION: transform_reduced_killed_count
#'
#' Count the number of killed mutants
#' @export

transform_reduced_killed_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(reduced_numerator = n)
  return(dt)
}

#' FUNCTION: transform_reduced_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_reduced_total_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(reduced_denominator = n)
  return(dt)
}

#' FUNCTION: transform_original_killed_count
#'
#' Count the number of killed mutants
#' @export

transform_original_killed_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_numerator = n)
  return(dt)
}

#' FUNCTION: transform_original_total_count
#'
#' Count the total number of mutants for a set
#' @export

transform_original_total_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::filter(killed %in% c("true", "false")) %>% dplyr::count()
  dt <- dt %>% dplyr::rename(original_denominator = n)
  return(dt)
}

#' FUNCTION: transform_mutant_count
#'
#' Count the total number of mutants associated with a schema
#' @export

transform_mutant_count <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(mutant_count = n())
  return(dt)
}

#' FUNCTION: transform_cost_reduction
#'
#' Calculate the reduction in time for performing mutation analysis on the reduced set compared to the original set
#' @export

transform_cost_reduction <- function(d) {
  dt <- d %>% dplyr::mutate(cost_reduction = ((original_time - reduced_time) / (original_time)))
  return(dt)
}

#' FUNCTION: transform_fractional_operator_cost
#'
#' Calculate the fractional cost of each operator per schema.
#' @export

transform_fractional_operator_cost <- function(d) {
  dt <- d %>% dplyr::mutate(fractional_cost = (operator_time / original_time))
  return(dt)
}

#' FUNCTION: transform_fractional_operator_frequencies
#'
#' Calculate the fractional frequencies of each operator per schema.
#' @export

transform_fractional_operator_frequencies <- function(d) {
  dt <- d %>% dplyr::mutate(fractional_frequency = (operator_frequencies / count))
  return(dt)
}

#' FUNCTION: transform_reduced_mutation_score
#'
#' Calculate the mutation score for a given set (number of killed mutants / total number of mutants)
#' @export

transform_reduced_mutation_score <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(reduced_mutation_score = ((reduced_numerator / reduced_denominator)))
  return(dt)
}

#' FUNCTION: transform_original_mutation_score
#'
#' Calculate the mutation score for the original set (number of killed mutants / total number of mutants)
#' @export

transform_original_mutation_score <- function(d) {
  dt <- d %>% collect_schema_data() %>% dplyr::mutate(original_mutation_score = ((original_numerator / original_denominator)))
  return(dt)
}

#' FUNCTION: transform_mutation_score
#'
#' Calculate the mutation score for a set (number of killed mutants / total number of mutants)
#' @export

transform_mutation_score <- function(d) {
  dt <- d %>% dplyr::mutate(original_mutation_score = ((original_numerator / original_denominator)))
  return(dt)
}

#' FUNCTION: transform_error
#'
#' Calculate the error --- the difference --- between the original mutation score and the reduced mutation score.
#' @export

transform_error <- function(d) {
  dt <- d %>% dplyr::mutate(error = abs((original_mutation_score - reduced_mutation_score)))
  return(dt)
}

#' FUNCTION: transform_mae
#'
#' Calculate Mean Absolute Error (mae)
#' @export

transform_mae <- function(d) {
  dt <- d %>% dplyr::mutate(mae = mean(abs(error)))
  return(dt)
}

#' FUNCTION: transform_rmse
#'
#' Calculate Root Mean Squared Error (rmse)
#' @export

transform_rmse <- function(d) {
  dt <- d %>% dplyr::mutate(rmse = sqrt(mean((error)^2)))
  return(dt)
}

#' FUNCTION: transform_keep
#'
#' This column has values representing whether or not a mutant is considered when calculating mutation score
#' as well as other metrics.
#' @export

transform_keep <- function(d, all=TRUE) {
  s <- c(TRUE, FALSE)

  if (all == TRUE) {
    dt <- d %>% dplyr::mutate(keep = TRUE)
  } else {
    dt <- d %>% dplyr::mutate(keep = sample(s, 1, size = nrow(d)))
  }
  return(dt)
}

# #' FUNCTION: transform_fitness
# #'
# #' Calculate the "fitness" of a step.
# #' @export
#
# transform_fitness <- function(d, error_weight, cost_weight) {
#   d <- d %>% collect_schema_data()
#   dt <- d %>% dplyr::mutate(fitness = ((cost_weight * cost_reduction) - (error_weight * error)))
#   return(dt)
# }

#' FUNCTION: transform_add_correlation
#'
#' Calculate the correlation of a step.
#' @export

transform_add_correlation <- function(d) {
  dt <- d %>% dplyr::mutate(correlation = calculate_correlation(d))
  return(dt)
}

#' FUNCTION: transform_add_step
#'
#' Simply add the current step to the dataframe
#' @export

transform_add_step <- function(d, s) {
  dt <- d %>% dplyr::ungroup() %>% dplyr::mutate(step = s)
  return(dt)
}

#' FUNCTION: transform_highest_correlation
#'
#' Determine which step is has the highest correlation
#' @export

transform_highest_correlation <- function(d) {
  d %>% collect_schema_data() %>%
    dplyr::mutate(highest_correlation = max(correlation))
}

#' FUNCTION: transform_add_percent_kept
#'
#' Add the percentage of kept mutants kept per-operator.
#' @export

transform_add_percent_kept <- function(d) {
  dt <- d %>% dplyr::mutate(percent_kept = (keep_count / count))
  return(dt)
}

#' FUNCTION: transform_add_percent_ignored
#'
#' Add the percentage of kept mutants ignored per-operator.
#' @export

transform_add_percent_ignored <- function(d) {
  dt <- d %>% dplyr::mutate(percent_ignored = (ignore_count / count))
  return(dt)
}

#' FUNCTION: transform_add_significance
#'
#' Append column with the boolean of whether the result found was significant or not.
#' @export

transform_add_significance <- function(d) {
  dt <- d %>% rowwise() %>% dplyr::mutate(significant = ranked_sum_interpret(p.value))
  return(dt)
}

#' FUNCTION: transform_add_mean_correlation_cost_reduction_ratio
#'
#' Calculate the correlation, cost reduction ratio (HIB / LIB) metric.
#' @export

transform_add_mean_correlation_cost_reduction_ratio <- function(d) {
  dt <- d %>% rowwise() %>% dplyr::mutate(ratio = (mean_correlation / (1 - mean_cost_reduction)))
  return(dt)
}

#' FUNCTION: transform_update_hill_climbing_applied_operator_data
#'
#' This function will append the necessary columns to make it match the data from actually performing
#' the hill climbing technique versus the model.
#' @export

transform_update_hill_climbing_applied_operator_data <- function(d, s) {
  dt <- d %>% transform_add_technique(paste("HC", s)) %>%
              transform_add_technique_group() %>%
              transform_add_step_size(s)
  return(dt)
}
