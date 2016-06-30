#' FUNCTION: analyse
#'
#' This function will encompass all of the reduction techniques, returning
#' a single dataframe with the data from performing all techniques.
#'
#' @export

analyse <- function(data) {
    d <- data.frame("method" = character(),
                    "schema" = character(),
                    "trial" = integer(),
                    "percentage" = integer(),
                    "reduced_numerator" = integer(),
                    "reduced_denominator" = integer(),
                    "original_numerator" = integer(),
                    "original_denominator" = integer(),
                    "reduced_time" = integer(),
                    "original_time" = integer(),
                    "cost_reduction" = double(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())

    names(d) <- c("method",
                  "schema",
                  "trial",
                  "percentage",
                  "reduced_numerator",
                  "reduced_denominator",
                  "original_numerator",
                  "original_denominator",
                  "reduced_time",
                  "original_time",
                  "cost_reduction",
                  "reduced_mutation_score",
                  "original_mutation_score")

    schemas <- select_unique_schemas(data)

    # get list of all schemas
    schemas <- select_unique_schemas(data)

    # for each schema
    for(sc in schemas[[1]]) {

        # 100% operator data for schema --- same as entire data set for schema
        original_data <- select_individual_schema_data(data, sc)

        a <- random_sampling(original_data)
        d <- rbind(d, a)

        b <- across_operators(original_data)
        d <- rbind(d, b)

    }
    return(d)
}

#' FUNCTION: random_sampling
#'
#' This is the function that will perform random sampling. There is a function
#' called analyse_random_sampling which utilizes this function to perform
#' random sampling in parallel.
#'
#' @export

random_sampling <- function(data) {
    # initiallize empty data frame
    d <- data.frame("method" = character(),
                    "schema" = character(),
                    "trial" = integer(),
                    "percentage" = integer(),
                    "reduced_numerator" = integer(),
                    "reduced_denominator" = integer(),
                    "original_numerator" = integer(),
                    "original_denominator" = integer(),
                    "reduced_time" = integer(),
                    "original_time" = integer(),
                    "cost_reduction" = double(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())

    names(d) <- c("method",
                  "schema",
                  "trial",
                  "percentage",
                  "reduced_numerator",
                  "reduced_denominator",
                  "original_numerator",
                  "original_denominator",
                  "reduced_time",
                  "original_time",
                  "cost_reduction",
                  "reduced_mutation_score",
                  "original_mutation_score")

        # create a sequence of all possible percentage values for k% selection
        s <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

            # for each k%
            for(i in s) {
                # run 30 trials
                for(j in 1:30) {
                    # reduction technique utilised
                    method <- "random_sampling"
                    # create a reduced data set
                    reduced_data <- select_k_percent(data, i)
                    # get the schema of observation
                    schema <- select_unique_schemas(data)
                    # trial number for given k%
                    trial <- j
                    # the k% we are currently observing
                    percentage <- (i * 100)
                    # number of killed mutants (numerator)
                    reduced_numerator <- dplyr::filter(reduced_data, killed == "true") %>% dplyr::count()
                    # number of killed and alive mutants (denominator)
                    reduced_denominator <- (reduced_numerator + dplyr::filter(reduced_data, killed == "false") %>% dplyr::count())
                    # number of killed mutants (numerator)
                    original_numerator <- dplyr::filter(data, killed == "true") %>% dplyr::count()
                    # number of killed and alive mutants (denominator)
                    original_denominator <- (original_numerator + dplyr::filter(data, killed == "false") %>% dplyr::count())
                    # total time (in ms) for running reduced set of mutants
                    reduced_time <- analyse_total_time(reduced_data)
                    # total time (in ms) for running original set of mutants (100 percent)
                    original_time <- analyse_total_time(data)
                    # creation cost reduction (%)
                    cost_reduction <- analyse_reduction(original_time, reduced_time)
                    # mutation score of reduced set for this observation
                    reduced_mutation_score <- analyse_mutation_score(reduced_data)
                    # original mutation score for full set of mutants
                    original_mutation_score <- analyse_mutation_score(data)

                    # add everything to vector 'a' to be passed to data frame 'd'
                    a <- data.frame(method,
                                    schema,
                                    trial,
                                    percentage,
                                    reduced_numerator,
                                    reduced_denominator,
                                    original_numerator,
                                    original_denominator,
                                    reduced_time,
                                    original_time,
                                    cost_reduction,
                                    reduced_mutation_score,
                                    original_mutation_score)
                    names(a) <- names(d)

                    # append observation (row) to end of data frame 'd'
                    d <- rbind(d, a)
                }
            }
    return(d)
}

#' FUNCTION: across_operators
#'
#' This function will be used to generate a sequence of values between 0.01 and 1.00
#' that will be used to test the mutation score of each fraction of reduced set of mutants
#' on a per-operator basis. Additionally, we will use this data to calculate mutation scores
#' and costs across all operators for each percentage.
#'
#' @export

across_operators <- function(data) {

    # initiallize empty data frame
    d <- data.frame("method" = character(),
                    "schema" = character(),
                    "trial" = integer(),
                    "percentage" = integer(),
                    "reduced_numerator" = integer(),
                    "reduced_denominator" = integer(),
                    "original_numerator" = integer(),
                    "original_denominator" = integer(),
                    "reduced_time" = integer(),
                    "original_time" = integer(),
                    "cost_reduction" = double(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())

    names(d) <- c("method",
                  "schema",
                  "trial",
                  "percentage",
                  "reduced_numerator",
                  "reduced_denominator",
                  "original_numerator",
                  "original_denominator",
                  "reduced_time",
                  "original_time",
                  "cost_reduction",
                  "reduced_mutation_score",
                  "original_mutation_score")

    # create a sequence of all possible percentage values for k% selection
    s <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

        # for each percentage
        for(i in s) {
            # for 30 trials
            for(j in 1:30) {
                reduced_operator_data <- select_percentage_across_operators(data, i)
                # reduction technique utilised
                method <- "across_operators"
                # schema
                # schema <- sc
                schema <- select_unique_schemas(data)
                # trial number for given k%
                trial <- j
                # the k% we are currently observing
                percentage <- (i * 100)
                # number of killed mutants (numerator)
                reduced_numerator <- dplyr::filter(reduced_operator_data, killed == "true") %>% dplyr::count()
                # number of killed and alive mutants (denominator)
                reduced_denominator <- (reduced_numerator + dplyr::filter(reduced_operator_data, killed == "false") %>% dplyr::count())
                # number of killed mutants (numerator)
                original_numerator <- dplyr::filter(data, killed == "true") %>% dplyr::count()
                # number of killed and alive mutants (denominator)
                original_denominator <- (original_numerator + dplyr::filter(data, killed == "false") %>% dplyr::count())
                # cost of reduced set of mutants
                reduced_time <- analyse_total_time(reduced_operator_data)
                # cost of original set of mutants
                original_time <- analyse_total_time(data)
                # creation cost reduction (%)
                cost_reduction <- analyse_reduction(original_time, reduced_time)
                # calculate the reduced  mutation score for the given operator data
                reduced_mutation_score <- analyse_mutation_score(reduced_operator_data)
                # calculate the original mutation score for the given operator data
                original_mutation_score <- analyse_mutation_score(data)

                a <- data.frame(method,
                                schema,
                                trial,
                                percentage,
                                reduced_numerator,
                                reduced_denominator,
                                original_numerator,
                                original_denominator,
                                reduced_time,
                                original_time,
                                cost_reduction,
                                reduced_mutation_score,
                                original_mutation_score)
                names(a) <- names(d)

                # append observation (row) to end of data frame 'd'
                d <- rbind(d, a)
                }
            }
    return(d)
}

#' FUNCTION: analyse_calculations
#'
#' This function for each percent will be used to show the effectiveness
#' and efficiency of each approach.
#'
#' @export

analyse_calculations <- function(data) {
    d <- data.frame("method" = character(),
                    "schema" = character(),
                    "percentage" = integer(),
                    "avg_reduced_mutation_score"= double(),
                    "correlation" = double(),
                    "avg_cost_reduction" = double(),
                    "mean_absolute_error" = double(),
                    "root_mean_squared_error" = double())

    names(d) <- c("method",
                  "schema",
                  "percentage",
                  "avg_reduced_mutation_score",
                  "correlation",
                  "avg_cost_reduction",
                  "mean_absolute_error",
                  "root_mean_squared_error")

    schemas <- select_unique_schemas(data)
    per <- select_unique_percentages(data) %>% dplyr::filter(percentage < 100)
    m <- select_unique_methods(data)

        for(n in m[[1]]) {
            for(s in schemas[[1]]) {
                for(p in per[[1]]) {

               method <- n
               schema <- s
               percentage <- p
               subset_data <- dplyr::filter(data, schema == s, percentage == p)
               corr_subset_data <- dplyr::filter(data, percentage == p)

               avg_reduced_mutation_score <- mean(subset_data$reduced_mutation_score)
               correlation <- calculate_mutation_score_correlation(corr_subset_data)
               avg_cost_reduction <- mean(subset_data$cost_reduction)
               mean_absolute_error <- Metrics::mae(subset_data$reduced_mutation_score, subset_data$original_mutation_score)
               root_mean_squared_error <- Metrics::rmse(subset_data$reduced_mutation_score, subset_data$original_mutation_score)

               # add everything to vector 'a' to be passed to data frame 'd'
               a <- data.frame(method,
                               schema,
                               percentage,
                               avg_reduced_mutation_score,
                               correlation[1],
                               avg_cost_reduction,
                               mean_absolute_error,
                               root_mean_squared_error)

               names(a) <- names(d)
               # append observation (row) to end of data frame 'd'
               d <- rbind(d, a)
           }
        }
    }
    return(d)
}

#' FUNCTION: calculate_mutation_score_correlation
#'
#' This function will calculate the correlation between percentages and the original
#' mutation score for that percent. This is only possible if we look at all schemas at the
#' same time for a given percent. Otherwise, the standard deviation is zero and correlation
#' is not able to be calculated
#'
#' @export

calculate_mutation_score_correlation <- function(data) {
  model <- cor.test(data$original_mutation_score, data$reduced_mutation_score, method = "kendall", use = "pairwise")
  tidy_model <- model %>% broom::tidy()
  return(tidy_model)
}

#' FUNCTION: analyse_total_time
#'
#' This function will be used to calculate the total time (in ms) of performing
#' mutation analysis with the given set of mutants.
#'
#' @export

analyse_total_time <- function(data) {
    total_time <- sum(data$time)

    return(total_time)
}

#' FUNCTION: analyse_mutation_score
#'
#' This function will be used to calculate mutation scores. It will calculate
#' mutation scores for both the entire set of mutants and the selected
#' set of mutants. Then we will use this to compare the effectiveness of
#' each selective approach.
#'
#' @export

analyse_mutation_score <- function(data) {
    k <- dplyr::filter(data, killed == "true") %>%
        dplyr::count()
    a <- dplyr::filter(data, killed == "false") %>%
        dplyr::count()
    s <- ((k / (k + a)))
    return(s)
}

#' FUNCTION: analyse_reduction
#'
#' This function calculates the percentage in reduction of creation cost
#' between the original and reduces sets. As input this function takes the
# cost to create all mutants (original_cost) and the cost to create each
# reduced set (reduced_cost).
#'
#' @export

analyse_reduction <- function(o, r) {
    p <- (o - r) / o
    return(p)
}
