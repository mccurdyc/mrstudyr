#' FUNCTION: analyse_across_operators
#'
#' This function will be used to generate a sequence of values between 0.01 and 1.00
#' that will be used to test the mutation score of each fraction of reduced set of mutants
#' on a per-operator basis. Additionally, we will use this data to calculate mutation scores
#' and costs across all operators for each percentage.
#'
#' @export

analyse_across_operators <- function(data) {
    # create a sequence of all possible percentage values for k% selection
    s <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    schemas <- select_unique_schemas(data)

    # initiallize empty data frame
    d <- data.frame("schema" = character(),
                    "trial" = integer(),
                    "percentage" = integer(),
                    "reduced_numerator" = integer(),
                    "reduced_denominator" = integer(),
                    "original_numerator" = integer(),
                    "original_denominator" = integer(),
                    "reduced_time" = integer(),
                    "original_time" = integer(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())
    names(d) <- c("schema",
                  "trial",
                  "percentage",
                  "reduced_numerator",
                  "reduced_denominator",
                  "original_numerator",
                  "original_denominator",
                  "reduced_time",
                  "original_time",
                  "reduced_mutation_score",
                  "original_mutation_score")
    # for each schema
    for(sc in schemas[[1]]) {
        # 100% operator data for schema --- same as entire data set for schema
        original_data <- select_individual_schema_data(data, sc)
        # for each percentage
        for(i in s) {
            # for 30 trials
            for(j in 1:30) {
                reduced_operator_data <- select_percentage_across_operators(data, sc, i)

                # schema
                schema <- sc
                # trial number for given k%
                trial <- j
                # the k% we are currently observing
                percentage <- (i * 100)
                # number of killed mutants (numerator)
                reduced_numerator <- dplyr::filter(reduced_operator_data, killed == "true") %>% dplyr::count()
                # number of killed and alive mutants (denominator)
                reduced_denominator <- (reduced_numerator + dplyr::filter(reduced_operator_data, killed == "false") %>% dplyr::count())
                # number of killed mutants (numerator)
                original_numerator <- dplyr::filter(original_data, killed == "true") %>% dplyr::count()
                # number of killed and alive mutants (denominator)
                original_denominator <- (original_numerator + dplyr::filter(original_data, killed == "false") %>% dplyr::count())
                # cost of reduced set of mutants
                reduced_time <- analyse_total_time(reduced_operator_data)
                # cost of original set of mutants
                original_time <- analyse_total_time(original_data)
                # calculate the reduced  mutation score for the given operator data
                reduced_mutation_score <- analyse_mutation_score(reduced_operator_data)
                # calculate the original mutation score for the given operator data
                original_mutation_score <- analyse_mutation_score(original_data)

                a <- data.frame(schema,
                                trial,
                                percentage,
                                reduced_numerator,
                                reduced_denominator,
                                original_numerator,
                                original_denominator,
                                reduced_time,
                                original_time,
                                reduced_mutation_score,
                                original_mutation_score)
                names(a) <- names(d)

                # append observation (row) to end of data frame 'd'
                d <- rbind(d, a)
                }
            }
        }
    return(d)
}

#' FUNCTION: analyse_random_sampling
#'
#' This function will be used to run selective mutation per schema and collect
#' data in a single data frame
#'
#' @export

analyse_random_sampling <- function(data, f = "fast") {
    # initiallize empty data frame
    d <- data.frame("schema" = character(),
                    "trial" = integer(),
                    "percentage" = integer(),
                    "reduced_numerator" = integer(),
                    "reduced_denominator" = integer(),
                    "original_numerator" = integer(),
                    "original_denominator" = integer(),
                    "reduced_time" = integer(),
                    "original_time" = integer(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())
    names(d) <- c("schema",
                  "trial",
                  "percentage",
                  "reduced_numerator",
                  "reduced_denominator",
                  "original_numerator",
                  "original_denominator",
                  "reduced_time",
                  "original_time",
                  "reduced_mutation_score",
                  "original_mutation_score")
    # find all unique schemas in data
    schemas <- select_unique_schemas(data)

    if(f != "fast") {
        for(s in schemas[[1]]) {
            # get parse out data per-schema individually
            i <- select_individual_schema_data(data, s)
            # collect data about each individual schema and store in 'a' to be bound to 'd' containing all schema data
            a <- analyse_select_mutation_score(i)

            names(a) <- c("schema",
                          "trial",
                          "percentage",
                          "reduced_numerator",
                          "reduced_denominator",
                          "original_numerator",
                          "original_denominator",
                          "reduced_time",
                          "original_time",
                          "reduced_mutation_score",
                          "original_mutation_score")
            # append per-schema data to end of 'd' which will contain all per-schema data in the end
            d <- rbind(d, a)
        }
    } else {
            # Calculate the number of cores
            no_cores <- detectCores() - 1

            # Initiate cluster
            cl <- makeCluster(no_cores)

            # load packages into cluster
            clusterEvalQ(cl, library(magrittr))

            i <- parLapply(cl, schemas[[1]], data = data, select_individual_schema_data)
            a <- parLapply(cl, i, analyse_select_mutation_score)
            d <- do.call(rbind, a)

            # close the cluster so that resources such as memory are returned to the operating system
            stopCluster(cl)
    }

     return(d)
}

#' FUNCTION: analyse_select_mutation_score
#'
#' This function will be used to generate a sequence of values between 0.01 and 1.00
#' that will be used to test the mutation score of each fraction of reduced set of mutants.
#' It returns a vector which we will calculate the average of for each fraction and then
#' append that avgeraged fraction to the entire mutant set.
#'
#' @export

analyse_select_mutation_score <- function(data) {
    # initiallize empty data frame
    d <- data.frame("schema" = character(),
                    "trial" = integer(),
                    "percentage" = integer(),
                    "reduced_numerator" = integer(),
                    "reduced_denominator" = integer(),
                    "original_numerator" = integer(),
                    "original_denominator" = integer(),
                    "reduced_time" = integer(),
                    "original_time" = integer(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())
    names(d) <- c("schema",
                  "trial",
                  "percentage",
                  "reduced_numerator",
                  "reduced_denominator",
                  "original_numerator",
                  "original_denominator",
                  "reduced_time",
                  "original_time",
                  "reduced_mutation_score",
                  "original_mutation_score")

    # create a sequence of all possible percentage values for k% selection
    # s <- seq(0.01, 1, by = 0.01)
    s <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

    # for each k%
    for(i in s) {
        # run 30 trials
        for(j in 1:30) {
            # create a reduced data set
            reduced_data <- select_k_percent(data, i)
            # get the schema of observation
            schema <- dplyr::distinct(dplyr::select(data, schema))
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
            # mutation score of reduced set for this observation
            reduced_mutation_score <- analyse_mutation_score(reduced_data)
            # original mutation score for full set of mutants
            original_mutation_score <- analyse_mutation_score(data)

            # add everything to vector 'a' to be passed to data frame 'd'
            a <- data.frame(schema,
                            trial,
                            percentage,
                            reduced_numerator,
                            reduced_denominator,
                            original_numerator,
                            original_denominator,
                            reduced_time,
                            original_time,
                            reduced_mutation_score,
                            original_mutation_score)
            names(a) <- c("schema",
                          "trial",
                          "percentage",
                          "reduced_numerator",
                          "reduced_denominator",
                          "original_numerator",
                          "original_denominator",
                          "reduced_time",
                          "original_time",
                          "reduced_mutation_score",
                          "original_mutation_score")
            # append observation (row) to end of data frame 'd'
            d <- rbind(d, a)
        }
    }
        return(d)
}

#' FUNCTION: analyse_percents_error
#'
#' This function will calculate both the mean absolute error
#' and rooth mean squared error for all percentages
#' for the random sampling data. This allows us to plot both against each
#' other in a visualization.
#'
#' @export

analyse_percents_error <- function(data) {
    d <- data.frame("schema" = character(),
                    "percentage" = integer(),
                    "mean_absolute_error" = double(),
                    "root_mean_squared_error" = double(),
                    "reduced_time" = double())
    names(d) <- c("schema",
                  "percentage",
                  "mean_absolute_error",
                  "root_mean_squared_error",
                  "reduced_time")

    per <- select_unique_percentages(data) %>% dplyr::filter(percentage < 100)
    schemas <- select_unique_schemas(data)
    for(s in schemas[[1]]) {
        for(p in per[[1]]) {
            schema <- s
            percentage <- p
            subset_data <- dplyr::filter(data, schema == s, percentage == p)
            mean_absolute_error <- Metrics::mae(subset_data$reduced_mutation_score, subset_data$original_mutation_score)
            root_mean_squared_error <- Metrics::rmse(subset_data$reduced_mutation_score, subset_data$original_mutation_score)
            reduced_time <- mean(subset_data$reduced_time)

            # add everything to vector 'a' to be passed to data frame 'd'
            a <- data.frame(schema,
                            percentage,
                            mean_absolute_error,
                            root_mean_squared_error,
                            reduced_time)
            names(a) <- names(d)
            # append observation (row) to end of data frame 'd'
            d <- rbind(d, a)
        }
    }
    return(d)
}

#' FUNCTION: analyse_correlation
#'
#' This function for each percent will calculate kendall's tau_b correlation between the given percent's
#' mutation score and the mutation score of 100 percent of the mutants for all schemas at that percentage.
#'
#' @export

analyse_correlation <- function(data) {
    d <- data.frame("percentage" = integer(),
                    "correlation" = double())
    names(d) <- c("percentage", "correlation")

    per <- select_unique_percentages(data) %>% dplyr::filter(percentage < 100)
    for(p in per[[1]]) {
       percentage <- p
       subset_data <- dplyr::filter(data, percentage == p)
       correlation <- calculate_mutation_score_correlation(subset_data)

            # add everything to vector 'a' to be passed to data frame 'd'
            a <- data.frame(percentage, correlation[1])
            names(a) <- c("percentage", "correlation")
            # append observation (row) to end of data frame 'd'
            d <- rbind(d, a)
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

#' FUNCTION: analyse_percents_average_reduced_mutation_score
#'
#' This function will calculate the averages of all of the thirty trials and
#' display them in an easy-to-view data frame.
#'
#' @export

analyse_percents_average_reduced_mutation_score <- function(data) {
    # initiallize empty data frame
    d <- data.frame("schema" = character(),
                    "percentage" = integer(),
                    "reduced_mutation_score" = double(),
                    "original_mutation_score" = double())
    names(d) <- c("schema", "percentage", "reduced_mutation_score", "original_mutation_score")
    per <- select_unique_percentages(data)
    ss <- select_unique_schemas(data)
    for(s in ss[[1]]) {
        for(p in per[[1]]) {
            schema <- s
            percentage <- p
            i <- select_individual_schema_data(data, s)
            percent_data <- select_individual_percent_data(i, p)
            reduced_mutation_score <- mean(percent_data$reduced_mutation_score)
            original_mutation_score <- dplyr::distinct(dplyr::select(percent_data, original_mutation_score))

            # add everything to vector 'a' to be passed to data frame 'd'
            a <- data.frame(schema, percentage, reduced_mutation_score, original_mutation_score)
            names(a) <- c("schema", "percentage", "reduced_mutation_score", "original_mutation_score")
            # append observation (row) to end of data frame 'd'
            d <- rbind(d, a)
        }
    }
    return(d)
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
