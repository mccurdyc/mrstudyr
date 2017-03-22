#' FUNCTION: analyze_random_sampling
#'
#' This function will perform random sampling.
#' @export

analyze_random_sampling <- function(d) {
  o <- d %>% collect_schema_data()

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

  df <- data.frame()

  for(i in percentages) {
    print(paste("RANDOM SAMPLING: Currently analyzing x =", (i*100), "percent ..."))
    for(j in 1:30) {
      r <- o %>% select_x_percent(i)
      dt <- evaluate_reduction_technique(o, r) %>% transform_add_percentage_trial((i * 100), j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: analyze_selective_random
#'
#' This function will analyze a select set of operators over percentages
#' @export

analyze_selective_random <- function(d, ops) {
  o <- d %>% collect_schema_operator_data()

  percentages <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  df <- data.frame()

  for(i in percentages) {
    print(paste("SELECTIVE RANDOM: Currently analyzing x =", (i * 100), "percent ..."))
    for(j in 1:30) {
      r <- o %>% select_x_percent_across_operators(i)
      dt <- evaluate_reduction_technique(o, r) %>% transform_add_percentage_trial((i * 100), j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
  }
  return(df)
}

#' FUNCTION: analyze_incremental_across_schemas
#'
#' Analyze how reducing the set incrementally effects the error between MS and MS' considering across
#' schemas
#' @export

analyze_incremental_across_schemas <- function(d, step_size, corr_threshold, cost_threshold) {
  df <- data.frame()
  count <- 1 # only used for printing the current schema
  schemas <- d %>% select_all_schemas()
  for (s in schemas[[1]]) {
    print(paste("current excluded schema ", count, ": ", s))
    ds <- d %>% exclude_schema(s) # exclude a single schema from the entire data set
    excluded_schema_data <- d %>% select_schema_data(s) # select only the data for the excluded schema
    # current best correlation in the hill-climbing algorithm cannot be worse than 5% lower than previous best
    # while cost must be reduced by more than 9% than previous cost reduction
    o <- ds %>% collect_schema_data() %>% transform_keep() # add keep column to dataframe
    o <- o[with(o, order(schema, operator)),] # order rows by schema, then operator so that operator data is consistent across schemas
    dh <- helper_incremental_across_schemas(o, step_size, corr_threshold, cost_threshold) # perform the hill climbing algorithm
    model <- dh %>% generate_operator_model() # based on the hill climbing data, generate a genralized model to apply
    path <- ("large_model.feather")
    feather::write_feather(model, path)
    # apply model 30 times to account for randomness (random percentages per operator)
    for (j in 1:30) {
      # apply the model informing how many mutants to keep per operator from original data set
      dt <- excluded_schema_data %>% apply_operator_model(model) %>% transform_add_trial(j) %>% as.data.frame()
      df <- rbind(df, dt)
    }
    count <- count + 1
  }
    pathtwo <- ("df-large.feather")
    feather::write_feather(df, pathtwo)
  return(df)
}
