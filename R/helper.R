#' FUNCTION: helper_bitflip_keep
#'
#' This is a helper function for ANALYZE_INCREMENTAL
#' Currently, this function negates boolean values i.e., TRUE -> FALSE, FALSE -> TRUE. Position is
#' used to idicate the current position to bitflip and partition_size is the number of subsequent
#' positions to also flip --- this could be useful if we wanted to try different sizes to reduce time
#' of HC by increasing step size. **We could add another parameter 'group_by' so that instead of just
#' flipping consecutive values, we could flip based on some group (e.g., operators).
#' @export

helper_bitflip_keep <- function(d, p, partition_size=1) {
  df <- data.frame()
  rows <- nrow(d)

  if ((p + partition_size) > rows) {
    remainder <- (p + partition_size) - rows

    b <- d %>% dplyr::filter(row_number() < remainder)
    a <- d %>% dplyr::filter(row_number() >= remainder, row_number() < p) # don't flip
    m <- d %>% dplyr::filter(row_number() >= p, row_number() <= rows) # flip
    bb <- b %>% dplyr::mutate(keep = !keep) # do the flip
    u <- m %>% dplyr::mutate(keep = !keep) # do the flip
    df <- rbind(bb, a, u)
  } else {
    b <- d %>% dplyr::filter(row_number() < p) # mutants before position
    m <- d %>% dplyr::filter(row_number() >= p, row_number() < (p + partition_size)) # mutants to flip
    r <- d %>% dplyr::filter(row_number() >= (p + partition_size)) # mutants following flip position
    u <- m %>% dplyr::mutate(keep = !keep) # do the flip
    df <- rbind(b, u, r)
  }
  return(df)
}

#' FUNCTION: helper_incremental
#'
#' Helper analyze incremental
#' @export

helper_incremental <- function(d, partition_size=1) {
  dd <- data.frame()

  # initialized to set temp_best_fit to fitness of original data
  current_best_fit <- evaluate_reduction_technique(d, d) %>%
    transform_fitness(0.5, 0.5) %>%
    transform_add_position(0) %>%
    calculate_best_fit() %>%
    collect_schema_data()

  for(j in 1:30) {
    dk <- data.frame() # hold the keep data
    df <- data.frame() # hold the best_fit data

    outside_step <- 1
    g <- d

    print(paste("TRIAL: ", j))
    start_position <- d %>% dplyr::count() %>% select_random_start_position()
    print(paste("START POSITION: ", start_position))

    g <- g %>% helper_bitflip_keep(start_position, partition_size) %>%
      transform_add_position(start_position) %>%
      as.data.frame()

    while (TRUE) {

      fst <- TRUE
      dk <- data.frame() # hold the keep data
      df <- data.frame() # hold the best_fit data

      position <- start_position + partition_size
      print(paste("OUTSIDE STEP: ", outside_step))

      while (TRUE) {

        if (position == start_position && outside_step == 1) {
          break
        }

        k <- g %>% helper_bitflip_keep(position, partition_size) %>%
          transform_add_position(position) %>%
          as.data.frame()
        r <- k %>% collect_keep_data() # only data that was 'kept'
        da <- evaluate_reduction_technique(d, r) %>%
          transform_fitness(0.5, 0.5) %>%
          transform_add_position(position) %>%
          as.data.frame()

        if (position == start_position && fst != TRUE) {
          break
        } else if ((position + partition_size) > nrow(g)) {
          position <- (position + partition_size) - nrow(d)
        } else {
          position <- position + partition_size
        }

        fst <- FALSE
        dk <- rbind(dk, k) # keep data
        df <- rbind(df, da) # best_fit data
      }

      temp_best_fit <- current_best_fit %>% as.data.frame()
      b <- df %>% calculate_best_fit() %>% collect_best_fit_data()
      current_best_fit <- b[!duplicated(b$schema), ] # if ties, only keep one per schema
      g <- collect_best_keep_data(current_best_fit, dk)
      outside_step <- outside_step + 1

      # we stop if it is equal because then we are no longer climbing, we have plateaued
      if ((current_best_fit$best_fit <= temp_best_fit$best_fit)) {
        a <- temp_best_fit %>% transform_add_start_position(start_position) %>% transform_add_trial(j)
        dd <- rbind(dd, a)
        break
      }
    }
  }
  # return(k) # the final reduced keep data telling you which mutants to keep and ignore
  return(dd) # just the actual best_fit values and their respective step for each schema
}

#' FUNCTION: helper_incremental_across_schemas
#'
#' Helper analyze incremental across all schemas to provide a "more actionable" and generalized
#' approach to follow.
#' @export

helper_incremental_across_schemas <- function(d, s, corr_threshold, cost_threshold) {
  dbk <- data.frame()
  dbc <- data.frame()
  bk <- data.frame()

  # 30 trials to account for random start position
  for (j in 1:30) {
    best_correlation_vector <- vector()
    f <- select_random_percent()
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print(paste("trial: ", j))
    print(paste("random fractional start position: ", f))
    print(paste("fractional step size: ", s))
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    # add random start position and per-schema mutant count
    dt <- d %>% transform_add_start_and_step(s, f) %>% transform_mutant_count() %>% as.data.frame()
    g <- dt # initialize g to original set of mutants
    previous_best_corr <- 0
    neighborhood_size <- 0
    outside_step <- 1

    while (TRUE) {
      print(paste("+++++++++++++++++++++++ NEIGHBORHOOD GENERATION: ", outside_step, " +++++++++++++++++++++++"))
      if (outside_step > 1) {
        g <- dt # initialize g to original set of mutants
        g$keep <- bk$keep # use previously "flipped" keep values as current keep column
      }
      stp <- 1 # current step
      neighborhood_keep_data <- data.frame()
      neighborhood_corr_data <- data.frame()
      frst <- TRUE
      wrap <- FALSE

      while (TRUE) {
        k <- g %>% helper_flip() %>% transform_add_step(stp)
        neighborhood_keep_data <- rbind(neighborhood_keep_data, k)
        r <- k %>% collect_keep_data()
        # compare the effectiveness of the reduced data (r) to the original data (d) across schemas
        current_corr <- d %>% evaluate_reduction_technique_across(r, stp) %>% dplyr::ungroup() %>% transform_add_correlation()
        neighborhood_corr_data <- rbind(neighborhood_corr_data, current_corr)

        # only use first position for comparing
        k %>% dplyr::glimpse()
        current_corr %>% dplyr::glimpse()
        if ((g$position[[1]] + g$step_size[[1]]) > g$mutant_count[[1]]) {
          p <- (g$position + g$step_size) - g$mutant_count
          wrap <- TRUE
        } else {
          p <- g$position + g$step_size
        }
        g <- g %>% transform_update_position(p)
        if (g$position[[1]] == g$start_position[[1]] && frst == FALSE ||
            g$position[[1]] >= g$start_position[[1]] && wrap == TRUE) {
          break
        }
        frst <- FALSE
        stp <- stp + 1
      }
      neighborhood_size <- g %>% calculate_neighborhood_size() # get max step
      print(paste("neighborhood size: ", neighborhood_size))

      # select a step that hasn't already been chosen as the best position
      b <- neighborhood_corr_data %>% dplyr::filter(!(step %in% best_correlation_vector)) %>%
        transform_highest_correlation() %>% collect_highest_correlation_data()
      current_best_corr <- b$highest_correlation[1] # get the highest correlation value
      current_cost_reduction <- sum(b$original_time) - sum(b$reduced_time)
      current_cost_reduction_percent <- current_cost_reduction / sum(b$original_time)
      print(paste("current best correlation: ", current_best_corr))
      print(paste("current cost reduction (ms): ", current_cost_reduction))
      print(paste("current cost reduction (%): ", current_cost_reduction_percent))
      highest_correlation_data <- b[!duplicated(b$schema), ] # if ties, only keep one per schema
      previous_bk <- bk %>% transform_add_trial(j)
      bk <- neighborhood_keep_data %>% collect_best_step_data(highest_correlation_data)
      best_correlation_vector <- append(best_correlation_vector, bk$step[1])
      print("chosen best positions: ")
      print(best_correlation_vector)
      if ((current_best_corr < (previous_best_corr - corr_threshold) &&
          current_cost_reduction_percent < (previous_cost_reduction_percent + cost_threshold)) ||
          outside_step >= neighborhood_size) {
        break
      }
      outside_step <- outside_step + 1
      previous_best_corr <- current_best_corr
      previous_cost_reduction_percent <- current_cost_reduction_percent
    }
    dbk <- rbind(dbk, previous_bk)
    # dbc <- rbind(dbc, previous_best_corr) # collected correlation data
  }
  return(dbk)
}

#' FUNCTION: helper_flip
#'
#' This function is responsible for doing the first flip. This includes choosing the random start
#' positions and ignoring the respective mutants.
#' @export

helper_flip <- function(d) {
  ds <- split(d, d$schema)
  dt <- ds %>% parallel::mclapply(helper_bitflip_keep_across) %>%
    lapply(as.data.frame) %>% dplyr::bind_rows()
  return(dt)
}

#' FUNCTION: helper_bitflip_keep_across
#'
#' Currently, this function negates boolean values i.e., TRUE -> FALSE, FALSE -> TRUE. Position is
#' used to idicate the current position to bitflip and step_size is the number of subsequent of mutants.
#' **We could add another parameter 'group_by' so that instead of just
#' flipping consecutive values, we could flip based on some group.
#' @export

helper_bitflip_keep_across <- function(d) {
  df <- data.frame()
  rows <- d %>% nrow() %>% as.numeric()
  p <- d %>% select_current_position()
  s <- d %>% select_current_step_size()

  if ((p + s - 1) > rows) {
    rem <- (p + s - 1) - rows
    b <- d[1:rem, ]
    a <- d[(rem + 1):(p - 1), ]
    m <- d[p:rows, ]
    bb <- b %>% dplyr::mutate(keep = FALSE) # do the flip
    u <- m %>% dplyr::mutate(keep = FALSE) # do the flip
    df <- rbind(bb, a, u)
  } else if ((p + s - 1) == rows) {
    b <- d[1:(p - 1), ]
    m <- d[p:rows, ]
    u <- m %>% dplyr::mutate(keep = FALSE) # do the flip
    df <- rbind(b, u)
  } else {
    if (p == 1) {
      m <- d[p:(p + s - 1), ]
      r <- d[(p + s):rows, ]
      u <- m %>% dplyr::mutate(keep = FALSE) # do the flip
      df <- rbind(u, r)
    } else {
      b <- d[1:(p - 1), ]
      m <- d[p:(p + s - 1), ]
      r <- d[(p + s):rows, ]
      u <- m %>% dplyr::mutate(keep = FALSE) # do the flip
      df <- rbind(b, u, r)
    }
  }
  return(df)
}

#' FUNCTION: helper_apply_operator_model
#'
#' @export

helper_apply_operator_model <- function(d, model) {
  ds <- split(d, d$operator)
  dt <- ds %>% parallel::mclapply(helper_select_operator_data, model=model) %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  return(dt)
}

#' FUNCTION: helper_select_operator_data
#'
#' This function is responsible for selecting the appropriate amount of mutants for an operator
#' according to the generated model.
#' @export

helper_select_operator_data <- function(d, model) {
  current_operator <- d$operator[1]
  model_rec <- model %>% dplyr::filter(operator == current_operator)
  dt <- d %>% dplyr::sample_frac(model_rec$average_percent_kept)
  return(dt)
}
