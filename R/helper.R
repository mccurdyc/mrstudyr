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

helper_incremental_across_schemas <- function(d, s) {
  # o <- evaluate_reduction_technique(d, d) # evaluate original data compared to itself; mainly to get mutation scores
  k <- helper_first_flip(d, s) # include ignore values for next flip
  f <- k %>% collect_keep_data() %>% collect_schema_data()
  e <- evaluate_reduction_technique(d, f)
  first_step_corr <- calculate_correlation(e)
  return()
}

#' FUNCTION: helper_first_flip
#'
#' This function is responsible for doing the first flip. This includes choosing the random start
#' positions and ignoring the respective mutants.
#' @export

helper_first_flip <- function(d, s) {
  f <- select_random_percent()
  d <- d %>% do(dplyr::mutate(., position = select_start_position(., f))) %>%
             do(dplyr::mutate(., step_size = select_step_size(., s))) %>%
             dplyr::ungroup() # has to be separate from first mutate; causes errors otherwise
  ds <- split(d, d$schema)
  # g_split_flipped <- g_split[[3]] %>% helper_bitflip_keep_across() # debugging for a single schema
  dt <- ds %>% parallel::mclapply(helper_bitflip_keep_across) %>%
               lapply(as.data.frame) %>%
               dplyr::bind_rows()
  return(dt)
}

#' FUNCTION: helper_bitflip_keep_across
#'
#' This is a helper function for ANALYZE_INCREMENTAL
#' Currently, this function negates boolean values i.e., TRUE -> FALSE, FALSE -> TRUE. Position is
#' used to idicate the current position to bitflip and partition_size is the number of subsequent
#' positions to also flip --- this could be useful if we wanted to try different sizes to reduce time
#' of HC by increasing step size. **We could add another parameter 'group_by' so that instead of just
#' flipping consecutive values, we could flip based on some group (e.g., operators).
#' @export

helper_bitflip_keep_across <- function(d) {
  df <- data.frame()
  rows <- d %>% nrow() %>% as.numeric()
  print(paste("current rows: ", rows))
  p <- d %>% select_current_position() %>% as.numeric()
  print(paste("current position: ", p))
  s <- d %>% select_current_step_size() %>% as.numeric()
  print(paste("current step size: ", s))

  if ((p + s - 1) > rows) {
    rem <- (p + s - 1) - rows
      b <- d[1:rem, ]
      a <- d[(rem + 1):(p - 1), ]
      m <- d[p:rows, ]
      bb <- b %>% dplyr::mutate(keep = !keep) # do the flip
      u <- m %>% dplyr::mutate(keep = !keep) # do the flip
      print("B")
      b %>% dplyr::glimpse()
      print("A")
      a %>% dplyr::glimpse()
      print("M")
      m %>% dplyr::glimpse()
      print("BB")
      bb %>% dplyr::glimpse()
      print("U")
      u %>% dplyr::glimpse()
      df <- rbind(bb, a, u)
  } else if ((p + s - 1) == rows) {
    b <- d[1:(p - 1), ]
    m <- d[p:rows, ]
    u <- m %>% dplyr::mutate(keep = !keep) # do the flip
    print("B")
    b %>% dplyr::glimpse()
    print("U")
    u %>% dplyr::glimpse()
    df <- rbind(b, u)
  } else {
    if (p == 1) {
      m <- d[p:(p + s - 1), ]
      r <- d[(p + s):rows, ]
      u <- m %>% dplyr::mutate(keep = !keep) # do the flip
      print("M")
      m %>% dplyr::glimpse()
      print("U")
      u %>% dplyr::glimpse()
      print("R")
      r %>% dplyr::glimpse()
      df <- rbind(u, r)
    } else {
      b <- d[1:(p - 1), ]
      m <- d[p:(p + s - 1), ]
      r <- d[(p + s):rows, ]
      u <- m %>% dplyr::mutate(keep = !keep) # do the flip
      print("B")
      b %>% dplyr::glimpse()
      print("M")
      m %>% dplyr::glimpse()
      print("U")
      u %>% dplyr::glimpse()
      print("R")
      r %>% dplyr::glimpse()
      df <- rbind(b, u, r)
    }
  }
  df %>% dplyr::glimpse()
  return(df)
}
