#' FUNCTION: helper_bitflip_keep
#'
#' This is a helper function for ANALYZE_INCREMENTAL
#' Currently, this function negates boolean values i.e., TRUE -> FALSE, FALSE -> TRUE. Position is
#' used to idicate the current position to bitflip and partition_size is the number of subsequent
#' positions to also flip --- this could be useful if we wanted to try different sizes to reduce time
#' of HC by increasing step size. **We could add another parameter 'group_by' so that instead of just
#' flipping consecutive values, we could flip based on some group (e.g., operators).
#' @export

helper_bitflip_keep <- function(d, position, partition_size=1) {
  d <- d %>% collect_schema_data()
  df <- data.frame()

  if (position == partition_size) {
    m <- d %>% dplyr::filter(row_number() <= position)
    r <- d %>% dplyr::filter(row_number() > position)
    u <- m %>% dplyr::mutate(keep = !keep)
    df <- rbind(u, r)

  } else {
    b <- d %>% dplyr::filter(row_number() <= (position - partition_size))
    m <- d %>% dplyr::filter(row_number() > (position - partition_size), row_number() <= position)
    r <- d %>% dplyr::filter(row_number() > position)
    u <- m %>% dplyr::mutate(keep = !keep)
    df <- rbind(b, u, r)
  }
  return(df)
}

#' FUNCTION: helper_gather_keep_data
#'
#' Gather data only the keep data cooresponding the best fit step for a given schema.
#' @export

helper_gather_keep_data <- function(b, k) {
  df <- data.frame()
  i <- 1
  while (i <= nrow(b)) {
    dt <- b %>% dplyr::ungroup() %>% dplyr::filter(row_number() == i) %>% collect_schema_step_data(k)
    df <- rbind(df, dt)
    i <- i + 1
  }
  return(df)
}
