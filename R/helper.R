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
