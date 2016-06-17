#' FUNCTION: cumulative_sum_time
#'
#' This function calculates the sum of times for each group---in this case, each mutant of
#' the same type, of the same operator, in the same schema. This will allow us to analyse
#' the overall cost per mutant type.
#' @export

cumulative_sum_time <- function(data) {
    p <- dplyr::mutate(data, cs = cumsum(time))
    return(p)
}

#' FUNCTION: sum_all_times
#'
#' This function is useful to see the overall cost (execution time) of an entire
#' mutant set.
#' @export

sum_all_times <- function(data) {
    p <- sum(data$time)
    return(p)
}


