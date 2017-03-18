#' FUNCTION: effectsize_interpret
#'
#' Interpret the Vargha-Delaney Effect Size using the approach proposed by Chris Wright.
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_interpret <- function(a) {
  size <- "none"
  if (a < 0.44 || a > 0.56) {
    size <- "small"
  }
  if (a < 0.36 || a > 0.64) {
    size <- "medium"
  }
  if (a < 0.29 || a > 0.71) {
    size <- "large"
  }
  return(size)
}

#' FUNCTION: effectsize_calculate_accurate
#'
#' Calculate the actual Vargha-Delaney A value using the more accurate approach.
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_calculate_accurate <- function(r1, m, n) {
  a <- (2*r1 - m*(m+1)) / (2*n*m)
  return(a)
}

#' FUNCTION: effectsize_calculate_standard
#'
#' Calculate the actual Vargha-Delaney A value using the standard approach.
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_calculate_standard <- function(r1, m, n) {
  a <- (r1/m - (m+1)/2)/n # formula (14) in Vargha and Delaney, 2000
  return(a)
}

#' FUNCTION: effectsize_default
#'
#' Default way to calculate the Vargha-Delaney Effect Size using the approach closest to published SBSE papers.
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_default <- function(d, f, ac) {

  # define the treatment and control according to the effsize package
  treatment <- d
  control <- f

  # define all of the value of variables in the effect size calculation
  m <- length(treatment)
  n <- length(control)
  r <- rank(c(treatment,control))
  r1 <- sum(r[seq_len(m)])

  # Compute the Vargha-Delaney effect size measure with the standard equation
  a <- ac(r1, m, n)

  # interpret the effect size and give it a human-readable meaning
  size <- effectsize_interpret(a)

  return(list(value = a,
              size = size,
              rank.sum = r1))
}

#' FUNCTION: effectsize_standard
#'
#' Calculate the Vargha-Delaney Effect Size using the approach closest to published SBSE papers.
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_standard <- function(d, f) {
  return(effectsize_default(d, f, effectsize_calculate_standard))
}

#' FUNCTION: effectsize_accurate
#'
#' Calculate the Vargha-Delaney Effect Size using the approach that is claimed to avoid accuracy errors
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_accurate <- function(d,f) {
  return(effectsize_default(d, f, effectsize_calculate_accurate))
}

#' FUNCTION: effectsize_wright
#'
#' Calculate the Vargha-Delaney Effect Size using the approach proposed by Chris Wright.
#' Author: @gkapfham https://github.com/gkapfham/virtualmutationanalysis
#' @export

effectsize_wright <- function(sample1, sample2) {
  # combine the data samples
  all <- c(sample1, sample2)

  # compute the ranks for the values and then sum them
  ranks <- rank(all, ties.method="average")
  rank.sum <- sum(ranks[1:length(sample1)])

  # compute the values of m and n (the same in our experimental work)
  m <- length(sample1)
  n <- length(sample2)

  # compute the Vargha-Delaney a effect size
  a <- ((rank.sum / m - (m + 1.0) / 2.0) / n)

  # interpret the effect size and give it a human-readable meaning
  size <- effectsize_interpret(a)

  return (list(value = a,
               size = size,
               rank.sum = rank.sum))
}
