#' @title crep_hedges_g
#'
#' @description Calculates Hedges' g from summary statistics.
#'
#' @param m1 The mean of group 1.
#' @param sd1 Standard deviation of group 1.
#' @param n1 N of group 1.
#' @param m2 Mean of group 2.
#' @param sd2 Standard deviation of group 2.
#' @param n2 N of group 2.
#'
#' @return g
#'
#' @export crep_hedges_g
#'
#' @examples
#'
#' CREP::crep_hedges_g(15, 4, 20, 40, 4, 32)
#'
crep_hedges_g <- function(m1, sd1, n1, m2, sd2, n2){

  psd_num <- (n1-1) * (sd1^2) + (n2-1) * (sd2^2)
  psd_den <- n1 + n2 - 2
  psd <- sqrt(psd_num/psd_den)

  # sample size correction
  omega <- 1 - (3 / (4 * (psd_den) - 1))
  g <- round(omega * (m1 - m2)/psd, 2)
  names(g) <- "g"
  return(g)
}
