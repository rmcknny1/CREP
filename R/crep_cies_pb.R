#' @title crep_cies_pb
#'
#' @description Calculates the N-corrected Cox index effect size when given the
#' probability of a given occurrence in two groups. The first two arguments
#' are the probability in the intervention group and control group,
#' respectively. Both should be decimals with a value < 1.00.
#'
#' The third argument (optional) is the total N for the dataset. If this
#' argument is supplied, the correction for sample size in CIES (omega) will
#' be applied. If it is not supplied, omega will not be applied. Omega is
#' missing from the WWC handbook, but discussed in their online training
#' for baseline equivalence.
#'
#' @param pi The probability of a given result in the intervention group.
#' @param pc The probability of a given result in the control group.
#' @param N The total sample size.
#'
#' @return
#' Uncorr. CIES: the CIES uncorrected for sample size
#' Corr. CIES: The CIES corrected for sample size
#'
#'
#' @export crep_cies_pb
#' @examples
#' crep_cies_pb(0.5, 0.7)
#' crep_cies_pb(0.6, 0.4, N = 10)


crep_cies_pb <- function(pi, pc, N = 0){
  or <- (pi/(1-pi)) / (pc/(1-pc))
  ces <- data.frame(log(or)/1.65, "na")

  if(N != 0){
    omega <- 1 - (3 / (4 * N - 9 ))
    ces[2] <- omega * ces[1]
  }
  names(ces) <- c("Uncorr. CIES", "Corr. CIES")
  return(ces)
}
