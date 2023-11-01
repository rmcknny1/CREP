#' @title crep_ii
#'
#' @description Calculates the percentile rank and improvement index
#' according to WWC methods. The argument is g or a g-equivalent effect
#' size such as the CIES. Other effect sizes may be used if translated
#' to g first.
#'
#' @usage crep_ii(es)
#'
#' @param es Effect size (g, d, CIES, or equivalent)
#'
#' @export crep_ii
#'
#' @return percentile rank, improvement index
#'
#' @examples
#' CREP::crep_ii(0.25)

crep_ii <- function(es){
  pr <- c()
  pr[1] <- round(pnorm(es)*100)
  pr[2] <- pr[1] - 50
  names(pr) <- c("Per. Rank", "Imp. Index")
  return(pr)
}
