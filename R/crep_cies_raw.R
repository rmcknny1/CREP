#' @title crep_cies_raw
#'
#' @description Calculates the N-corrected Cox index effect size when
#' given two variables. The first two arguments should be dichotomous
#' data. The third argument should be "wide" or "long", and specifies
#' the form of the data. If each column is data for a separate group,
#' the data is wide. If one column is data and the other is group
#' membership, the data is long. For long data, the group variable is
#' expected to come first. If no type is specified, wide is the default.
#'
#' @param group1 Data for the first group if `type = "wide`. A variable
#' indicating group membership if `type = "long"`.
#' @param group2 Data for the first group if `type = "wide"`. Outcome data
#' if `type = "long"`.
#' @param type Can be `wide` or `long`.
#'
#' @return cies
#' @export crep_cies_raw
#'
#' @examples
#' df <- data.frame(treatment = c(0, 0, 0, 0, 1, 1, 1, 1),
#'                  control = c(1, 0, 1, 1, 0, 1, 1, 0))
#'
#' crep_cies_raw(df$treatment, df$control)
#' colnames(df) <- c("group", "data")
#' crep_cies_raw(df$group, df$data, type = "long")


crep_cies_raw <- function(group1, group2, type = "wide"){
  if(length(table(group1)) != 2){
    stop("First variable must have exactly two categories")}
  if(length(table(group2)) != 2){
    stop("Second variable must have exactly two categories")}
  v1 <- group1
  v2 <- group2
  if(type == "long"){
    dfr <- data.frame(group1, group2)
    tbl <- table(group1)
    v1 <- dplyr::filter(dfr, group1 == 0)$group2
    v2 <- dplyr::filter(dfr, group1 == 1)$group2
    if(length(table(v1)) != 2){stop(
      "First variable must have exactly two categories")}
    if(length(table(v2)) != 2){stop(
      "Second variable must have exactly two categories")}
  }
  t1 <- table(v1)
  t2 <- table(v2)
  prob_t <- t1[2] / length(v1)
  prob_c <- t2[2] / length(v2)
  or <- (prob_t/(1-prob_t)) / (prob_c/(1-prob_c))
  ces <- log(or)/1.65
  n <- length(v1) + length(v2)
  # if(is.infinite(ces)){
  #   if(cor(v1, v2) == 1){
  #     return("Inf")
  #   }
  #   return(0)
  # }
  omega <- 1 - (3 / (4 * n - 9 ))
  ces[2] <- omega * ces
  names(ces) <- c("Uncorr. CIES", "Corr. CIES")
  return(ces)
}


