#' @title crep_desc
#'
#' @description Calculates descriptives on all variables in the
#' supplied dataframe.
#'
#' @usage
#' crep_desc(df)
#'
#' @param df A dataframe to run descriptives on.
#'
#' @details
#' Variables as separated as categorical or numeric based on their datatype.
#' If a numeric variable represents nominal categories, it should be encoded
#' as a factor with `as.factor()` before using `crep_desc()`.
#'
#' For each categorical variable, the number of levels and number of NAs is
#' reported. For each numeric variable, range, median, mean, and standard
#' deviation are supplied. Additionally, skewness and kurtosis are calculated,
#' and the robust Jarque-Bera test of normality is run. NA counts are
#' also reported.
#'
#' @return
#' `crep_desc()` returns a list of two tables. The first is the results for
#' any factors. The second is the results for any numeric variables.
#'
#' @export crep_desc
#' @examples
#' x <- runif(10)
#' y <- runif(10)
#' df <- data.frame(x, y)
#' crep_desc(df)
#'
#' @import stats

crep_desc <- function(df){
  # Build separate tables for strings/numeric variables
  stvar <- data.frame(matrix(ncol = 3))
  colnames(stvar) <- c("variable", "#levels", "NA")
  stvar <- stvar[-1,]
  numvar <- data.frame(matrix(ncol = 11))
  colnames(numvar) <- c("variable", "min", "median", "mean", "max", "sd",
                        "skewness", "kurtosis", "RJB", "RJB.p", "NA")
  numvar <- numvar[-1,]
  # separate out strings and numeric variables
  types <- c()
  for(i in 1:seq_along(colnames(df))){
    if(is.numeric(df[[i]])){types[i] <- "numeric"}
    else{types[i] <- "factor"}
 #   if(typeof(df[[i]]) %in%
 #          c("character", "logical", "date")){types[i] <- "factor"}
    if(is.factor(df[[i]])){types[i] <- "factor"}
    if(types[i] == "factor"){
      d <- c(colnames(df[i]), length(unique(df[[i]])), sum(is.na(df[[i]])))
      stvar[nrow(stvar) + 1, ] <- d
    }
    else{
      d <- c()
      d[1] <- colnames(df[i])
      d[2] <- round(min(df[[i]]), 3)
      d[3] <- round(median(df[[i]]), 3)
      d[4] <- round(mean(df[[i]]), 3)
      d[5] <- round(max(df[[i]]), 3)
      d[6] <- round(sd(df[[i]]), 3)
      d[7] <- round(moments::kurtosis(df[[i]]), 3)
      d[8] <- round(moments::skewness(df[[i]]), 3)
      rjb<- lawstat::rjb.test(df[[i]])
      d[9] <- round(rjb$statistic, 3)
      d[10] <- round(rjb$p.value, 3)
      d[11] <- sum(is.na(df[[i]]))
      numvar[nrow(numvar) + 1, ] <- d
    }
  }
  rownames(stvar) <- stvar[, 1]
  rownames(numvar) <- numvar[, 1]
  desc <- c()
  desc[[1]] <- stvar[, -1]
  desc[[2]] <- numvar[, -1]
  names(desc) <- c("categorical", "numeric")
  return(desc)
}
