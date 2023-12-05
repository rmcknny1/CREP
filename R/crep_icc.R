#' @title crep_easyicc
#'
#' @description Calculates the intraclass correlation coefficient on all
#' main effects and interactions. The supplied dataframe should be long,
#' with ratings in one column, rater in a second, and item identifier
#' (usually a number, 1-11 for the RIBA or 1-26 for the SOM) in a third.
#' Each row should be a single item: one rating from one rater on one subject.
#' If you have an expert rater to compare other raters to, you will need
#' to supply their associated factor name to the "expert" argument.
#'
#' Every other column in the dataframe will be included as a possible
#' confounder, which makes this procedure valid for repeated measures
#' analysis if a column for subject is included. All columns will be treated
#'  as factors unless specified otherwise.
#'
#' @usage crep_icc(df, rating = "rating", rater = "rater", item = "item")
#'
#' @param df The dataframe that contains your IRR data.
#' @param rating A string with the column name of your dependent variable
#'  - the ratings for each rater. Defaults to "rating".
#' @param rater A string with the column name of your 'rater' variable.
#' Defaults to "rater".
#' @param item A string with the column name of the item. Defaults to
#' "item".
#'
#' @export crep_icc
#'
#' @details
#' Rather than ANOVA, `crep_icc()` estimates the ICC using a linear mixed
#' effects approach as described in Chen et al. (2017), estimated through
#' REML. Specifically, it uses `lme4::lmer()` to estimate an LME model
#' using all main and interaction effects (except the highest-level
#' interaction). Because of this, values may differ marginally from the
#' ANOVA-based approaches implemented in `irrICC` or other packages.
#' Compared to ANOVA, the LME approach can handle missing data and
#' confounding effects, and will always return positive values.
#' It can also be used to examine the degree of agreement
#' for variables other than rater (see example below).
#'
#' Currently, the ICC value returned will always be ICC(2, 1). Future
#' releases may expand this to an ICC(x, k) model. Additionally, the
#' current function only looks at item, rater, and rating. Future
#' releases will allow for more independent variables.
#'
#' @importFrom lme4 lmer
#' @importFrom stats pf
#'
#' @return The ICC, with F and p values.
#'
#' @examples
#'
#' df <- data.frame(item = factor(rep(c(1, 2, 3, 4), 2)),
#'                  rating = c(2, 2, 3, 4, 1, 2, 3, 2),
#'                  rater = rep(c("r1", "r2"), each = 4))
#'
#'# Gives the ICC as normal
#'crep_icc(df)
#'
#'# Gives the ICC for items - the agreement between any two items
#'crep_icc(df, rater = "item", item = "rater")
#'

crep_icc <- function(df, rating = "rating", rater = "rater",
                     item = "item"){

  # # Mark columns as factors
  #
  # #fdf <- data.frame(as.factor(fdf))
  # if(length(num) >= 1){
  #   fdf <- subset(df, select = -item)
  #   ndf <- subset(df, num)
  #   ndf <- data.frame(as.numeric(ndf))
  #   df <- data.frame(fdf, ndf)
  # }
  #

  # Calculate REML model
  mdl <- lme4::lmer(formula = paste0(
    rating, " ~ 1 + ", item, " + (1|", rater, ") + (1|", item, ")"), data = df)

  # Extract variance components
  vc <- as.data.frame(lme4::VarCorr(mdl))
  row.names(vc) <- vc[, 1]
  icc <- vc[item, 4] / sum(vc[, 4])
  icc <- round(icc, 3)

  # Calculate F
  fnum <- nrow(unique(df[rater])) * vc[item, 4]
  fden <- sum(vc[, 4]) - vc[item, 4]
  f <- round(fnum / fden, 3) + 1
  rnum <- nrow(unique(df[rater]))
  dfnum <- (nrow(df) /  rnum) - 1
  dfden <- dfnum * (rnum - 1)
  p <- pf(f, dfnum, dfden, lower.tail = FALSE)
  results <- data.frame(icc, f, dfnum, dfden, p)
  colnames(results) <- c("ICC", "F", "df num", "df den", "p")
  return(results)
}

