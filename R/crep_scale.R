#' @title crep_scale
#'
#' @description When given a dataframe and a list of relevant columns, will
#' create new variables that either standardize (z-scores) or center
#' the supplied columns.
#'
#' @usage
#'
#' crep_scale(df, vars, type = c("standardize", "center"))
#'
#' @param df A dataframe
#' @param vars A string list with the names of the columns to be scaled
#' @param type The default, `"standardize"`, calculates z-scores for each
#' variable. `"center"` will subtract the mean of a variable from each of
#' its values.
#'
#' @export crep_scale
#' @return
#' crep_scale returns a new dataframe with the standardized variables
#' attached.
#'
#' @examples
#' crep_scale(crep_som, c("rating"), "center")
#'
#' crep_scale(crep_somwide, c("rater1", "rater2"), "standardize")

crep_scale <- function(df, vars, type = c("standardize", "center")){
  if(type == "standardize"){
    for(i in 1:seq_along(vars)){
      varname <- paste0(vars[i], "_std")
      std <- scale(df[[vars[i]]])
      colnames(std) <- varname
      df <- dplyr::bind_cols(list(df, std))
    }
  }
  if(type == "center"){
    for(i in 1:seq_along(vars)){
      varname <- paste0(vars[i], "_cen")
      cen <- scale(df[[vars[i]]], scale = FALSE)
      colnames(cen) <- varname
      df <- dplyr::bind_cols(list(df, cen))
    }
  }
  return(df)
}





