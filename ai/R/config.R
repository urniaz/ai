#' @title Models parameters
#'
#' @description
#' The \code{config} function sets additional models parameters
#'
#' @aliases config
#'
#' @param formula formula parameter for eg. linear models including lm, rlm, read more: \link[stats]{lm}
#' @param k number of neighbors considered from knn models, read more: \link[class]{knn}
#'
#' @return configuration list contains models parameters different than defaults
#'
#' @examples
#' \donttest{
#'
#' config(formula = "Status ~ Value")
#'
#'}
#'
#' @export

config <- function(formula = NULL, k = NULL){

  config <- list()

  # --- LM / RLM - Fitting Linear Models

  config["formula"] <- list( formula = formula)

  # --- KNN / k-Nearest Neighbour Classification

  config["k"] <- list( k = k)

  return(config)

}
