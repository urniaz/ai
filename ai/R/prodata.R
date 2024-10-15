#' @title Data processing
#'
#' @description
#' The \code{prodata} function generates an data list for models. It additionally splits data for training and testing set by split ratio.
#'
#'
#' @param data data.frame with data to be modeled
#' @param status_colname name of the column in data where the true results (true positive, expected) values are listed
#' @param SplitRatio Splitting ratio; 0.75 means 75\% data for training and 25\% for testing, more: \link[caTools]{sample.split}
#'
#' @return data list
#'
#' @examples
#' \donttest{
#'
#' model_data <- data.frame(a = c(1,2,3,4,5,6),
#'                          b = c(1,2,3,4,5,6),
#'                          s = c(1,2,3,4,5,6))
#'
#' prodata(data = model_data, status_colname = "s")
#'
#'}
#'
#' @export

prodata <- function(data, status_colname, SplitRatio = 0.75){

  # Data split for training set and testing set

  sample_split <- caTools::sample.split(Y = data[ , status_colname], SplitRatio = SplitRatio)
  test_data <- base::subset(x = data, sample_split == FALSE)
  train_data <- base::subset(x = data, sample_split == TRUE)

  data <- list(SplitRatio = SplitRatio,
               status_colname = status_colname,
               sample_split = sample_split,
               test_data = test_data,
               train_data = train_data
  )

  return(data)

}
