#' @title AI/ML models
#'
#' @description
#' The \code{model} function generates AI/ML models
#'
#' @aliases model
#'
#' @param data data object with data to be modeled, read more \link[ai]{prodata}
#' @param type model type, lm (Fitting Linear Models) by default; available are lm, rlm, ctree, knn, knn1
#' @param config additional parameters for model, read more \link[ai]{config}
#' @param verbose if true the messages are displayed in console, false by default
#'
#' @return model list contains model, predicted, and expected values for all generated models
#'
#' @examples
#' \donttest{
#'
#' model_data <- data.frame(a = c(1,2,3,4,5,6),
#'                          b = c(1,2,3,4,5,6),
#'                          s = c(1,2,3,4,5,6))
#'
#' config <- config(formula = "a ~ b + s")
#'
#' model_data <- prodata(model_data, status_colname = "s")
#'
#' model(model_data, config)
#' }
#'
#' @export

model <- function(data, type = "lm", config = NULL, verbose = FALSE){

  if (is.null(data$SplitRatio)){ stop("Data object is required for data parameter, please use data() function first")}

  results <- list()

  # --- LM - Fitting Linear Models

  if ("lm" %in% type){

    # Parameters check
    if(is.null(config$formula)){ stop("Formula object is required for lm model, please use config() function first")}

    # Start
    if(verbose){print(paste("Running lm models for ", config$formula))}

    model <- stats::lm(stats::as.formula(config$formula),
                       data = data$train_data)

    predicted <- stats::predict(model, newdata = data$test_data)

    # Calc
    results["lm"] <- list( list(model = model,
                           predicted = predicted,
                           expected = data$test_data[ , data$status_colname]
     ))

     # End
    if(verbose){print(paste("Done!"))}

  }

  # --- RLM - Robust Fitting of Linear Models

  if ("rlm" %in% type){

    # Parameters check
    if(is.null(config$formula)){ stop("Formula object is required for rlm model, please use config() function first")}

    # Start
    if(verbose){print(paste("Running rlm models for ", config$formula))}

    model <- MASS::rlm(stats::as.formula(config$formula),
                       data = data$train_data)

    predicted <- stats::predict(model, newdata = data$test_data)

    # Calc
    results["rlm"] <- list( list(model = model,
                                predicted = predicted,
                                expected = data$test_data[ , data$status_colname]
    ))

    # End
    if(verbose){print(paste("Done!"))}

  }

  # --- ctree - Conditional Inference Trees

  if ("ctree" %in% type){

    # Parameters check
    if(is.null(config$formula)){ stop("Formula object is required for ctree model, please use config() function first")}

    # Start
    if(verbose){print(paste("Running ctree models for ", config$formula))}

    model <- party::ctree(stats::as.formula(config$formula),
                       data = data$train_data)

    predicted <- stats::predict(model, newdata = data$test_data)

    # Calc
    results["ctree"] <- list( list(model = model,
                                   predicted = predicted,
                                   expected = data$test_data[ , data$status_colname]
    ))

    # End
    if(verbose){print(paste("Done!"))}

  }

  # --- KNN - k-Nearest Neighbour Classification
  if ("knn" %in% type){

    # Parameters check
    if(is.null(config$k)){ stop("k object is required for knn model, please use config() function first or knn1 as model type")}

    # Start
    if(verbose){print(paste("Running knn models for k = ", config$k))}

    # Calc
    predicted <- class::knn(train = data$train_data,
                                    test = data$test_data,
                                    cl = data$train_data[ , data$status_colname],
                                    k = config$k
                            )

    results["knn"] <- list( list(model = paste0("k =", config$k),
                                 predicted = predicted,
                                 expected = data$test_data[ , data$status_colname]
    ))

    # End
    if(verbose){print(paste("Done!"))}

  }

  # --- KNN1 - 1-Nearest Neighbour Classification
  if ("knn1" %in% type){

    # Start
    if(verbose){print(paste("Running knn1 model"))}


    # Calc
    predicted <- class::knn1(train = data$train_data,
                            test = data$test_data,
                            cl = data$train_data[ , data$status_colname]
    )

    results["knn1"] <- list( list(model = paste0("k = 1"),
                                 predicted = predicted,
                                 expected = data$test_data[ , data$status_colname]
    ))

    # End
    if(verbose){print(paste("Done!"))}
  }

  return(results)

}
