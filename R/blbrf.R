## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' Bag of Little Boostrap for Random Forest
#'
#' Implementation of the Bag of Little Bootstrap algorithm for Random Forest.
#' Includes methods blb estimates for predictions. Based on the R Ranger package.
#'
#' @param formula object of class "formula"
#' @param data a dataframe
#' @param m an integer
#' @param B an integer
#' @param nthreads an integer
#'
#' @return blbrf
#' @export
#' @examples
#' blbrf(mpg ~ wt * hp, data = mtcars, m = 3, B = 10000, nthreads = 8)
#' @export
blbrf <- function(formula, data, m = 10, B = 500, nthreads = 1) {
  data_list <- split_data(data, m) # split data into m sets

  estimates <- map(
    data_list,
    ~ rf_each_subsample(formula = formula, data = ., B = B, nthreads = nthreads)
  )

  res <- list(estimates = estimates, levels = estimates[[1]]$forest$levels, formula = formula)
  class(res) <- "blbrf"
  invisible(res)
}


#' compute the estimates
#' @param formula formula
#' @param data split dataset
#' @param B integer number of repetitions
#' @param nthreads integer number of workers
rf_each_subsample <- function(formula, data, B, nthreads) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  ranger(formula, data, num.trees = B, probability = TRUE, keep.inbag = TRUE, num.threads = nthreads)
}


#' print.blbrf
#'
#' Prints the formula and level of the blbrf model
#'
#' @param x blbrf
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method print blbrf
print.blbrf <- function(x, ...) {
  cat("blbrf model:", capture.output(x$formula))
  cat(", levels:", capture.output(x$levels))
  cat("\n")
}


#' predict.blbrf
#'
#' Predicts with new observations. Return level of prediction, probability of
#' all levels, or confidence interval.
#'
#' @param object blbrf
#' @param new_data dataframe of new data entries
#' @param type a character vector specifying type of return. Default prediction.
#' @param level double level of confidence interval
#' @param ... further arguments passed to or from other methods.
#' @param nthreads integer number of workers
#' @export
#' @method predict blbrf
predict.blbrf <- function(object, new_data, type = "prediction", level = 0.95, nthreads = 1, ...) {
  est <- object$estimates
  if (type == "prediction") {
    map_mean(est, ~ predict(., new_data, type = "response", num.threads = nthreads)$predictions) %>%
      {
        colnames(.)[max.col(.)]
      } %>%
      factor(levels = object$levels)
  } else if (type == "CI") {
    pred <- map_mean(est, ~ predict(., new_dat, type = "se", num.threads = nthreads)$predictions)
    se <- map_mean(est, ~ predict(., new_dat, type = "se")$se)
    alpha <- 1 - level
    pred + c(-1, 1) * qnorm(1 - alpha / 2) * se
  } else if (type == "probability") {
    map_mean(est, ~ predict(., new_data, type = "response", num.threads = nthreads)$predictions)
  }
}
