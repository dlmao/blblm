#' @import purrr
#' @import furrr
#' @import future
#' @import stats
#' @import ranger
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' Bind two factors
#'
#' Create a new factor from two existing factors, where the new factor's levels
#' are the union of the levels of the input factors.
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
blbrf = function(formula, data, m = 10, B = 500, nthreads = 1) {
  data_list <- split_data(data, m) # split data into m sets
  if (nthreads == 1) {
    estimates <- map(
      data_list,
      ~ rf_each_subsample(formula = formula, data = ., B = B, nthreads = 1)
    )
  }
  else {
    estimates <- map(
      data_list,
      ~ rf_each_subsample(formula = formula, data = ., B = B, nthreads = nthreads)
    )
  }

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
#'
#' @param x blbrf
#' @export
#' @method print blbrf
print.blbrf <- function(x, ...) {
  cat("blbrf model:", capture.output(x$formula))
  cat(", levels:", capture.output(x$levels))
  cat("\n")
}


#' predict.blbrf
#'
#'
#' @param object blbrf
#' @param new_data dataframe of new data entries
#' @param type string type of return
#' @param level double level of confidence interval
#' @export
#' @method predict blbrf
predict.blbrf <- function(object, new_data, type = "prediction", level = 0.95, ...) {
  est <- object$estimates
  if(type == "prediction") {
    map_mean(est, ~ predict(., new_data, type = "response")$predictions) %>%
      {colnames(.)[max.col(.)]} %>% factor(levels = object$levels)
  } else if (type == "CI") {
    pred = map_mean(est, ~ predict(., new_dat, type = "se")$predictions)
    se = map_mean(est, ~ predict(., new_dat, type = "se")$se)
    alpha = 1 - level
    pred + c(-1, 1) * qnorm(1 - alpha / 2) * se
  } else if (type == "probability") {
    map_mean(est, ~ predict(., new_data, type = "response")$predictions)
  }
}
