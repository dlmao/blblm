#' @import purrr
#' @import furrr
#' @import future
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' Bag of Little Bootstraps for Binomial Logistic Regression
#'
#' Implementation of the Bag of Little Bootstrap algorithm for binomial Logistic
#' Regression. Includes methods blb estimates for common glm statistics.
#'
#' @param formula An object of class "formula".
#' @param data A data frame containing the variables of the model.
#' @param m An integer denoting the number of splits
#' @param B an integer denoting the number of bootstrap samples per split
#' @param nthreads an integer denoting number of workers
#'
#' @return blbglm
#' @export
#' @examples
#' blbglm(Species ~ ., data = iris, m = 2, B = 5000, nthreads = 8)
#' @export
blbglm <- function(formula, data, m = 10, B = 5000, nthreads = 1) {
  data_list <- split_data(data, m) # split data into m sets
  if (nthreads == 1) {
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
    )
  }
  else {
    plan(multiprocess, workers = nthreads, gc = TRUE)
    options(future.rng.onMisuse = "ignore")
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
    )
    ## R CMD check: make sure any open connections are closed afterward
    if (!inherits(plan(), "sequential")) plan(sequential)
  }
  response <- all.vars(formula)[1]
  res <- list(estimates = estimates, formula = formula, levels = levels(data[[response]]))
  class(res) <- "blbglm"
  invisible(res)
}

#' compute the estimates
#' @param formula formula
#' @param data split dataset
#' @param n length of dataset
#' @param B number of repetitions
glm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, glm1(X, y, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#' @param X Least Squares Matrix
#' @param y Least Squares target Matrix
#' @param n length of dataset
glm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- glm.fit(X, y, freqs, family = binomial(link = "logit"))
  list(coef = blbcoef(fit))
}

#' print.blbglm
#'
#' Prints formula of blbglm model
#'
#' @param x blbglm
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method print blbglm
print.blbglm <- function(x, ...) {
  cat("blbglm model:", capture.output(x$formula))
  cat(", levels:", capture.output(x$levels))
  cat("\n")
}


#' coef.blbglm
#'
#' Returns blb coefficients of logistic regression model
#'
#' @param object blbglm
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method coef blbglm
coef.blbglm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' conflint.blbglm
#'
#' blb confidence interval for logistic regression coefficients
#'
#' @param object blbglm
#' @param parm string specific fit variable
#' @param level double confidence interval level
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method confint blbglm
confint.blbglm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}


#' predict.blbglm
#'
#' Predict with new observation. Can return prediction or confidence interval.
#'
#' @param object blbglm
#' @param new_data dataframe of new data entries
#' @param type a character vector specifying type of return. Default prediction.
#' @param level double level of confidence interval
#' @param nthreads an integer denoting number of workers
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method predict blbglm
predict.blbglm <- function(object, new_data, type = "prediction", level = 0.95, nthreads = 1, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (nthreads == 1) {
    if (type == "CI") {
      map_mean(est, ~ map_cbind(., ~ 1 / (1 + exp(-X %*% .$coef))) %>%
        apply(1, mean_lwr_upr, level = level) %>%
        t())
    } else if (type == "prediction") {
      map_mean(est, ~ map_cbind(., ~ 1 / (1 + exp(-X %*% .$coef))) %>% rowMeans()) %>%
        {
          ifelse(. > 0.5, object$levels[1], object$levels[2])
        } %>%
        factor(levels = object$levels)
    } else if (type == "probability") {
      map_mean(est, ~ map_cbind(., ~ 1 / (1 + exp(-X %*% .$coef))) %>% rowMeans())
    }
  } else {
    plan(multiprocess, workers = nthreads, gc = TRUE)
    options(future.rng.onMisuse = "ignore")
    if (type == "CI") {
      map_future_mean(est, ~ map_cbind(., ~ 1 / (1 + exp(-X %*% .$coef))) %>%
        apply(1, mean_lwr_upr, level = level) %>%
        t())
    } else if (type == "prediction") {
      map_future_mean(est, ~ map_cbind(., ~ 1 / (1 + exp(-X %*% .$coef))) %>% rowMeans()) %>%
        {
          ifelse(. > 0.5, object$levels[1], object$levels[2])
        } %>%
        factor(levels = object$levels)
    } else if (type == "probability") {
      map_future_mean(est, ~ map_cbind(., ~ 1 / (1 + exp(-X %*% .$coef))) %>% rowMeans())
    }
    ## R CMD check: make sure any open connections are closed afterward
    if (!inherits(plan(), "sequential")) plan(sequential)
  }
}
