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


#' Bag of Little Bootstraps for fitting Linear Models
#'
#' Implementation of the Bag of Little Bootstrap algorithm for linear regression.
#' Includes methods blb estimates for common regression statistics.
#'
#' @param formula An object of class "formula".
#' @param data A data frame containing the variables of the model.
#' @param m An integer denoting the number of splits
#' @param B an integer denoting the number of bootstrap samples per split
#' @param nthreads an integer denoting number of workers
#'
#' @return blblm
#' @export
#' @examples
#' blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 10000, nthreads = 8)
#' @export
blblm <- function(formula, data, m = 10, B = 5000, nthreads = 1) {
  data_list <- split_data(data, m) # split data into m sets
  if (nthreads == 1) {
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
    )
  }
  else {
    plan(multiprocess, workers = nthreads, gc = TRUE)
    options(future.rng.onMisuse = "ignore")
    estimates <- future_map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
    )
    ## R CMD check: make sure any open connections are closed afterward
    if (!inherits(plan(), "sequential")) plan(sequential)
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#' @param data dataset
#' @param m number of splits
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#' @param formula formula
#' @param data split dataset
#' @param n length of dataset
#' @param B number of repetitions
lm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, lm2(X, y, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#' @param X Least Squares Matrix
#' @param y Least Squares target Matrix
#' @param n length of dataset
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' Rcpp version of lm1
#' @param X Least Squares Matrix
#' @param y Least Squares target Matrix
#' @param n length of dataset
lm2 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))

  fit <- lmW(X, y, freqs)
  fit$coefficients <- as.vector(fit$coefficients)
  fit$residuals <- as.vector(fit$residuals)
  fit$weights <- as.vector(fit$weights)
  fit$rank <- as.double(fit$rank)
  names(fit$coefficients) <- colnames(X)

  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#' @param fit lm object
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#' @param fit lm object
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' print.blblm
#'
#' Prints formula of blblm model
#'
#' @param x blblm
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' sigma.blblm
#'
#' Returns blb prediction of sigma of linear model.
#' Can return a confidence interval instead.
#'
#' @param object blblm
#' @param confidence boolean return confidence interval or not
#' @param level if confidence is TRUE level of confidence interval
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}


#' coef.blblm
#'
#' Returns blb coefficients of linear model
#'
#' @param object blblm
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' conflint.blblm
#'
#' blb confidence interval for Regression coefficients
#'
#' @param object blblm
#' @param parm string specific fit variable
#' @param level double confidence interval level
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
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

#' predict.blblm
#'
#' Predict with new observation. Can return prediction or confidence interval.
#'
#' @param object blblm
#' @param new_data dataframe of new data entries
#' @param confidence boolean return confidence interval
#' @param level double level of confidence interval
#' @param nthreads an integer denoting number of workers
#' @param ... further arguments passed to or from other methods.
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, nthreads = 1, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (nthreads == 1) {
    if (confidence) {
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
                 apply(1, mean_lwr_upr, level = level) %>%
                 t())
    } else {
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
  } else {
    plan(multiprocess, workers = nthreads, gc = TRUE)
    options(future.rng.onMisuse = "ignore")
    if (confidence) {
      map_future_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
                 apply(1, mean_lwr_upr, level = level) %>%
                 t())
    } else {
      map_future_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
    ## R CMD check: make sure any open connections are closed afterward
    if (!inherits(plan(), "sequential")) plan(sequential)
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_future_mean <- function(.x, .f, ...) {
  (future_map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
