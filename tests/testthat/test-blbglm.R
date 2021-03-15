context("test-blbglm.r")

library(kernlab)
data(spam)

test_that("factor levels are correct", {
  model <- blbglm(type ~ make + address + all, data = spam, m = 2, B = 500, nthreads = 1)
  expect_identical(model$levels, levels(spam$type))
})

test_that("ensure prob of each subset is done correctly", {
  model <- blbglm(type ~ make + address + all, data = spam, m = 1, B = 2, nthreads = 1)
  new_data <- spam[1,-58]
  X <- matrix(c(1, 0, 0.64, 0.64), 1, 4)
  est <- model$estimates$`1`
  p1 <- 1 / (1 + exp(-X %*% est[[1]]$coef))
  p2 <- 1 / (1 + exp(-X %*% est[[2]]$coef))
  a <- as.numeric((p1 + p2) / 2)
  b <- predict(model, new_data, type = "probability")
  names(b) <- NULL
  expect_equal(a, b)
})

test_that("ensure prob is reduced correctly", {
  model <- blbglm(type ~ make + address + all, data = spam, m = 2, B = 1, nthreads = 1)
  new_data <- spam[1,-58]
  X <- matrix(c(1, 0, 0.64, 0.64), 1, 4)
  est1 <- model$estimates$`1`
  est2 <- model$estimates$`2`
  p1 <- 1 / (1 + exp(-X %*% est1[[1]]$coef))
  p2 <- 1 / (1 + exp(-X %*% est2[[1]]$coef))
  a <- as.numeric((p1 + p2) / 2)
  b <- predict(model, new_data, type = "probability")
  names(b) <- NULL
  expect_equal(a, b)
})

