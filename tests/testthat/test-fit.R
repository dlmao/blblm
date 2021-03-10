context("test-fit.r")

test_that("lmW coefficients are correct", {
  m <- model.frame(Volume ~ Girth + Height, trees)
  X <- model.matrix(Volume ~ Girth + Height, m)
  y <- model.response(m)
  w <- as.vector(rmultinom(1, 100, rep(1, nrow(X))))
  mod1 <- lmW(X, y, w)
  mod2 <- lm.wfit(X, y, w)
  mod1$coefficients <- as.vector(mod1$coefficients)
  names(mod1$coefficients) <- colnames(X)
  expect_equal(mod1$coefficients, mod2$coefficients)
})

test_that("lmW residuals are correct", {
  m <- model.frame(Volume ~ Girth + Height, trees)
  X <- model.matrix(Volume ~ Girth + Height, m)
  y <- model.response(m)
  w <- as.vector(rmultinom(1, 100, rep(1, nrow(X))))
  mod1 <- lmW(X, y, w)
  mod2 <- lm.wfit(X, y, w)
  mod1$residuals <- as.vector(mod1$residuals)
  names(mod2$residuals) <-  NULL
  expect_equal(mod1$residuals, mod2$residuals)
})

test_that("lmW weight is correct", {
  m <- model.frame(Volume ~ Girth + Height, trees)
  X <- model.matrix(Volume ~ Girth + Height, m)
  y <- model.response(m)
  w <- as.vector(rmultinom(1, 100, rep(1, nrow(X))))
  mod1 <- lmW(X, y, w)
  mod2 <- lm.wfit(X, y, w)
  mod1$weights <- as.double(mod1$weights)
  names(mod2$weights) <- NULL
  expect_equal(mod1$weights, mod2$weights)
})

test_that("lmW rank is correct", {
  m <- model.frame(Volume ~ Girth + Girth, trees)
  X <- model.matrix(Volume ~ Girth + Girth, m)
  y <- model.response(m)
  w <- as.vector(rmultinom(1, 100, rep(1, nrow(X))))
  mod1 <- lmW(X, y, w)
  mod2 <- lm.wfit(X, y, w)
  mod1$rank <- as.double(mod1$rank)
  names(mod2$rank) <- NULL
  expect_equal(mod1$rank, mod2$rank)
})
