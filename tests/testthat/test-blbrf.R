context("test-blbrf.r")

test_that("factor levels of tree are correct", {
  model <- blbrf(Species ~ ., data = iris, m = 2, B = 100, nthreads = 1)
  expect_identical(model$levels, levels(iris$Species))
})

test_that("ensure end probs are the mean prob of all models", {
  model <- blbrf(Species ~ ., data = iris, m = 2, B = 100, nthreads = 1)
  new_dat <- iris[1,-5]
  x1 = model$estimates$`1`
  predicted1 = predict(x1, new_dat)
  x2 = model$estimates$`2`
  predicted2 = predict(x2, new_dat)
  m1 = (predicted1$predictions + predicted2$predictions) / 2
  expect_identical(m1, predict(model, new_dat, type = 'probability'))
})