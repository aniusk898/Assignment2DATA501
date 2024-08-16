# tests/testthat/test-valInputs.R

data_test <- data.frame(
  y = c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
  x1 = c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
)
model_test <- lm(y ~ x1, data = data_test)

test_that("valInputs works correctly with valid inputs", {
  expect_true(valInputs(data_test, model_test))
})

test_that("valInputs throws an error when the model is not of class 'lm'", {
  expect_error(valInputs(data_test, list()),
               "The argument 'model' must be an object of class 'lm'.")
})

test_that("valInputs throws an error when the data is not a data frame", {
  expect_error(valInputs(matrix(data_test), model_test), "The data must be a data frame")
})

test_that("valInputs throws an error when the data contains NA values", {
  data_na <- data_test
  data_na[1, 1] <- NA
  expect_error(valInputs(data_na, model_test),
               "The data contains NA or Inf values")
})

test_that("valInputs throws an error when the data contains Inf values", {
  data_inf <- data_test
  data_inf[1, 1] <- Inf
  expect_error(valInputs(data_inf, model_test),
               "The data contains NA or Inf values")
})

test_that("valInputs throws an error when the data dimensions do not match the model",
          {
            data_short <- data_test[-1, ]
            expect_error(
              valInputs(data_short, model_test),
              "The dimensions of the data do not match the number of observations in the model"
            )
          })

test_that("valInputs throws an error when the data contains duplicated rows",
          {
            data_dup <- data_test
            data_dup[2, ] <- data_dup[1, ]
            expect_error(valInputs(data_dup, model_test),
                         "The data contains duplicated rows.")
          })

test_that("valInputs throws an error when the data contains character variables",
          {
            data_char <- data_test
            data_char$x1 <- as.character(data_char$x1)
            expect_error(valInputs(data_char, model_test),
                         "The data contains character variables")
          })

