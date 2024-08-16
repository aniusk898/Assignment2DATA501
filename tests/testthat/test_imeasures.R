test_that("cooks_distance works correctly", {
  data <- data.frame(
    y = c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
    x1 = c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
  )
  model <- lm(y ~ x1, data = data)
  cooks_d <- cooks_distance(model)

  expect_length(cooks_d, nrow(data))
  expect_type(cooks_d, "double")
})
