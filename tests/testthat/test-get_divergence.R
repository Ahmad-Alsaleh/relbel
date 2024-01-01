test_that("get_divergence() works", {
  features <- list(
    sample1 = c(
      0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
      1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18
    ),
    sample2 = c(0.78, 0.71, 0.98, 0.68, 1.00, 1.10, 0.78, 1.10)
  )

  initialization_function <- function(feature) {
    list(a = 0, b = 2, s_1 = 2, s_2 = 4)
  }

  actual_result <- withr::with_seed(42, {
    get_divergence(features, initialization_function)
  })

  expected_result <- list(
    prior = 0.002511956,
    post = 0.002666063
  )

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
