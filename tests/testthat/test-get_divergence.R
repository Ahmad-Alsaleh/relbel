test_that("get_divergence_() works", {
  features <- list(
    sample1 = c(
      0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
      1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18
    ),
    sample2 = c(0.78, 0.71, 0.98, 0.68, 1.00, 1.10, 0.78, 1.10)
  )

  mu_0_lambda_0_alpha_0_beta_0 <- list(
    sample1 = list(
      mu_0 = 1,
      lambda_0 = 0.25,
      alpha_0 = 6.454639,
      beta_0 = 26.84146
    ),
    sample2 = list(
      mu_0 = 1,
      lambda_0 = 0.25,
      alpha_0 = 6.454639,
      beta_0 = 26.84146
    )
  )

  actual_result <- withr::with_seed(42, {
    get_divergence_(features, mu_0_lambda_0_alpha_0_beta_0)
  })

  expected_result <- c(
    prior = 0.002511956,
    post = 0.002666063
  )

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
