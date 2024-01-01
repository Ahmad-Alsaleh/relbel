test_that("get_single_sigma_sq_mu_prior() works", {
  mu_0 <- 7
  lambda_0 <- 0.875
  alpha_0 <- 2.105783
  beta_0 <- 15.11126

  actual_result <- withr::with_seed(42, {
    get_single_sigma_sq_mu_prior(mu_0, lambda_0, alpha_0, beta_0)
  })

  expected_result <- list(
    sigma_sq = 3.963156,
    mu = 6.01634
  )
  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
