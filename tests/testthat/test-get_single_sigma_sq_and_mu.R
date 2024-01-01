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

test_that("get_single_sigma_sq_mu_post() works", {
  feature <- c(
    0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
    1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18
  )
  mu_0 <- 1
  lambda_0 <- 0.25
  alpha_0 <- 6.454639
  beta_0 <- 26.84146

  actual_result <- withr::with_seed(42, {
    get_single_sigma_sq_mu_post(mu_0, lambda_0, alpha_0, beta_0, feature)
  })

  expected_result <- list(
    sigma_sq = 1.13083,
    mu = 0.8545522
  )
  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
