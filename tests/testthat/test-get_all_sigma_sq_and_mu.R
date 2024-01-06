test_that("get_all_sigma_sq_and_mu() works", {
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
    get_all_sigma_sq_and_mu(features, mu_0_lambda_0_alpha_0_beta_0)
  })

  expected_result <- list(
    sample1 = list(
      sigma_sq = list(prior = 2.747348, post = 1.403449),
      mu = list(prior = 0.7660014, post = 1.068044)
    ),
    sample2 = list(
      sigma_sq = list(prior = 3.844381, post = 1.763836),
      mu = list(prior = 0.9479802, post = 0.9380883)
    )
  )

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})

test_that("get_single_sigma_sq_mu_prior_() works", {
  mu_0 <- 7
  lambda_0 <- 0.875
  alpha_0 <- 2.105783
  beta_0 <- 15.11126

  actual_result <- withr::with_seed(42, {
    get_single_sigma_sq_mu_prior_(mu_0, lambda_0, alpha_0, beta_0)
  })

  expected_result <- list(
    sigma_sq = 3.963156,
    mu = 6.01634
  )
  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})

test_that("get_single_sigma_sq_mu_post_() works", {
  feature <- c(
    0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
    1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18
  )
  mu_0 <- 1
  lambda_0 <- 0.25
  alpha_0 <- 6.454639
  beta_0 <- 26.84146

  actual_result <- withr::with_seed(42, {
    get_single_sigma_sq_mu_post_(mu_0, lambda_0, alpha_0, beta_0, feature)
  })

  expected_result <- list(
    sigma_sq = 1.13083,
    mu = 0.8545522
  )
  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
