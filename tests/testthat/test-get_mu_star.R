test_that("get_mu_star() works", {
  features <- list(
    sample1 = c(
      0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
      1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18
    ),
    sample2 = c(0.78, 0.71, 0.98, 0.68, 1.00, 1.10, 0.78, 1.10)
  )

  sigma_sq_and_mu <- list(
    sample1 = list(
      sigma_sq = list(prior = 2.747348, post = 1.403449),
      mu = list(prior = 0.7660014, post = 1.068044)
    ),
    sample2 = list(
      sigma_sq = list(prior = 3.844381, post = 1.763836),
      mu = list(prior = 0.9479802, post = 0.9380883)
    )
  )

  actual_result <- withr::with_seed(42, {
    get_mu_star(features, sigma_sq_and_mu)
  })

  expected_result <- list(
    prior = 0.8418478,
    post = 1.01046
  )

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
