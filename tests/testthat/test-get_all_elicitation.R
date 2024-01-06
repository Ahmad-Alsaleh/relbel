test_that("get_all_elicitations() works", {
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

  actual_result <- get_all_elicitations(features, initialization_function)
  expected_result <- list(
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

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})

test_that("get_single_elicitation_() works", {
  a <- 0
  b <- 14
  s_1 <- 2
  s_2 <- 8

  actual_result <- get_single_elicitation_(a, b, s_1, s_2)
  expected_result <- list(
    mu_0 = 7,
    lambda_0 = 0.875,
    alpha_0 = 2.105783,
    beta_0 = 15.11126
  )

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})

test_that("get_single_alpha_and_beta_() works", {
  alpha_beta <- c(2, 2)
  s_1_sq <- 4
  s_2_sq <- 64

  actual_result <- get_single_alpha_and_beta_(alpha_beta, s_1_sq, s_2_sq)
  expected_result <- matrix(c(4.32261586, 0.01333706))

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})

test_that("newtons_method_() works", {
  f <- function(x, constant) {
    return(matrix(c(x[1]^2 + x[2]^2 - constant, x[1] - constant * x[2])))
  }
  x_0 <- matrix(c(1, 1))

  actual_result <- newtons_method_(f, x_0, 1)
  expected_result <- matrix(c(0.7500000, 0.7500000))

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})
