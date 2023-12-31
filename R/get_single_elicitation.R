#' Computes `mu_0`, `lambda_0`, `alpha_0`, `beta_0` using elicitation
#' for a single feature.
#' This function corresponds to part (i) of the algorithm in the paper.
#'
#' @param a (number)
#' @param b (number)
#' @param s_1 (number)
#' @param s_2 (number)
#'
#' @return `list(mu_0, lambda_0, alpha_0, beta_0)`
#'
get_single_elicitation <- function(a, b, s_1, s_2) {
  mu_0 <- (a + b) / 2
  lambda_0 <- (b - a) / (2 * s_2)

  alpha_beta <- newtons_method_(
    f = get_single_alpha_and_beta_,
    x_0 = matrix(c(2, 2)), s_1^2, s_2^2
  )

  return(list(
    mu_0 = mu_0, lambda_0 = lambda_0,
    alpha_0 = alpha_beta[1], beta_0 = alpha_beta[2]
  ))
}

get_single_alpha_and_beta_ <- function(alpha_beta, s_1_sq, s_2_sq) {
  probability <- 0.9995
  alpha_0 <- alpha_beta[1]
  beta_0 <- alpha_beta[2]

  result <- matrix(nrow = 2, ncol = 1)
  result[1] <- stats::qgamma(probability, alpha_0, beta_0) - (stats::qnorm(probability) / s_1_sq)^2 # nolint: line_length_linter.
  result[2] <- stats::qgamma(1 - probability, alpha_0, beta_0) - (stats::qnorm(probability) / s_2_sq)^2 # nolint: line_length_linter.

  return(result)
}

newtons_method_ <- function(f, x_0, ...) {
  TOLERANCE <- 0.5 # nolint: object_name_linter.
  MAX_ITERATION <- 1000 # nolint: object_name_linter.
  h <- 1e-9

  x <- x_0
  delta <- x
  iteration_i <- 0


  while (sum(abs(delta)) > TOLERANCE && iteration_i < MAX_ITERATION) {
    f_of_x <- f(x, ...)
    Jx <- matrix(nrow = length(x), ncol = length(x)) # nolint: object_name_linter, line_length_linter.

    for (i in seq_along(x)) {
      x_plus_h <- x
      x_plus_h[i] <- x_plus_h[i] + h

      f_of_x_plus_h <- f(x_plus_h, ...)
      Jx[, i] <- (f_of_x_plus_h - f_of_x) / h # nolint: object_name_linter.
    }

    delta <- -solve(Jx) %*% f_of_x
    x <- x + delta
    iteration_i <- iteration_i + 1
  }

  return(x)
}
