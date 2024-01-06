#' Computes `mu_0`, `lambda_0`, `alpha_0`, `beta_0` using elicitation for a all
#' features. This function corresponds to part (i) of the algorithm in the
#' paper.
#'
#' @param features (list)
#' ```
#' list(sample1 = c(1, 2, 3), sample2 = c(4, 5, 6), ...)
#' ```
#' @param initialization_function (function)
#' should return `list(a, b, s_1, s_2)`
#' Default: `get_single_a_b_s1_s2()`.
#'
#' @return
#' ```
#' list(
#'   sample1 = list(mu_0, lambda_0, alpha_0, beta_0),
#'   sample2 = ...
#' )
#' ```

get_all_elicitations <- function(features, initialization_function = NULL) {
  # todo: after fixing get_single_a_b_s1_s2(), consider removing this if-else...
  # ...and just using get_single_a_b_s1_s2(feature)
  if (is.null(initialization_function)) {
    initialization_function <- get_single_a_b_s1_s2
  }
  lapply(features, function(feature) {
    a_b_s1_s2 <- initialization_function(feature)
    get_single_elicitation_(
      a_b_s1_s2$a,
      a_b_s1_s2$b,
      a_b_s1_s2$s_1,
      a_b_s1_s2$s_2
    )
  })
}

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
get_single_elicitation_ <- function(a, b, s_1, s_2) {
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

  # TODO: ask the prof, why s_1_sq and s_2_sq are squared (that's not the case in the paper) # nolint
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
