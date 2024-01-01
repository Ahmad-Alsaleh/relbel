#' Computes **_prior_** `sigma_sq` and `mu` for a single feature.
#' This function corresponds to part (ii) of the algorithm in the paper.
#'
#' @param mu_0 (number)
#' @param lambda_0 (number)
#' @param alpha_0 (number)
#' @param beta_0 (number)
#'
#' @return `list(sigma_sq, mu)`
#'
get_single_sigma_sq_mu_prior <- function(mu_0, lambda_0, alpha_0, beta_0) {
  sigma_sq <- 1 / stats::rgamma(n = 1, shape = alpha_0, rate = beta_0)
  mu <- stats::rnorm(n = 1, mean = mu_0, sd = lambda_0 * sqrt(sigma_sq))

  return(list(sigma_sq = sigma_sq, mu = mu))
}


#' Computes **_posterior_** `sigma_sq` and `mu` for a single feature.
#' This function corresponds to part (v) of the algorithm in the paper.
#'
#' @param mu_0 (number)
#' @param lambda_0 (number)
#' @param alpha_0 (number)
#' @param beta_0 (number)
#'
#' @return `list(sigma_sq, mu)`
#'
get_single_sigma_sq_mu_post <- function(
    mu_0, lambda_0,
    alpha_0, beta_0,
    feature) {
  n <- length(feature)
  x_bar <- mean(feature)

  beta_x <- beta_0 + (n - 1) * 0.5 * sd(feature)^2 +
    0.5 * n * (x_bar - mu_0)^2 / (n * lambda_0^2 + 1)

  sigma_sq <- 1 / stats::rgamma(n = 1, shape = alpha_0 + 0.5 * n, rate = beta_x)

  sigma_x_sq <- sigma_sq / (n + 1 / lambda_0^2)

  mu_x <- (mu_0 / lambda_0^2 + n * x_bar) / (n + 1 / lambda_0^2)

  mu <- stats::rnorm(n = 1, mean = mu_x, sd = sqrt(sigma_x_sq))

  return(list(sigma_sq = sigma_sq, mu = mu))
}
