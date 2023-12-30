#' Computes `sigma_sq` and `mu` for a single feature.
#' This function corresponds to part (ii) of the algorithm in the paper.
#'
#' @param mu_0 (number)
#' @param lambda_0 (number)
#' @param alpha_0 (number)
#' @param beta_0 (number)
#'
#' @return `list(sigma_sq, mu)`
#'
get_single_sigma_sq_and_mu <- function(mu_0, lambda_0, alpha_0, beta_0) {
  sigma_sq <- 1 / stats::rgamma(n = 1, shape = alpha_0, rate = beta_0)
  mu <- stats::rnorm(n = 1, mean = mu_0, sd = lambda_0 * sqrt(sigma_sq))

  return(list(sigma_sq = sigma_sq, mu = mu))
}
