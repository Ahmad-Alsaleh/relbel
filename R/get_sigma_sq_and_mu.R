#' Computes prior & posterior `sigma_sq` and `mu` for all features.
#' This function corresponds to parts (ii) & (v) of the algorithm in the paper.
#'
#' @param features (list)
#' @param initialization_function (function) returns `list(a, b, s_1, s_2)`.
#' Default: `get_single_a_b_s1_s2()`.
#'
#' @return
#' ```
#' list(
#'   sample1 = list(
#'     sigma_sq = list(prior, post),
#'     mu = list(prior, post)
#'   ),
#'   sample2 = ...
#' )
#' ```
#'
get_all_sigma_sq_and_mu <- function(features, initialization_function = NULL) {
  # TODO: after fixing get_single_a_b_s1_s2(), consider removing this if-else and just using get_single_a_b_s1_s2(feature) # nolint: line_length_linter.
  if (is.null(initialization_function)) {
    initialization_function <- get_single_a_b_s1_s2
  }

  lapply(features, function(feature) {
    a_b_s1_s2 <- initialization_function(feature)

    mu_0_lambda_0_alpha_0_beta_0 <- get_single_elicitation(
      a_b_s1_s2$a,
      a_b_s1_s2$b,
      a_b_s1_s2$s_1,
      a_b_s1_s2$s_2
    )

    sigma_sq_and_mu_prior <- get_single_sigma_sq_mu_prior_(
      mu_0_lambda_0_alpha_0_beta_0$mu_0,
      mu_0_lambda_0_alpha_0_beta_0$lambda_0,
      mu_0_lambda_0_alpha_0_beta_0$alpha_0,
      mu_0_lambda_0_alpha_0_beta_0$beta_0
    )

    sigma_sq_and_mu_post <- get_single_sigma_sq_mu_post_(
      mu_0_lambda_0_alpha_0_beta_0$mu_0,
      mu_0_lambda_0_alpha_0_beta_0$lambda_0,
      mu_0_lambda_0_alpha_0_beta_0$alpha_0,
      mu_0_lambda_0_alpha_0_beta_0$beta_0,
      feature
    )

    return(list(
      sigma_sq = list(
        prior = sigma_sq_and_mu_prior$sigma_sq,
        post = sigma_sq_and_mu_post$sigma_sq
      ),
      mu = list(
        prior = sigma_sq_and_mu_prior$mu,
        post = sigma_sq_and_mu_post$mu
      )
    ))
  })
}

#' Computes **_prior_** `sigma_sq` and `mu` for a single feature.
#'
#' @param mu_0 (number)
#' @param lambda_0 (number)
#' @param alpha_0 (number)
#' @param beta_0 (number)
#'
#' @return `list(sigma_sq, mu)`
#'
get_single_sigma_sq_mu_prior_ <- function(mu_0, lambda_0, alpha_0, beta_0) {
  sigma_sq <- 1 / stats::rgamma(n = 1, shape = alpha_0, rate = beta_0)
  mu <- stats::rnorm(n = 1, mean = mu_0, sd = lambda_0 * sqrt(sigma_sq))

  return(list(sigma_sq = sigma_sq, mu = mu))
}


#' Computes **_posterior_** `sigma_sq` and `mu` for a single feature.
#'
#' @param mu_0 (number)
#' @param lambda_0 (number)
#' @param alpha_0 (number)
#' @param beta_0 (number)
#'
#' @return `list(sigma_sq, mu)`
#'
get_single_sigma_sq_mu_post_ <- function(
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
