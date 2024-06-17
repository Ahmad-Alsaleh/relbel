# Computes prior & posterior `sigma_sq` and `mu` for all features. This
# function corresponds to parts (ii) & (v) of the algorithm in the paper.
#
# @param features (list)
# ```
# list(sample1 = c(1, 2, 3), sample2 = c(4, 5, 6), ...)
# ```
# @param mu_0_lambda_0_alpha_0_beta_0 (list)
# ```
# list(
#   sample1 = list(mu_0, lambda_0, alpha_0, beta_0),
#   sample2 = ...
# )
# ```
# @return
# ```
# list(
#   sample1 = list(
#     sigma_sq = list(prior, post),
#     mu = list(prior, post)
#   ),
#   sample2 = ...
# )
# ```
#
get_all_sigma_sq_and_mu <- function(features, mu_0_lambda_0_alpha_0_beta_0) {
  mapply(function(feature, single_mu_0_lambda_0_alpha_0_beta_0) { # nolint
    sigma_sq_and_mu_prior <- get_single_sigma_sq_mu_prior_(
      single_mu_0_lambda_0_alpha_0_beta_0$mu_0,
      single_mu_0_lambda_0_alpha_0_beta_0$lambda_0,
      single_mu_0_lambda_0_alpha_0_beta_0$alpha_0,
      single_mu_0_lambda_0_alpha_0_beta_0$beta_0
    )

    sigma_sq_and_mu_post <- get_single_sigma_sq_mu_post_(
      single_mu_0_lambda_0_alpha_0_beta_0$mu_0,
      single_mu_0_lambda_0_alpha_0_beta_0$lambda_0,
      single_mu_0_lambda_0_alpha_0_beta_0$alpha_0,
      single_mu_0_lambda_0_alpha_0_beta_0$beta_0,
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
  }, features, mu_0_lambda_0_alpha_0_beta_0, SIMPLIFY = FALSE)
}

# Computes "prior" `sigma_sq` and `mu` for a single feature.
#
# @param mu_0 (number)
# @param lambda_0 (number)
# @param alpha_0 (number)
# @param beta_0 (number)
#
# @return `list(sigma_sq, mu)`
#
get_single_sigma_sq_mu_prior_ <- function(mu_0, lambda_0, alpha_0, beta_0) {
  sigma_sq <- 1 / stats::rgamma(n = 1, shape = alpha_0, rate = beta_0)
  mu <- stats::rnorm(n = 1, mean = mu_0, sd = lambda_0 * sqrt(sigma_sq))

  return(list(sigma_sq = sigma_sq, mu = mu))
}


# Computes "posterior" `sigma_sq` and `mu` for a single feature.
#
# @param mu_0 (number)
# @param lambda_0 (number)
# @param alpha_0 (number)
# @param beta_0 (number)
# @param feature (vector)
#
# @return `list(sigma_sq, mu)`
#
get_single_sigma_sq_mu_post_ <- function(
    mu_0, lambda_0,
    alpha_0, beta_0,
    feature) {
  n <- length(feature)
  x_bar <- mean(feature)

  beta_x <- beta_0 + (n - 1) * 0.5 * stats::sd(feature)^2 +
    0.5 * n * (x_bar - mu_0)^2 / (n * lambda_0^2 + 1)

  sigma_sq <- 1 / stats::rgamma(n = 1, shape = alpha_0 + 0.5 * n, rate = beta_x)

  sigma_x_sq <- sigma_sq / (n + 1 / lambda_0^2)

  mu_x <- (mu_0 / lambda_0^2 + n * x_bar) / (n + 1 / lambda_0^2)

  mu <- stats::rnorm(n = 1, mean = mu_x, sd = sqrt(sigma_x_sq))

  return(list(sigma_sq = sigma_sq, mu = mu))
}
