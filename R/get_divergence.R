#' Computes a distribution of the prior & posterior KL divergence values.
#' This function corresponds to parts (iii) & (vi) of the algorithm in the paper
#'
#' @param features (list)
#' ```
#' list(sample1 = c(1, 2, 3), sample2 = c(4, 5, 6), ...)
#' ```
#' @param initialization_function (function)
#' should return `list(a, b, s_1, s_2)`
#' Default: `get_single_a_b_s1_s2()`.
#' @param repetition (integer)
#' number of times to repeat the computation
#' (i.e.: number of samples in the distribution)
#'
#' @return matrix of size `length(features)` x `repetition`
#' each row corresponds to a feature
#'
get_divergence_distribution <- function(
    features, initialization_function = NULL, repetition = 5000) {
  mu_0_lambda_0_alpha_0_beta_0 <- get_all_elicitations(
    features,
    initialization_function
  )
  divergences <- replicate(repetition, {
    get_divergence_(features, mu_0_lambda_0_alpha_0_beta_0)
  })
  divergences

  # ! continue from here: complete this function.
  # ! continue from after the loop in the example 3. (you need to start with the last part in the paper) # nolint
  # ! take a look at the relbel-bu code that I wrote long time ago.
}


#' Computes the prior & posterior KL divergence values.
#'
#' @param mu_0_lambda_0_alpha_0_beta_0 (list)
#' ```
#' list(
#'   sample1 = list(mu_0, lambda_0, alpha_0, beta_0),
#'   sample2 = ...
#' )
#' ```
#' @param features (list)
#' ```
#' list(sample1 = c(1, 2, 3), sample2 = c(4, 5, 6), ...)
#' ```
#'
#' @return `c(prior, post)`
#'
get_divergence_ <- function(features, mu_0_lambda_0_alpha_0_beta_0) {
  sigma_sq_and_mu <- get_all_sigma_sq_and_mu(
    features,
    mu_0_lambda_0_alpha_0_beta_0
  )
  mu_star <- get_mu_star(features, sigma_sq_and_mu)

  prior_and_post_divergence <- sapply(sigma_sq_and_mu, function(feature) {
    prior <- (mu_star$prior - feature$mu$prior)^2 / feature$sigma_sq$prior
    post <- (mu_star$post - feature$mu$post)^2 / feature$sigma_sq$post
    return(c(
      prior = prior,
      post = post
    ))
  })

  prior_divergence <- 0.5 * sum(prior_and_post_divergence["prior", ])
  post_divergence <- 0.5 * sum(prior_and_post_divergence["post", ])


  return(c(
    prior = prior_divergence,
    post = post_divergence
  ))
}
# todo: run document()
# todo: rename this file

###
# initialization_function <- function(feature) list(a = 0, b = 2, s_1 = 2, s_2 = 4) # nolint
###
