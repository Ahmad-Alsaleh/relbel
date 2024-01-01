#' Computes the prior & posterior KL divergence values.
#' This function corresponds to parts (iii) & (vi) of the algorithm in the paper
#'
#' @param features (list)
#' @param initialization_function (function) returns `list(a, b, s_1, s_2)`.
#' Default: `get_single_a_b_s1_s2()`.
#'
#' @return `list(prior, post)`
#'
get_divergence <- function(features, initialization_function = NULL) {
  sigma_sq_and_mu <- get_all_sigma_sq_and_mu(features, initialization_function)
  mu_star <- get_mu_star(features, sigma_sq_and_mu)

  prior_and_post <- sapply(sigma_sq_and_mu, function(feature) {
    prior <- (mu_star$prior - feature$mu$prior)^2 / feature$sigma_sq$prior
    post <- (mu_star$post - feature$mu$post)^2 / feature$sigma_sq$post
    return(c(
      prior = prior,
      post = post
    ))
  })

  prior <- 0.5 * sum(prior_and_post["prior", ])
  post <- 0.5 * sum(prior_and_post["post", ])


  return(list(
    prior = prior,
    post = post
  ))
}
