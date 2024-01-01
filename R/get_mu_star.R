#' Computes prior & posterior & `mu_star`.
#'
#' @param features (list)
#' @param sigma_sq_and_mu ()
#'
#' @return
#' @export
#'
#' @examples
get_mu_star <- function(features, sigma_sq_and_mu) {
  numerator_and_denominator <- sapply(sigma_sq_and_mu, function(feature) {
    prior <- c(
      numerator = feature$mu$prior / feature$sigma_sq$prior,
      denominator = 1 / feature$sigma_sq$prior
    )
    post <- c(
      numerator = feature$mu$post / feature$sigma_sq$post,
      denominator = 1 / feature$sigma_sq$post
    )
    return(c(
      prior = prior,
      post = post
    ))
  })

  mu_star_prior <- sum(numerator_and_denominator["prior.numerator", ]) /
    sum(numerator_and_denominator["prior.denominator", ])

  mu_star_post <- sum(numerator_and_denominator["post.numerator", ]) /
    sum(numerator_and_denominator["post.denominator", ])

  return(list(
    prior = mu_star_prior,
    post = mu_star_post
  ))
}
