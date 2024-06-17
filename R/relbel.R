# todo: make nice printing of the results (maybe use summary())
# todo: update the doc of relbel

#' Compute Relative Belief Ratio (RB) Score
#'
#' @description
#' This function computes the relative belief ratio based on the given features
#' and parameters.
#'
#' @param x a numeric vector matrix, or data frame; the feature(s).
#' @param y a categorical vector; the response variable.
#' @param initialization_function A function used for initializing the mean
#'        and standard deviation. See 'Details' for more information.
#' @param repetition The number of repetitions.
#' @param L The value of L.
#' @param i_0 The value of i_0.
#' @param y a
#' @return The computed relative belief ratio.
#' @export
#'
#' @author Ahmad Alsaleh
#'
#' @examples
#' initialization_function <- function(feature) {
#'   list(a = 0, b = 2, s_1 = 2, s_2 = 4)
#' }
#' repetition <- 5000
#' L <- 20
#' i_0 <- 1
#'
#' dataset <- data.frame(
#'   x1 = c(-0.8606568, 1.4141698, 0.2235239, -1.1968656, 0.7677741),
#'   x2 = c(-0.318, -0.712, -1.320, -0.008, 0.048),
#'   y = c("A", "B", "A", "B", "B")
#' )
#'
#' relbel(
#'   dataset[1:2], dataset["y"],
#'   initialization_function, repetition, L, i_0
#' )
#'
relbel <- function(
    x, y, initialization_function = NULL,
    repetition = 5000, L = 20, i_0 = 1) { # nolint: object_name_linter.
  x <- as.data.frame(x)
  y <- as.data.frame(y)

  check_arguments_(x, y, initialization_function, repetition, L, i_0)

  # todo: after fixing get_single_a_b_s1_s2(), consider removing this if-else...
  # ...and just using get_single_a_b_s1_s2(feature)
  if (is.null(initialization_function)) {
    initialization_function <- function(feature) {
      list(a = 0, b = 2, s_1 = 2, s_2 = 4)
    }
  }

  sapply(x, function(x_i) {
    features <- split(x_i, y)
    get_relbel_(features, initialization_function, repetition, L, i_0)
  })
}

# Computes the relative belief ratio and strength of evidence for a given set
# of features.
#
# @param features (list) `list(sample1 = c(1, 2), sample2 = c(3, 4), ...)`
# @param initialization_function (function) should return
# `list(a, b, s_1, s_2)` Default: `get_single_a_b_s1_s2()`.
# @param repetition (integer) number of times to repeat the computation
# (i.e.: number of samples in the distribution)
# @param L (integer)
# @param i_0 (integer)
# #
# @return `list(relbel, strength)`
#
get_relbel_ <- function(
    features, initialization_function,
    repetition, L, i_0) { # nolint: object_name_linter.
  distributions <- get_divergence_distribution(
    features, initialization_function,
    repetition
  )

  prior_quantiles <- sapply(1:(L + 1), function(i) {
    stats::quantile(distributions["prior", ], (i - 1) / L)
  })
  prior_quantiles[1] <- 0

  post_ecdf <- stats::ecdf(distributions["post", ])
  prior_ecdf <- stats::ecdf(distributions["prior", ])

  relbel_hat <- sapply(1:L, function(i) {
    # ! todo: ask the prof if this is actually correct? shouldn't we use prior_cdf somewhere? # nolint
    # ! The following line matches the paper's formula but the paper says:
    # ! "[RB hat is] the ration of the estimates of the posterior and prior [...]" # nolint
    L * (post_ecdf(prior_quantiles[i + 1]) - post_ecdf(prior_quantiles[i]))
  })

  prior_quantiles_p_0 <- prior_quantiles[i_0 + 1]
  # ! todo: ask the prof about this. it is different from the paper.
  relbel_0 <- post_ecdf(prior_quantiles_p_0) / prior_ecdf(prior_quantiles_p_0)
  strength <- sum(Filter(function(x) x <= relbel_0, relbel_hat)) / L

  return(c(
    relbel = relbel_0,
    strength = strength
  ))
}

check_arguments_ <- function(
    x, y, initialization_function,
    repetition, L, i_0) { # nolint: object_name_linter.
  stopifnot(
    "`x` must not be empty" = length(x) > 0,
    "`y` must not be empty" = length(x) > 0,
    "`x` and `y` should be the same length" = nrow(x) == nrow(y),
    "`x` must contain only numeric values" = all(sapply(x, is.numeric)),
    "`initialization_function` must be a function or NULL" =
      is.function(initialization_function) || is.null(initialization_function),
    "`repetition` must be a positive integer" =
      repetition %% 1 == 0 && repetition > 0,
    "`L` must be a positive integer" = L %% 1 == 0 && L > 0,
    "`i_0` must be positive and an integer" = i_0 %% 1 == 0 && i_0 > 0
  )
  if (i_0 / L < 0.03 || i_0 / L > 0.07) {
    warning("`i_0 / L` is not in the recommended range (around 0.05)")
  }
}
