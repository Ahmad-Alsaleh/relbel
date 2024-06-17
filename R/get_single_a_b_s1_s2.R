# Calculate the confidence intervals of the mean and standard deviation for a
# given feature. This function corresponds to the part "before" part (i) of the
# algorithm in the paper.
#
# @param feature (vector)
# @param conf_level (number) should be between 0 and 1, both exclusive
#
# @return `list(a, b, s_1, s_2)` where a and b are the lower and upper bound of
#   the mean and s_1 and s_2 are the lower and upper bound of the standard
#   deviation.
#
get_single_a_b_s1_s2 <- function(feature, conf_level = 0.95) {
  if (conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` should be between 0 and 1, both exclusive")
  }
  confidence_interval_of_mean <- stats::t.test(
    feature,
    conf.level = conf_level
  )$conf.int
  a <- confidence_interval_of_mean[1]
  b <- confidence_interval_of_mean[2]

  df <- length(feature) - 1
  s <- stats::sd(feature)
  chi_1 <- stats::qchisq((1 - conf_level) / 2, df, lower.tail = FALSE)
  chi_2 <- stats::qchisq((1 - conf_level) / 2, df, lower.tail = TRUE)
  s_1 <- sqrt(df * s^2 / chi_1)
  s_2 <- sqrt(df * s^2 / chi_2)

  return(list(a = a, b = b, s_1 = s_1, s_2 = s_2))
}
