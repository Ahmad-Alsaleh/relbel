test_that("get_single_a_b_s1_s2() works when `conf_level` is between 0 and 1", {
  feature <- 1:50
  conf_level <- 0.90

  actual_result <- get_single_a_b_s1_s2(feature, conf_level)
  expected_result <- list(
    a = 22.0437, b = 28.9563,
    s_1 = 12.52836, s_2 = 17.51796
  )

  expect_equal(actual_result, expected_result, tolerance = 1e-5)
})

test_that("get_single_a_b_s1_s2() raises an error for invalid `conf_level`", {
  feature <- 1:50

  expect_error(get_single_a_b_s1_s2(feature, conf_level = -2))
  expect_error(get_single_a_b_s1_s2(feature, conf_level = 2))

  expect_error(get_single_a_b_s1_s2(feature, conf_level = 0))
  expect_error(get_single_a_b_s1_s2(feature, conf_level = 1))
})
