initialization_function <- function(feature) {
  list(a = 0, b = 2, s_1 = 2, s_2 = 4)
}
repetition <- 5000
L <- 20 # nolint: object_name_linter.
i_0 <- 1


test_that("relbel() works", {
  dataset <- data.frame(
    x1 = c(
      0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
      1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18,
      0.78, 0.71, 0.98, 0.68, 1.00, 1.10, 0.78, 1.10
    ),
    x2 = -c(
      0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
      1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18,
      0.78, 0.71, 0.98, 0.68, 1.00, 1.10, 0.78, 1.10
    ),
    y = c(
      "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
      "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
      "B", "B", "B", "B", "B", "B", "B", "B"
    )
  )

  actual_result <- withr::with_seed(42, {
    relbel(
      dataset[1:2], dataset["y"],
      initialization_function, repetition, L, i_0
    )
  })
  expected_result <- matrix(
    c(
      1.304, 0.708,
      0.7272, 0.138
    ),
    nrow = 2, ncol = 2, byrow = TRUE,
    dimnames = list(
      c("relbel", "strength"),
      c("x1", "x2")
    )
  )

  expect_equal(actual_result, expected_result)
})

test_that("get_relbel_() works", {
  features <- list(
    sample1 = c(
      0.97, 0.72, 1.00, 0.81, 0.62, 1.32, 1.24, 0.99, 0.90, 0.74, 0.88, 0.94,
      1.16, 0.86, 0.85, 0.58, 0.57, 0.64, 0.98, 1.09, 0.92, 0.78, 1.24, 1.18
    ),
    sample2 = c(0.78, 0.71, 0.98, 0.68, 1.00, 1.10, 0.78, 1.10)
  )

  actual_result <- withr::with_seed(42, {
    get_relbel_(features, initialization_function, repetition, L, i_0)
  })
  expected_result <- c(
    relbel = 1.304,
    strength = 0.7272
  )

  expect_equal(actual_result, expected_result)
})
