test_that("invalid BNG references are detected", {
  df <- readRDS(test_cases("is_valid_bng"))
  expect_equal(is_valid_bng(df$bng_ref), df$expected)
})
