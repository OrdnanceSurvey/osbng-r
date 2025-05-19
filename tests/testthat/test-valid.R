test_that("invalid BNG references are detected", {
  df <- readRDS(test_cases("is_valid_bng"))
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    expect_equal(is_valid_bng(r$bng_ref), r$expected)
  }
})


test_that("invalid resolutions are detected", {
  expect_equal(is_valid_bng_resolution(0), FALSE)
  expect_equal(is_valid_bng_resolution(10), TRUE)
  expect_equal(is_valid_bng_resolution("101"), FALSE)
  expect_equal(is_valid_bng_resolution("100"), TRUE)
  expect_equal(is_valid_bng_resolution("100m"), TRUE)
  expect_equal(is_valid_bng_resolution("5km"), TRUE)
  expect_equal(is_valid_bng_resolution(5000), TRUE)
  expect_equal(is_valid_bng_resolution(100000), TRUE)
})


test_that("resolutions are returned as integers", {
  df <- readRDS(test_cases("get_bng_resolution"))
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    expect_equal(get_bng_resolution(as_bng_reference(r$bng_ref)), r$expected)
  }
})


test_that("resolutions are returned as strings", {
  df <- readRDS(test_cases("get_bng_resolution_string"))
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    expect_equal(get_bng_resolution_string(as_bng_reference(r$bng_ref)), 
                 r$expected)
  }
})
