test_that("invalid BNG references are detected", {
  df <- readRDS(test_cases("is_valid_bng"))
  for (i in 1:nrow(df)) {
    r <- df[i, ]
    expect_equal(is_valid_bng(r$bng_ref), r$expected)
  }
})


test_that("resolutions are returned as integers", {
  df <- readRDS(test_cases("get_bng_resolution"))
  for (i in 1:nrow(df)) {
    r <- df[i, ]
    expect_equal(get_bng_resolution(as_bng_reference(r$bng_ref)), r$expected)
  }
})


test_that("resolution are returned as strings", {
  df <- readRDS(test_cases("get_bng_resolution_string"))
  for (i in 1:nrow(df)) {
    r <- df[i, ]
    expect_equal(get_bng_resolution_string(as_bng_reference(r$bng_ref)), r$expected)
  }
})


