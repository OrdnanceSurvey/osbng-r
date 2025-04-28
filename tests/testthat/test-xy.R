test_that("eastings and northings convert", {
  # general function inputs
  expect_error(xy_to_bng(350000, 550000))
  
  # conversion test cases
  df <- readRDS(test_cases("xy_to_bng"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    if (r$expected == "expect_error") {
      expect_error(xy_to_bng(r$easting, r$northing, 
                             type.convert(r$resolution, as.is = TRUE)))
    } else {
      expect_equal(xy_to_bng(r$easting, r$northing, 
                             type.convert(r$resolution, as.is = TRUE)), 
                   as_bng_reference(r$expected))
    }
  }
})


test_that("grid reference convert to coordinates", {
  # conversion test cases
  df <- readRDS(test_cases("bng_to_xy"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_equal(bng_to_xy(as_bng_reference(r$bng_ref), position = r$position),
                 matrix(r$expected[[1]], ncol = 2))
  }
})