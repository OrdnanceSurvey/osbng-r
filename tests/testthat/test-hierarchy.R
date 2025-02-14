test_that("bng parents are returned", {
  df <- readRDS(test_cases("bng_to_parent"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    if (r$expected == "expect_error") {
      expect_error(bng_to_parent(r$bng_ref, r$resolution))
    } else {
      if (r$resolution == "NULL") {
        expect_equal(bng_to_parent(as_bng_reference(r$bng_ref)), 
                     as_bng_reference(r$expected))
      } else {
        expect_equal(bng_to_parent(as_bng_reference(r$bng_ref), 
                                   type.convert(r$resolution, as.is = TRUE)), 
                     as_bng_reference(r$expected))
      }
    }
  }
})


test_that("bng children are returned", {
  df <- readRDS(test_cases("bng_to_children"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    if (r$expected == "expect_error") {
      expect_error(bng_to_children(r$bng_ref, r$resolution))
    } else {
      if (r$resolution == "NULL") {
        expect_equal(bng_to_children(as_bng_reference(r$bng_ref)), 
                     as_bng_reference(r$expected[[1]]))
      } else {
        expect_equal(bng_to_children(as_bng_reference(r$bng_ref), 
                                   type.convert(r$resolution, as.is = TRUE)), 
                     as_bng_reference(r$expected[[1]]))
      }
    }
  }
})
