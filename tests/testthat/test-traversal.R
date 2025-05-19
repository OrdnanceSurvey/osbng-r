test_that("bng neighbours are tested", {
  df <- readRDS(test_cases("bng_is_neighbour"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    if (r$expected == "expect_error") {
      expect_error(bng_is_neighbour(as_bng_reference(r$bng_ref1), 
                                    as_bng_reference(r$bng_ref2)))
    } else {
      expect_equal(bng_is_neighbour(as_bng_reference(r$bng_ref1), 
                                    as_bng_reference(r$bng_ref2)), 
                   as.logical(r$expected))
    }
  }
})


test_that("distances are calculated", {
  expect_equal(bng_distance(as_bng_reference("SU14"), 
                            as_bng_reference("SU14"),
                            by_element = TRUE), 0)
  
  expect_equal(bng_distance(as_bng_reference("SU14"), 
                            as_bng_reference("SU14"),
                            by_element = FALSE), as.matrix(0))
  
  df <- readRDS(test_cases("bng_distance"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_equal(bng_distance(as_bng_reference(r$bng_ref1),
                              as_bng_reference(r$bng_ref2),
                              by_element = TRUE),
                 r$expected)
  }
})


test_that("krings are generated", {
  expect_error(bng_kring(as_bng_reference("SU34")))
  expect_error(bng_kring(as_bng_reference("SU12"), k = -1))
  
  df <- readRDS(test_cases("bng_kring"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_equal(sort(bng_kring(as_bng_reference(r$bng_ref), k = r$k)[[1]]),
                 sort(as_bng_reference(r$expected[[1]])))
  }
})


test_that("kdiscs are generated", {
  expect_error(bng_kdisc(as_bng_reference("SU34")))
  expect_error(bng_kdisc(as_bng_reference("SU12"), k = -1))
  
  df <- readRDS(test_cases("bng_kdisc"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_equal(sort(bng_kdisc(as_bng_reference(r$bng_ref), k = r$k)[[1]]),
                 sort(as_bng_reference(r$expected[[1]])))
  }
})


test_that("dwithin references are generated", {
  expect_error(bng_dwithin(as_bng_reference("SU34")))
  expect_error(bng_dwithin(as_bng_reference("SU12"), d = -1))
  
  df <- readRDS(test_cases("bng_dwithin"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_equal(sort(bng_dwithin(as_bng_reference(r$bng_ref), d = r$d)[[1]]),
                 sort(as_bng_reference(r$expected[[1]])))
  }
})
