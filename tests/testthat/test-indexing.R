test_that("bboxes convert to BNG", {
  df <- readRDS(test_cases("bbox_to_bng"))
  
  expect_equal(bbox_to_bng(0, 0, 0, 0, "1km"), NA)
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    if(!is.na(r$expected_warning)) {
      expect_warning(val <- bbox_to_bng(r$xmin, r$ymin, r$xmax, r$ymax, 
                                        resolution = r$resolution))
    } else {
      val <- bbox_to_bng(r$xmin, r$ymin, r$xmax, r$ymax, 
                         resolution = r$resolution)
    }
    expect_equal(sort(val), sort(as_bng_reference(r$expected[[1]])))
  }
})


test_that("bng bboxes are returned", {
  df <- readRDS(test_cases("bng_to_bbox"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_equal(bng_to_bbox(as_bng_reference(r$bng_ref)),
                             matrix(r$expected[[1]], ncol = 4))
  }
})


test_that("bng bboxes are returned as geometries", {
  df <- readRDS(test_cases("bng_to_grid_geom"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    expect_true(geos::geos_equals(bng_to_grid_geom(as_bng_reference(r$bng_ref)),
                                 r$expected))
  }
})


test_that("bng references are returned for geometries", {
  df <- readRDS(test_cases("geom_to_bng"))
  
  for (i in seq_len(nrow(df))) {
    print(i)
    r <- df[i, ]
    
    if(!is.na(r$expected_warning)) {
      val <- geom_to_bng(geos::geos_read_geojson(r$geom), 
                         resolution = r$resolution)
      
      expect_equal(sort(val), sort(as_bng_reference(r$expected[[1]]$bng_ref)))
    } else {
      if (r$expected[[1]] == "expect_error") {
        expect_error(geom_to_bng(geos::geos_read_geojson(r$geom), 
                                 resolution = r$resolution))
      } else {
        val <- geom_to_bng(geos::geos_read_geojson(r$geom), 
                           resolution = r$resolution)
        
        expect_equal(sort(val), sort(as_bng_reference(r$expected[[1]]$bng_ref)))
      }
    }
  }
})
