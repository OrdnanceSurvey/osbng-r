test_that("bboxes convert to BNG", {
  df <- readRDS(test_cases("bbox_to_bng"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    if (!is.na(r$expected_warning)) {
      expect_warning(val <- bbox_to_bng(r$xmin, r$ymin, r$xmax, r$ymax, 
                                        resolution = r$resolution)[[1]])
    } else {
      val <- bbox_to_bng(r$xmin, r$ymin, r$xmax, r$ymax, 
                         resolution = r$resolution)[[1]]
    }
    expect_equal(sort(val), sort(as_bng_reference(r$expected[[1]])))
  }
  
  # input as a matrix
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    bbox <- matrix(c(r$xmin, r$ymin, r$xmax, r$ymax), ncol=4)
    
    if (!is.na(r$expected_warning)) {
      expect_warning(val <- bbox_to_bng(bbox, 
                                        resolution = r$resolution)[[1]])
    } else {
      val <- bbox_to_bng(bbox, 
                         resolution = r$resolution)[[1]]
    }
    expect_equal(sort(val), sort(as_bng_reference(r$expected[[1]])))
  }
  
  # input as a data.frame
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    bbox <- data.frame(t(c(r$xmin, r$ymin, r$xmax, r$ymax)))
    
    if (!is.na(r$expected_warning)) {
      expect_warning(val <- bbox_to_bng(bbox, 
                                        resolution = r$resolution)[[1]])
    } else {
      val <- bbox_to_bng(bbox, 
                         resolution = r$resolution)[[1]]
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
    r <- df[i, ]
    
    if (!is.na(r$expected_warning)) {
      expect_warning(val <- geom_to_bng(geos::geos_read_geojson(r$geom), 
                                        resolution = r$resolution))
      
      expect_equal(sort(val[[1]]), 
                   sort(as_bng_reference(r$expected[[1]]$bng_ref)))
    } else {
      if (r$expected[[1]] == "expect_error") {
        expect_error(geom_to_bng(geos::geos_read_geojson(r$geom), 
                                 resolution = r$resolution))
      } else {
        val <- geom_to_bng(geos::geos_read_geojson(r$geom), 
                           resolution = r$resolution)
        
        expect_equal(sort(val[[1]]), 
                     sort(as_bng_reference(r$expected[[1]]$bng_ref)))
      }
    }
  }
})


test_that("geometries are decomposed to bng references", {
  df <- readRDS(test_cases("geom_to_bng_intersection"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    if (!is.na(r$expected_warning)) {
      expect_warning(val <- geom_to_bng_intersection(geos::geos_read_geojson(r$geom),
                                                     resolution = r$resolution))
      
      expect_equal(length(val[[1]]), 3L)
      expect_equal(names(val[[1]]), c("BNGReference", "is_core", "geom"))
      expect_equal(length(val), length(r$expected))
      
      expect_equal(sort(val[[1]]$BNGReference), 
                   sort(as_bng_reference(r$expected[[1]]$bng_ref)))
      
      expect_equal(val[[1]]$is_core[order(val[[1]]$BNGReference)],
                   r$expected[[1]]$is_core[order(r$expected[[1]]$bng_ref)])
      
    } else {
      if (any(r$expected[[1]] == "expect_error")) {
        expect_error(geom_to_bng_intersection(geos::geos_read_geojson(r$geom), 
                                              resolution = r$resolution))
      } else {
        val <- geom_to_bng_intersection(geos::geos_read_geojson(r$geom),
                                        resolution = r$resolution)
        
        expect_equal(length(val[[1]]), 3L)
        expect_equal(names(val[[1]]), c("BNGReference", "is_core", "geom"))
        expect_equal(length(val), length(r$expected))
        
        expect_equal(sort(val[[1]]$BNGReference), 
                     sort(as_bng_reference(r$expected[[1]]$bng_ref)))
        
        expect_equal(val[[1]]$is_core[order(val[[1]]$BNGReference)],
                     r$expected[[1]]$is_core[order(r$expected[[1]]$bng_ref)])
      }
    }
  }
}) 


test_that("geometries are decomposed in a spatial data frame", {
  skip_if_not_installed("sf")
  
  df <- readRDS(test_cases("geom_to_bng_intersection"))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    
    if (!is.na(r$expected_warning)) {
      expect_warning(val <- geom_to_bng_intersection_explode(geos::geos_read_geojson(r$geom),
                                                             resolution = r$resolution))
      
      expect_s3_class(val, "sf")
      expect_equal(ncol(val), 3L)
      expect_equal(names(val), c("bng_reference", "is_core", "geometry"))
      expect_equal(nrow(val), length(r$expected[[1]]$bng_ref))
      
      expect_equal(sort(val$bng_ref), 
                   sort(as_bng_reference(r$expected[[1]]$bng_ref)))
      
      expect_equal(val$is_core[order(val$bng_reference)],
                   r$expected[[1]]$is_core[order(r$expected[[1]]$bng_ref)])
      
    } else {
      if (any(r$expected[[1]] == "expect_error")) {
        expect_error(geom_to_bng_intersection(geos::geos_read_geojson(r$geom), 
                                              resolution = r$resolution))
      } else {
        val <- geom_to_bng_intersection_explode(geos::geos_read_geojson(r$geom),
                                                resolution = r$resolution)
        
        expect_s3_class(val, "sf")
        expect_equal(ncol(val), 3L)
        expect_equal(names(val), c("bng_reference", "is_core", "geometry"))
        expect_equal(nrow(val), length(r$expected[[1]]$bng_ref))
        
        expect_equal(sort(val$bng_reference), 
                     sort(as_bng_reference(r$expected[[1]]$bng_ref)))
        
        expect_equal(val$is_core[order(val$bng_reference)],
                     r$expected[[1]]$is_core[order(r$expected[[1]]$bng_ref)])
      }
    }
  }
})
