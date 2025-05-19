# Convert JSON files with tests and expectations to .Rds format
library(jsonlite)

fixture_path <- "./tests/testthat/fixtures"

# is_valid_bng
df <- data.frame(
  read_json(file.path(fixture_path, "json/valid_reference_test_cases.json"), 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, file.path(fixture_path, "is_valid_bng_test_cases.rds"))

# get_bng_resolution
df <- data.frame(
  read_json(file.path(fixture_path, "json/valid_resolution_test_cases.json"), 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, file.path(fixture_path, "get_bng_resolution_test_cases.rds"))

# get_bng_resolution_string
df <- data.frame(
  read_json(file.path(fixture_path, 
                      "json/valid_resolution_string_test_cases.json"), 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, file.path(fixture_path, "get_bng_resolution_string_test_cases.rds"))

# bng_to_parent
df <- data.frame(
  read_json(file.path(fixture_path, "json/parent_test_cases.json"), 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "resolution", "expected")
saveRDS(df, file.path(fixture_path, "bng_to_parent_test_cases.rds"))

# bng_to_children
df <- data.frame(
  read_json(file.path(fixture_path, "json/children_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "resolution", "expected")
saveRDS(df, file.path(fixture_path, "bng_to_children_test_cases.rds"))

# xy_to_bng
df <- data.frame(
  read_json(file.path(fixture_path, "json/xy_to_bng_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("easting", "northing", "resolution", "expected")
saveRDS(df, file.path(fixture_path, "xy_to_bng_test_cases.rds"))

# bng_to_xy
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_to_xy_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "position", "expected")
saveRDS(df, file.path(fixture_path, "bng_to_xy_test_cases.rds"))

# bng_is_neighbour
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_is_neighbour_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref1", "bng_ref2", "expected")
saveRDS(df, file.path(fixture_path, "bng_is_neighbour_test_cases.rds"))

# bng_distance
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_distance_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref1", "bng_ref2", "expected")
saveRDS(df, file.path(fixture_path, "bng_distance_test_cases.rds"))

# bng_kring
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_kring_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "k", "expected")
saveRDS(df, file.path(fixture_path, "bng_kring_test_cases.rds"))

# bng_kdisc
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_kdisc_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "k", "expected")
saveRDS(df, file.path(fixture_path, "bng_kdisc_test_cases.rds"))

# bng_dwithin
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_dwithin_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "d", "expected")
saveRDS(df, file.path(fixture_path, "bng_dwithin_test_cases.rds"))

# bbox_to_bng
df <- data.frame(
  read_json(file.path(fixture_path, "json/bbox_to_bng_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("xmin", "ymin", "xmax", "ymax", 
               "resolution", "expected_warning", "expected")
saveRDS(df, file.path(fixture_path, "bbox_to_bng_test_cases.rds"))

# bng_to_bbox
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_to_bbox_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, file.path(fixture_path, "bng_to_bbox_test_cases.rds"))

# bng_to_grid_geom
df <- data.frame(
  read_json(file.path(fixture_path, "json/bng_bbox_geom_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = FALSE))
names(df) <- c("bng_ref", "output")

df$expected <- sapply(seq_along(df$bng_ref), function(i) {
  x <- df[i, "output", drop = TRUE]
  
  if (x$type == "Polygon") {
    g <- geos::geos_make_polygon(x$coordinates[[1]][, , 1], 
                                 x$coordinates[[1]][, , 2])
    
    return(geos::geos_write_wkt(g))
  }
})

saveRDS(df[, c("bng_ref", "expected")], 
        file.path(fixture_path, "bng_to_grid_geom_test_cases.rds"))

# geom_to_bng
df <- data.frame(
  read_json(file.path(fixture_path, "json/geom_to_bng_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("geom", "resolution", "expected", "expected_warning")
saveRDS(df, file.path(fixture_path, "geom_to_bng_test_cases.rds"))

# geom_to_bng_intersection
df <- data.frame(
  read_json(file.path(fixture_path, 
                      "json/geom_to_bng_intersection_test_cases.json"), 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("geom", "resolution", "expected", "expected_warning")
saveRDS(df, file.path(fixture_path, "geom_to_bng_intersection_test_cases.rds"))
