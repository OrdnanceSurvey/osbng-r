# Convert JSON files with tests and expectations to .Rds format
library(jsonlite)

# is_valid_bng
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/valid_reference_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, "./tests/testthat/fixtures/is_valid_bng_test_cases.rds")

# get_bng_resolution
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/valid_resolution_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, "./tests/testthat/fixtures/get_bng_resolution_test_cases.rds")

# get_bng_resolution_string
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/valid_resolution_string_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, "./tests/testthat/fixtures/get_bng_resolution_string_test_cases.rds")

# bng_to_parent
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/parent_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "resolution", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_to_parent_test_cases.rds")

# bng_to_children
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/children_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "resolution", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_to_children_test_cases.rds")

# xy_to_bng
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/xy_to_bng_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("easting", "northing", "resolution", "expected")
saveRDS(df, "./tests/testthat/fixtures/xy_to_bng_test_cases.rds")

# bng_to_xy
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/bng_to_xy_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "position", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_to_xy_test_cases.rds")

# bng_is_neighbour
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/bng_is_neighbour_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref1", "bng_ref2", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_is_neighbour_test_cases.rds")

# bng_distance
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/bng_distance_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref1", "bng_ref2", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_distance_test_cases.rds")

# bng_kring
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/bng_kring_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "k", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_kring_test_cases.rds")

# bng_kdisc
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/bng_kdisc_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "k", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_kdisc_test_cases.rds")

# bng_dwithin
df <- data.frame(
  read_json("./tests/testthat/fixtures/json/bng_dwithin_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "d", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_dwithin_test_cases.rds")
