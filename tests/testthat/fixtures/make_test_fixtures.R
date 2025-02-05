# Convert JSON files with tests and expectations to .Rds format
library(jsonlite)

# is_valid_bng
df <- data.frame(
  read_json("./tests/testthat/fixtures/valid_reference_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, "./tests/testthat/fixtures/is_valid_bng_test_cases.rds")

# get_bng_resolution
df <- data.frame(
  read_json("./tests/testthat/fixtures/valid_resolution_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, "./tests/testthat/fixtures/get_bng_resolution_test_cases.rds")

# get_bng_resolution_string
df <- data.frame(
  read_json("./tests/testthat/fixtures/valid_resolution_string_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "expected")
saveRDS(df, "./tests/testthat/fixtures/get_bng_resolution_string_test_cases.rds")

# bng_to_parent
df <- data.frame(
  read_json("./tests/testthat/fixtures/parent_test_cases.json", 
            simplifyVector = TRUE))
names(df) <- c("bng_ref", "resolution", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_to_parent_test_cases.rds")

# bng_to_children
df <- data.frame(
  read_json("./tests/testthat/fixtures/children_test_cases.json", 
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))
names(df) <- c("bng_ref", "resolution", "expected")
saveRDS(df, "./tests/testthat/fixtures/bng_to_children_test_cases.rds")
