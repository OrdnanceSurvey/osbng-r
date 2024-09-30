# Locate the appropriate file of test cases
test_cases <- function(fname){
  testthat::test_path("fixtures", paste0(fname, "_test_cases.rds"))
}  
