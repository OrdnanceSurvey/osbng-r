# code to prepare internal datasets

# Test for valid BNG patterns
# constrained to GB-specific bounding area
# spaces allowed, all are optional
# limited to >= 1m resolution
bng_pattern <- paste0("^(H[LMNOPQRSTUVWXYZ]|",
                      "N[ABCDEFGHJKLMNOPQRSTUVWXYZ]|",
                      "O[ABFGLMQRVW]|",
                      "S[ABCDEFGHJKLMNOPQRSTUVWXYZ]|",
                      "T[ABFGLMQRVW]|",
                      "J[LMQRVW])\\s?",
                      "(?:(\\d{2}|\\d{4}|\\d{6}|\\d{8}|",
                      "\\d{1}\\s\\d{1}|\\d{2}\\s\\d{2}|",
                      "\\d{3}\\s\\d{3}|\\d{4}\\s\\d{4}|",
                      "\\d{10}$|\\d{5}\\s\\d{5}$))",
                      "\\s?(NE|SE|SW|NW)?$")


# Store .rda file for the package.
usethis::use_data(bng_pattern,
                  internal = TRUE, overwrite = TRUE)
