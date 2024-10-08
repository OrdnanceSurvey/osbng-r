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


# Set of valid BNG prefixes. Arrange in a 7x13 array for geographic positioning
bng_prefixes <- c("SV", "SW", "SX", "SY", "SZ", "TV", "TW", "SQ", "SR", "SS",
                  "ST", "SU", "TQ", "TR", "SL", "SM", "SN", "SO", "SP", "TL", 
                  "TM", "SF", "SG", "SH", "SJ", "SK", "TF", "TG", "SA", "SB", 
                  "SC", "SD", "SE", "TA", "TB", "NV", "NW", "NX", "NY", "NZ", 
                  "OV", "OW", "NQ", "NR", "NS", "NT", "NU", "OQ", "OR", "NL", 
                  "NM", "NN", "NO", "NP", "OL", "OM", "NF", "NG", "NH", "NJ", 
                  "NK", "OF", "OG", "NA", "NB", "NC", "ND", "NE", "OA", "OB", 
                  "HV", "HW", "HX", "HY", "HZ", "JV", "JW", "HQ", "HR", "HS", 
                  "HT", "HU", "JQ", "JR", "HL", "HM", "HN", "HO", "HP", "JL", 
                  "JM")

# Set of valid BNG suffixes for quadrant positions
bng_suffixes <- c("SW", "SE", "NW", "NE")


# Store .rda file for the package.
usethis::use_data(bng_pattern,
                  bng_prefixes,
                  bng_suffixes,
                  internal = TRUE, overwrite = TRUE)
