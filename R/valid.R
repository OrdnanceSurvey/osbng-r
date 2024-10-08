
#' Check validity of a BNG Reference
#' 
#' Validates a British National Grid reference string using a regular expression
#' pattern.
#' @param bng_ref Input vector of the BNG reference string(s) to validate.
#' 
#' @details
#' The BNG is structured using a hierarchical system of grid squares at various
#' resolutions. At its highest level, the grid is divided into 100 km by 100 km
#' squares, each of which is identified by a two-letter code. Successive levels
#' of resolution further subdivide the grid squares into finer detail, down to
#' individual 1-meter squares.
#' 
#' Each reference consists of a 2-letter prefix (identifying the 100 km grid
#' square), followed by an easting and northing value, which may be further
#' subdivided using intermediate resolutions. Additionally, an optional suffix
#' representing ordinal (intercardinal) directions (NE, SE, SW, NW) may be
#' appended to the reference to account for quadtree subdivision of the grid at
#' finer resolutions. The grid reference can be expressed at different scales,
#' as follows:
#' 
#'   1. 100 km: Identified by a two-letter code (e.g. 'TQ').
#'   2. 50 km: Subdivides the 100 km grid into four quadrants. The grid reference adds an ordinal direction suffix (NE, NW, SE, SW) to indicate the quadrant within the 100 km square (e.g. 'TQSW').
#'   3. 10 km: Adds two-digit easting and northing values (e.g. 'TQ23').
#'   4. 5 km: Subdivides the 10 km square adding an ordinal suffix (e.g. 'TQ53SW').
#'   5. 1 km: Adds four-digit easting and northing values (e.g. 'TQ2334').
#'   6. 500 m: Subdivides the 1 km square adding an ordinal suffix (e.g. 'TQ2334NE').
#'   7. 100 m: Adds six-digit easting and northing values (e.g. ' TQ238347').
#'   8. 50 m: Subdivides the 100 m square adding an ordinal suffix (e.g. 'TQ238347SE').
#'   9. 10 m: Adds eight-digit easting and northing values (e.g. 'TQ23863472').
#'   10. 5 m: Subdivides the 10 m square adding an ordinal suffix (e.g. e.g. 'TQ23863472NW').
#'   11. 1 m: Adds ten-digit easting and northing values (e.g. 'TQ2386334729').
#' 
#' BNG references must adhere to the following format:
#' 
#' * Whitespace may or may not separate  the components of the reference (i.e. between the two-letter 100km grid square prefix, easting, northing, and ordinal suffix).
#' * If whitespace is present, it should be a single space character.
#' * Whitespace can be inconsistently used between components of the reference.
#' * The two-letter 100 km grid square prefixes and ordinal direction suffixes (NE, SE, SW, NW) should be capitalised.
#' 
#' At each resolution, a given location can be identified with increasing
#' detail, allowing for variable accuracy depending on the geospatial
#' application, from small-scale mapping to precise survey measurements.
#' 
#' @returns A logical vector indicating for each reference of \code{bng_ref}
#'   whether it is valid.
#'   
#' @examples
#' is_valid("TQ1234")  # TRUE
#' 
#' is_valid("TQ123")  # FALSE
#' 
#' @export
#' @name valid
is_valid_bng <- function(bng_ref) UseMethod("is_valid_bng")

#' @export
#' @name valid
is_valid_bng.character <- function(bng_ref) {
  grepl(bng_pattern, bng_ref)
}


#' Validate input
#' 
#' Internal helper function used to verify inputs.
#' @param x object to test
#' @details
#' Primarily called for the side-effect of stopping execution.
#' 
#' @returns \code{TRUE} when the input is a \code{BNGReference} object.
#' @keywords internal
validate_bng_ref <- function(x) {
  if (!is_bng_reference(x)) {
    stop("Please supply a BNG Reference object.", call. = FALSE)
  } 
  
  invisible(x)
}


#' Validate positions
#' 
#' Internal helper function used to verify eastings.
#' @param easting object to test
#' @returns logical vector testing easting position
#' @keywords internal
validate_easting <- function(easting) {
  easting >= 0 & easting <= 700000 & !is.na(easting)
}


#' Validate positions
#' 
#' Internal helper function used to verify northings.
#' @param northing object to test
#' @returns logical vector testing northing position
#' @keywords internal
validate_northing <- function(northing) {
  northing >= 0 & northing <= 1300000 & !is.na(northing)
}
