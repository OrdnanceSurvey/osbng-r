
#' BNG reference resolution
#' 
#' Find the spatial resolution (i.e. grid size) of a British National Grid
#' square, or list valid resolutions.
#' @param bng_ref Vector of \code{BNGReference} objects to test.
#' @details
#' The integer values represent spatial resolutions in metres, while the string
#' labels provide a human-readable descriptor for each resolution level. For
#' example, the numeric resolution 1000 is mapped to the label '1km'.
#' 
#' @returns A vector of numeric values for \code{get_bng_resolution()} in metres
#'   or character strings expressing the resolution of the grid references.
#' @examples
#' get_bng_resolution(as_bng_reference("TQ1234"))
#' 
#' get_bng_resolution_string(as_bng_reference("TQ1234NE"))
#' 
#' @export
#' @aliases bng_resolutions
get_bng_resolution <- function(bng_ref) {
  validate_bng_ref(bng_ref)
  res <- internal_get_resolution(bng_ref)
  
  res
}


#' @export
#' @rdname get_bng_resolution
#' @aliases bng_resolutions
get_bng_resolution_string <- function(bng_ref) {
  validate_bng_ref(bng_ref)
  # get the numeric resolution
  res <- get_bng_resolution(bng_ref)
  
  # Look-ups for BNG resolution
  bng_resolution_tbl <- list_bng_resolution("all")
  bng_resolution_lbl <- list_bng_resolution("all", lbl = TRUE)
  
  # form look-ups
  lu <- match(res, bng_resolution_tbl)
  # get string format
  res_str <- bng_resolution_lbl[lu]
  
  res_str
}


#' @keywords internal
#' @noRd
internal_get_resolution <- function(x) {
  x <- gsub(" ", "", as.character(x))
  
  # Look-ups for BNG resolution
  bng_resolution <- list_bng_resolution("whole")
  
  # get eastings/northings
  en <- get_digits(x)
  # get suffix (if present)
  suffix <- get_suffix(x)
  
  # check digits
  l <- nchar(en) / 2
  # get resolution
  res <- bng_resolution[l + 1]
  
  res <- ifelse(suffix != "", res / 2, res)
  
  res
}


#' Convert a string representation of a resolution to numeric
#' @keywords internal
#' @noRd
internal_resolution_to_numeric <- function(x) {
  if (all(is.character(x))) {
    all_res <- list_bng_resolution("all")
    all_lbl <- list_bng_resolution("all", lbl = TRUE)
    
    # double to cover the case of c("5000", "5km")
    res_dup <- c(all_res, all_res)
    
    res <- res_dup[match(x, c(all_lbl, all_res))]
    return(res)
    
  } else if (all(is.numeric(x))) {
    # numeric already
    return(x)
  } 
}


#' Convert resolution to the general scale (i.e. 500m --> 1000)
#' @keywords internal
#' @noRd
internal_get_scale <- function(res) {
  # look-up scale equivalents of resolution
  idx <- findInterval(res + (res + .1), 
                      sort(list_bng_resolution("whole")))
  
  scale <- sort(list_bng_resolution("whole"))[idx]  # i.e. 500m -> 1000
  
  scale
}


#' @param which character indicating what set of resolutions to return.
#' @param lbl logical. Should resolutions labels be returned? Default is
#'   \code{FALSE} to return numeric resolutions.
#' @returns vector of BNG resolutions as either numeric values or character
#'   labels.
#'   
#' @examples
#' list_bng_resolution(which = "all", lbl = TRUE)
#' 
#' @export
#' @rdname get_bng_resolution
#' @aliases bng_resolutions
list_bng_resolution <- function(which = c("all", "whole", "quad"), 
                                lbl = FALSE) {
  which <- match.arg(which)
  
  bng_resolution <- c(100000, 50000, 
                      10000, 5000, 
                      1000, 500, 
                      100, 50, 
                      10, 5, 
                      1)
  
  bng_resolution_lbl <- c("100km", "50km", "10km", "5km", "1km", "500m", 
                          "100m", "50m", "10m", "5m", "1m")
  
  if (which == "all") {
    idx <- seq_along(bng_resolution)
    
  } else {
    if (which == "whole") {
      begin <- 1
    } else if (which == "quad") {
      begin <- 2
    }
    
    idx <- seq(begin, length(bng_resolution), by = 2)
  }
  
  if (lbl == TRUE) {
    out <- bng_resolution_lbl[idx]
  } else {
    out <- bng_resolution[idx]
  }
  
  out
}
