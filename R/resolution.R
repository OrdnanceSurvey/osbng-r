
#' BNG reference resolution
#' 
#' Find the spatial resolution (i.e. grid size) of a British National Grid
#' square.
#' @param bng_ref Vector of \code{BNGReference} objects to test.
#' @details
#' How the resolution of grid reference is determined.
#' 
#' @returns A vector of numeric values for \code{get_bng_resolution()} in metres
#'   or character strings expressing the resolution of the grid references.
#' @examples
#' get_bng_resolution("TQ1234")
#' 
#' get_bng_resolution_string("TQ1234NE")
#' 
#' @export
get_bng_resolution <- function(bng_ref) {
  validate_bng_ref(bng_ref)
  res <- internal_get_resolution(bng_ref)
  
  res
}


#' @export
#' @rdname get_bng_resolution
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
    
    res <- all_res[match(x, all_lbl)]
    return(res)
    
  } else if(all(is.numeric(x))) {
    # numeric already
    return(x)
  } else {
    stop("Resolution values must be all strings or integers.", call. = FALSE)
  }
}


#' List valid BNG resolutions
#' 
#' Internal helper function to provide vector of resolutions or labels.
#' @param which character indicating what set of resolutions to return
#' @param lbl logical. Should resolutions labels be returned? Default is
#'   \code{FALSE} to return numeric resolutions.
#' @returns vector of BNG resolutions as either numeric values or character
#'   labels.
#' @keywords internal
#' @noRd
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
    idx <- 1:length(bng_resolution)
    
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
