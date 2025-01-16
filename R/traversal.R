
#' Traverse BNG references
#' 
#' Identify neighbours in a hollow ring or solid disc at grid distance 'k' from
#' a target BNG reference.
#' @param bng_ref an object of type \code{BNGReference}
#' @param k numeric value measuring the number of grid squares traversed between
#'   the ring and input BNG reference
#' @details
#' Additional details...
#' @returns an unordered collection of objects of type \code{BNGReference}.
#' 
#' @examples
#' # example code
#' 
#' @rdname bng_kring
#' @aliases bng_kdisc
#' @export
bng_kring <- function(bng_ref, k) {
  validate_bng_ref(bng_ref)
  
  # set up return
  ring_list <- vector("list", length = length(bng_ref))
  valid_idx <- !is.na(bng_ref)
  
  if (missing(k)) {
    stop("Please provide grid distance parameter 'k'", call. = FALSE)
  }
  
  if (length(k) > 1L) {
    stop("Please provide a single value of 'k'.", call. = FALSE)
  }
  
  if (is.numeric(k) == FALSE) {
    stop("Invalid parameter 'k' supplied.", call. = FALSE)
  }
  
  stopifnot(k >= 1)
  
  rings <- lapply(seq_along(bng_ref), function(i) {
    ref <- bng_ref[i]
    resolution <- internal_get_resolution(ref)
    
    refs <- get_disc_neighbours(ref, resolution, k, TRUE)
    
    return(refs)
  })
  
  # replace valid
  ring_list[valid_idx] <- rings
  
  ring_list
}


#' @rdname bng_kring
#' @aliases bng_kdisc
#' @export
bng_kdisc <- function(bng_ref, k) {
  validate_bng_ref(bng_ref)
  
  # set up return
  disc_list <- vector("list", length = length(bng_ref))
  valid_idx <- !is.na(bng_ref)
  
  if (missing(k)) {
    stop("Please provide grid distance parameter 'k'", call. = FALSE)
  }
  
  if (length(k) > 1L) {
    stop("Please provide a single value of 'k'.", call. = FALSE)
  }
  
  if (is.numeric(k) == FALSE) {
    stop("Invalid parameter 'k' supplied.", call. = FALSE)
  }
  
  stopifnot(k >= 1)
  
  discs <- lapply(seq_along(bng_ref), function(idx) {
    ref <- bng_ref[idx]
    resolution <- internal_get_resolution(ref)
    
    neighs <- get_disc_neighbours(ref, resolution, k, FALSE)
    return(neighs)
  })
  
  # replace valid
  disc_list[valid_idx] <- discs
  
  disc_list
}


bng_is_neighbour <- function(bng_ref1, bng_ref2) {
  
}


bng_neighbours <- function(bng_ref) {
  
}

# Helper function to handle internal neighbour generation
#' @keywords internal
#' @noRd
get_disc_neighbours <- function(ref, resolution, k, ring = FALSE) {
  # calculate relative neighbours
  i <- c(-k:k)
  g <- expand.grid("x" = i, "y" = i)
  g <- g[!(g$x == 0 & g$y == 0), ]
  
  # subset to a ring
  if (ring == TRUE) {
    g <- g[g$x %in% c(-k, k) | g$y %in% c(-k, k), ]
  }
  
  # get starting point
  coords <- bng_to_xy(ref, 'centre')
  easting <- coords[1]
  northing <- coords[2]
  
  # validate positions
  e <- easting + g$x * resolution
  n <- northing + g$y * resolution
  
  valid_e <- validate_easting(e)
  valid_n <- validate_northing(n)
  valid_all <- valid_e & valid_n
  
  if (all(valid_all == FALSE)) {
    return(NA)
  }
  
  refs <- xy_to_bng(e[valid_all], n[valid_all], resolution)
  
  refs
}

