
#' Spatial neighbourhoods
#' 
#' Identify neighbours in a hollow ring or solid disc at grid distance 'k' from
#' a target BNG reference.
#' @param bng_ref an object of type \code{BNGReference}
#' @param k numeric value measuring the number of grid squares traversed between
#'   the ring and input BNG reference
#' @param ... additional parameters. Not currently used
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
bng_kring <- function(bng_ref, k, ...) {
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
    
    refs <- get_disc_neighbours(ref, resolution, k, type = "ring")
    
    return(refs)
  })
  
  # replace valid
  ring_list[valid_idx] <- rings
  
  if (length(ring_list) == 1L) {
    ring_list <- ring_list[[1]]
  }
  
  ring_list
}


#' @rdname bng_kring
#' @aliases bng_kdisc
#' @export
bng_kdisc <- function(bng_ref, k, ...) {
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
    
    neighs <- get_disc_neighbours(ref, resolution, k, type = "disc")
    return(neighs)
  })
  
  # replace valid
  disc_list[valid_idx] <- discs
  
  if (length(disc_list) == 1L) {
    disc_list <- disc_list[[1]]
  }
  
  disc_list
}


#' Identify neighbours
#' 
#' Find BNG references which share a grid cell edge with a target BNG reference.
#' @param bng_ref target object of type \code{BNGReference}
#' @param ... additional parameters. Not currenlty used
#' @details Grid reference cells are "neighbours" when they share a contiguous
#' edge (i.e. corners do not define neighbours). In the event that a target
#' reference is along the edge or corner of the valid BNG area, then 3 or 2
#' references, respectively, will be returned \code{bng_is_neighbour} only
#' compares references of equal resolution.
#' @returns A set of up to four \code{BNGReference} objects that border the
#'   target reference.
#' @examples
#' # example code
#' 
#' @name bng_neighbours
#' @export
bng_neighbours <- function(bng_ref, ...) {
  validate_bng_ref(bng_ref)
  
  # set up return
  neigh_list <- vector("list", length = length(bng_ref))
  valid_idx <- !is.na(bng_ref)
  
  if (all(valid_idx == FALSE)) {
    stop("Please provide a valid BNG reference.", call. = FALSE)
  }

  neighbours <- lapply(seq_along(bng_ref), function(idx) {
    ref <- bng_ref[idx]
    resolution <- internal_get_resolution(ref)
    
    neighs <- get_disc_neighbours(ref, resolution, k = 1, type = "rook")
    return(neighs)
  })
  
  # replace valid
  neigh_list[valid_idx] <- neighbours
  
  if (length(neigh_list) == 1L) {
    neigh_list <- neigh_list[[1]]
  }
  
  neigh_list
}


#' @param bng_ref1,bng_ref2 \code{BNGReference} object for comparison when
#'   assessing neighbour relationships.
#' @returns a boolean identifying if the grid references share a border
#' @aliases bng_neighbours
#' @export
#' @rdname bng_neighbours
bng_is_neighbour <- function(bng_ref1, bng_ref2, ...) {
  validate_bng_ref(bng_ref1)
  validate_bng_ref(bng_ref2)
  
  if (length(bng_ref1) != length(bng_ref2)) {
    args <- expand_args(bng_ref1, bng_ref2)
    bng_ref1 <- as_bng_reference(args[[1]])
    bng_ref2 <- as_bng_reference(args[[2]])
  }
  
  # set up return
  neigh_list <- rep(NA, length(bng_ref1))
  valid_idx <- !is.na(bng_ref1) & !is.na(bng_ref2)
  
  if (all(valid_idx == FALSE)) {
    stop("Please provide a valid BNG reference.", call. = FALSE)
  }
  
  # only process valid refs
  bng_ref1 <- bng_ref1[valid_idx]
  bng_ref2 <- bng_ref2[valid_idx]
  
  res1 <- get_bng_resolution(bng_ref1)
  res2 <- get_bng_resolution(bng_ref2)
  
  if (any(res1 != res2, na.rm = TRUE)) {
    stop("Resolutions must match to test neighbours.", call. = FALSE)
  }
  
  neighs <- sapply(seq_along(bng_ref1), function(i) {
    if (bng_ref2[i] == bng_ref1[i]) {
      return (TRUE)
    } else {
      b2 <- as.character(bng_ref2[i])
      n <- as.character(bng_neighbours(bng_ref1[i]))
      
      return (match(b2, n) > 0)
    }
  })
  
  neigh_list[valid_idx] <- neighs
  
  neigh_list
}


#' Distance calculations
#' 
#' Compute Euclidean distances between BNG references and distance-based
#' neighbours lists.
#' @param bng_ref object of class \code{BNGReference}.
#' @param d distance expressed in metres.
#' @param ... additional parameters. Not currently used.
#' @returns an unordered vector of \code{BNGReference} objects around a given
#'   grid square within an absolute distance \code{d}.
#' @examples
#' # example code
#' @export
#' @name bng_distance
bng_dwithin <- function(bng_ref, d, ...) {
  
}


#' @param bng_ref1,bng_ref2 object of \code{BNGReference}
#' @param by_element logical. If \code{TRUE}, return a vector with distance
#'   between each BNG reference. An error is raised if the \code{BNGReference}
#'   objects are not the same length. If \code{FALSE}, return a dense matrix
#'   with all pairwise distances.
#' @rdname bng_distance
#' @export
bng_distance <- function(bng_ref1, bng_ref2, by_element = FALSE) {
  
}


# Helper function to handle internal neighbour generation
#' @keywords internal
#' @noRd
get_disc_neighbours <- function(ref, 
                                resolution, 
                                k, 
                                type = c("disc", "ring", "rook")) {
  type = match.arg(type)
  
  # calculate relative neighbours
  i <- c(-k:k)
  g <- expand.grid("x" = i, "y" = i)
  
  # subset to a ring
  if (type == "ring") {
    g <- g[g$x %in% c(-k, k) | g$y %in% c(-k, k), ]
  }
  
  if (type == "rook") {
    g <- g[!(g$x == 0 & g$y == 0), ]
    g <- g[g$x == 0 | g$y == 0, ]
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

