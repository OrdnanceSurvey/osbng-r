
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
  
  rings <- lapply(which(valid_idx, arr.ind = TRUE), function(i) {
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
  
  discs <- lapply(which(valid_idx, arr.ind = TRUE), function(idx) {
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
#' @param ... additional parameters. Not currently used
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

  neighbours <- lapply(which(valid_idx, arr.ind = TRUE), function(idx) {
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
      return(FALSE)
    } else {
      b2 <- as.character(bng_ref2[i])
      n <- as.character(bng_neighbours(bng_ref1[i]))
      
      return(match(b2, n, nomatch = 0) > 0)
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
#' @param d numeric. Distance expressed in metres.
#' @param ... additional parameters. Not currently used.
#' @returns an unordered vector of \code{BNGReference} objects around a given
#'   grid square within an absolute distance \code{d}.
#' @examples
#' # example code
#' 
#' @export
#' @name bng_distance
bng_dwithin <- function(bng_ref, d, ...) {
  validate_bng_ref(bng_ref)
  
  # set up return
  dlist <- vector("list", length = length(bng_ref))
  valid_idx <- !is.na(bng_ref)
  
  if (missing(d)) {
    stop("Please provide the distance parameter.", call. = FALSE)
  }
  
  if (is.numeric(d) == FALSE || length(d) > 1L) {
    stop("Invalid distance paramter.", call. = FALSE)
  }
  
  if (d <= 0) {
    stop("The distance parameter must be greater than 0.", 
         call. = FALSE)
  }
  
  dneighs <- lapply(which(valid_idx, arr.ind = TRUE), function(idx) {
    ref <- bng_ref[idx]
    resolution <- internal_get_resolution(ref)
    # convert distance to resolution-specific 'k'
    # k <- floor(d / resolution)
    k <- ceiling(d / resolution)
    
    neighs <- get_disc_neighbours(ref, resolution, k, type = "disc")
    return(neighs)
  })
  
  # replace valid
  dlist[valid_idx] <- dneighs
  
  if (length(dlist) == 1L) {
    dlist <- dlist[[1]]
  }
  
  dlist
}


#' @param bng_ref1,bng_ref2 object of \code{BNGReference}
#' @param by_element logical. If \code{TRUE}, return a vector with distance
#'   between each pair of BNG references. An error is raised if the
#'   \code{BNGReference} objects are not the same length. Default is
#'   \code{FALSE}, to return a dense matrix with all pairwise distances.
#' @param edge_to_edge Logical. Should the distances be measured between the
#'   edges of the grid references? Default is \code{FALSE} to use the centroid.
#'
#' @returns If \code{by_element} is \code{FALSE} \code{bng_distance} returns a
#'   dense numeric matrix of dimension length(x) by length(y); otherwise it
#'   returns a numeric vector the same length as \code{x} and \code{y} with an
#'   error raised if the lengths of \code{x} and \code{y} are unequal. Distances
#'   involving invalid references are \code{NA}.
#'   
#' @examples
#' # example code
#' 
#' @rdname bng_distance
#' @export
bng_distance <- function(bng_ref1, 
                         bng_ref2, 
                         by_element = FALSE, 
                         edge_to_edge = FALSE) {
  validate_bng_ref(bng_ref1)
  
  # follow pattern from sf::st_distance
  missing_ref2 <- FALSE
  
  if (missing(bng_ref2)) {
    bng_ref2 <- bng_ref1
    missing_ref2 <- TRUE
  }
  
  validate_bng_ref(bng_ref2)
  
  # get locations
  if (edge_to_edge) {  # edge-based
    ref1 <- bng_to_grid_geom(bng_ref1)
    ref2 <- bng_to_grid_geom(bng_ref2)
  } else {  # centroid-based
    ref1 <- bng_to_xy(bng_ref1, position = "centre")
    ref2 <- bng_to_xy(bng_ref2, position = "centre")
    
    ref1 <- geos::geos_make_point(ref1[, 1], ref1[, 2])
    ref2 <- geos::geos_make_point(ref2[, 1], ref2[, 2])
  }
  
  # check to fill in results with NA
  valid1 <- geos::geos_is_empty(ref1)
  valid2 <- geos::geos_is_empty(ref2)

  # distance calculations
  if (by_element) {
    if (missing_ref2)
      stop("Please provide bng_ref2 when 'by_element' is TRUE.", call. = FALSE)
    
    if (length(ref1) != length(ref2)) 
      stop("BNG references must have the same length", call. = FALSE)
    
    d <- geos::geos_distance(ref1, ref2)
    d[valid1 | valid2] <- NA
  } else {  # distance matrix
    d <- outer(ref1, ref2, FUN = geos::geos_distance)
    d[valid1, valid2] <- NA
  }
  
  d
}


# Helper function to handle internal neighbour generation
#' @keywords internal
#' @noRd
get_disc_neighbours <- function(ref, 
                                resolution, 
                                k, 
                                type = c("disc", "ring", "rook")) {
  type <- match.arg(type)
  
  # calculate relative neighbours
  i <- c(-k:k)
  # establish a disc (including centre reference)
  g <- expand.grid("x" = i, "y" = i)
  
  # subset to a ring
  if (type == "ring") {
    g <- g[g$x %in% c(-k, k) | g$y %in% c(-k, k), ]
  }
  
  if (type == "rook") { # used for neighbours
    g <- g[!(g$x == 0 & g$y == 0), ]
    g <- g[g$x == 0 | g$y == 0, ]
  }
  
  # get starting point
  coords <- bng_to_xy(ref, "centre")
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
