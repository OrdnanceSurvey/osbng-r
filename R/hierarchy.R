
#' Navigate the British National Grid hierarchy
#' 
#' Identify the "parent" or "children" references which contain or are nested
#' within a given BNG reference.
#' @param bng_ref object of type \code{BNGReference}
#' @param resolution (optional) value of the target resolution of parent/child
#'   references. If omitted, the next resolution relative to the input BNG
#'   reference is assumed.
#' @param ... additional parameters. Not currently used
#' @details The BNG is structured using a hierarchical system of grid squares at
#'   various resolutions. At its highest level, the grid divides GB into 100 km
#'   by 100 km squares, each identified by a two-letter code. Successive levels
#'   of resolution further subdivide the grid squares into finer detail, down to
#'   individual 1-meter squares. These functions allow for the traversal of this
#'   hierarchy by providing methods to return the parent and children of
#'   BNGReference objects at specified resolutions.
#'   
#'   Definitions:
#'   \itemize{
#'    \item{Parent}{The parent of a BNGReference object is the grid square at
#'    the next higher (coarser) resolution level that contains the current
#'    reference. For example, the parent of a 1km grid square reference would be
#'    the 5km grid square that contains it.}
#'    \item{Children}{The children of a BNGReference object are the grid squares
#'    at the next lower (finer) resolution level that are contained within the
#'    current reference. For example, the children of a 10km grid square
#'    reference would be all the 5km grid squares that it contains.}
#'   }
#'   
#'   
#' @returns child references will be a list of \code{BNGReference} objects with
#'   each item in the list being the set of children for the input grid
#'   reference. Parent references will be a vector of \code{BNGReference}
#'   objects.
#'   
#' @examples
#' bng_to_children(as_bng_reference("SU"))
#' 
#' bng_to_children(as_bng_reference("SU36"))
#' 
#' @rdname bng_to_children
#' @aliases bng_to_parent
#' @export
bng_to_children <- function(bng_ref, resolution, ...) {
  validate_bng_ref(bng_ref)
  
  # set up return
  child_list <- vector("list", length = length(bng_ref))
  valid_idx <- !is.na(bng_ref)
  
  # check if any valid
  if (all(valid_idx == FALSE)) {
    stop("No valid BNG references supplied.", call. = FALSE)
  }
  
  # process references
  child_results <- get_children(bng_ref[valid_idx], resolution)
  
  # fill in results
  child_list[valid_idx] <- child_results
  
  child_list
}


#' @examples
#' bng_to_parent(as_bng_reference("SU36SW"))
#' 
#' bng_to_parent(as_bng_reference("SU342567"))
#' 
#' bng_to_parent(as_bng_reference("SU342567"), resolution = 10000)
#' 
#' @rdname bng_to_children
#' @aliases bng_to_parent
#' @export
bng_to_parent <- function(bng_ref, resolution, ...) {
  validate_bng_ref(bng_ref)
  
  # set up return
  parent_list <- rep(NA, length(bng_ref))
  valid_idx <- !is.na(bng_ref)
  
  # check if any valid
  if (all(valid_idx == FALSE)) {
    stop("No valid BNG references supplied.", call. = FALSE)
  }
  
  # process references
  parent_results <- get_parent(bng_ref[valid_idx], resolution)
  
  # fill in results
  parent_list[valid_idx] <- parent_results
  
  as_bng_reference(parent_list)
}


#' Helper function to split target grid cell to children
#' @param ref vector of objects of type \code{BNGReference}
#' @param resolution target resolution as a string or numeric vector
#' @return list containing multiple \code{BNGRerence} objects which are nested
#'   within the target reference at the given resolution
#' @keywords internal
#' @noRd
get_children <- function(ref, resolution) {
  bng_res <- internal_get_resolution(ref)
  
  # check on resolution
  if (missing(resolution)) {
    # assume target is "1 less" resolution
    list_res <- list_bng_resolution("all")
    child_idx <- match(bng_res, list_res) + 1
    child_idx[child_idx > length(list_res)] <- length(list_res)
    
    child_res <- list_res[child_idx]
  } else {
    # user provided resolution
    child_res <- internal_resolution_to_numeric(resolution)
    if ((length(child_res) > 1) && (length(child_res) != length(ref))) {
      stop("Mismatch found between number of valid references and resolutions.",
           call. = FALSE)
    }
  }
  args <- expand_args(ref, child_res)
  ref <- as_bng_reference(args[[1]])
  child_res <- args[[2]]
  
  if (any(is.na(child_res))) {
    stop("Invalid resolution detected.", call. = FALSE)
  }
  
  if (any(child_res > bng_res)) {
    stop("Child resolution must be smaller than the BNG reference resolution.", 
         call. = FALSE)
  }
  
  # find list of children for each reference
  child_list <- lapply(seq_along(ref), FUN = function(i) {
    # get shape of BNG as bbox
    bb <- bng_to_bbox(ref[i])
    # get child refs within the box
    refs <- bbox_to_bng(bb[1], bb[2], bb[3], bb[4], child_res[i])[[1]]
    
    return(refs)
  })
  
  child_list
}


#' Helper function to find containing "parent" references
#' @param ref vector of objects of type \code{BNGReference}
#' @param resolution target resolution as a string or numeric vector
#' @return list containing multiple \code{BNGRerence} objects which are nested
#'   within the target reference at the given resolution
#' @keywords internal
#' @noRd
get_parent <- function(ref, resolution) {
  bng_res <- internal_get_resolution(ref)
  
  # check on resolution
  if (missing(resolution)) {
    # assume target is "1 larger" resolution
    list_res <- list_bng_resolution("all")
    parent_idx <- match(bng_res, list_res) - 1
    parent_idx[parent_idx < 1] <- 1
    
    parent_res <- list_res[parent_idx]
  } else {
    # user provided resolution
    parent_res <- internal_resolution_to_numeric(resolution)
    if ((length(parent_res) > 1) && (length(parent_res) != length(ref))) {
      stop("Mismatch found between number of valid references and resolutions.",
           call. = FALSE)
    }
  }
  
  args <- expand_args(ref, parent_res)
  ref <- as_bng_reference(args[[1]])
  parent_res <- args[[2]]
  
  if (any(is.na(parent_res))) {
    stop("Invalid resolution detected.", call. = FALSE)
  }
  
  if (any(bng_res > parent_res)) {
    stop("Parent resolution must be larger than the BNG reference resolution.", 
         call. = FALSE)
  }
  
  # find the parent for each reference
  coords <- bng_to_xy(ref)
  parents <- xy_to_bng(coords, parent_res)
  
  return(parents)
}
