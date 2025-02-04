
#' Retrieve a hierarchy of BNG indices
#' 
#' Identify the "parent" or "children" references which contain or are nested
#' within a given BNG reference.
#' @param bng_ref object of type \code{BNGReference}
#' @param resolution (optional) numeric value of the target resolution of
#'   parent/child references. If omitted, the next "whole" resolution relative
#'   to the input BNG reference is assumed.
#' @param ... additional parameters. Not currently used
#' @details More details
#' @returns child references will be a list of \code{BNGReference} objects with
#'   each item in the list being the set of children for the input grid
#'   reference. Parent references will be a vector of \code{BNGReference}
#'   objects.
#' @examples
#' # example code
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
  
  if (length(child_list) == 1L) {
    child_list <- child_list[[1]]
  }
  
  child_list
}


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


# helper function to split target grid cell to children
get_children <- function(ref, resolution) {
  bng_res <- internal_get_resolution(ref)
  
  # check on resolution
  if (missing(resolution)) {
    # assume target is "1 less" resolution
    scale <- internal_get_scale(bng_res)
    
    list_res <- list_bng_resolution("all")
    child_idx <- match(scale, list_res) + 1
    child_idx[child_idx > length(list_res)] <- length(list_res)
    
    child_res <- list_res[child_idx]
  } else {
    # user provided resolution
    child_res <- internal_resolution_to_numeric(resolution)
    if (length(child_res) > 1 & length(child_res) != length(ref)) {
      stop("Mismatch found between number of valid references and resolutions.", 
           call. = FALSE)
    }
  }
  args <- expand_args(ref, child_res)
  ref <- as_bng_reference(args[[1]])
  child_res <- args[[2]]
  
  if(any(is.na(child_res))) {
    stop("Invalid resolution detected.", call. = FALSE)
  }
  
  if (any(child_res > bng_res)) {
    stop("Child resolution must be smaller than the BNG reference resolution.", 
         call. = FALSE)
  }
  
  # find list of children for each reference
  child_list <- lapply(seq_along(ref), FUN = function(i){
    # get shape of BNG as bbox
    bb = bng_to_bbox(ref[i])
    # get child refs within the box
    refs = bbox_to_bng(bb[1], bb[2], bb[3], bb[4], child_res[i])
    
    return(refs)
  })
  
  child_list
}


# Helper function to find containing "parent" references
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
    if (length(parent_res) > 1 & length(parent_res) != length(ref)) {
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
