
# bbox_to_bng <- function()
  
# bng_to_bbox <- function()

# bng_to_grid_geom <- function() {}

bng_to_xy <- function(bng_ref, position, ...) {
  validate_bng_ref(bng_ref)
  
  position <- match.arg(position, c("lower-left", "upper-left", "upper-right", "lower-right", "centre"))
}

#' @export
xy_to_bng <- function(...) UseMethod("xy_to_bng")

#' @export
xy_to_bng.numeric <- function(easting, northing, resolution, ...) {
  # check inputs
  if (missing(resolution)) {
    stop("Please provide a target grid reference resolution.", call. = FALSE)
  }
  
  if (length(easting) != length(northing)) {
    stop("Lengths of supplied eastings and northings must match.",
         call. = FALSE)
  }
  
  chk_easting <- validate_easting(easting)
  chk_northing <- validate_northing(northing)
  
  if (any(chk_easting == FALSE) |
      any(chk_northing == FALSE)) {
    warning("Invalid coordinates detected.", call. = FALSE)
  }
  
  # allow vectors of resolutions
  args <- expand_args(easting, northing, resolution)
  
  easting <- args[[1]]
  northing <- args[[2]]
  resolution <- args[[3]]
  
  # convert resolution to numeric values
  if (is.character(resolution)) {
    resolution <- list_bng_resolution("all")[match(resolution, 
                                                   list_bng_resolution("all", 
                                                                       lbl = TRUE))]
  }
  
  # check resolution
  chk_resolution <- is_valid_bng_resolution(resolution)
  
  if (all(chk_resolution == FALSE)) {
    stop("No valid resolutions detected.", call. = FALSE)
  } else if (any(chk_resolution == FALSE)) {
    warning("Invalid resolution detected. NA returned.", call. = FALSE)
  }
  
  if (length(unique(resolution)) > 1) {
    warning("Varying resolutions detected.", call. = FALSE)
  }
  
  # set-up storage for results
  grid_refs <- rep(NA, length(resolution))
  
  # drop invalid inputs
  valid_idx <- chk_easting & chk_northing & chk_resolution
  
  easting <- easting[valid_idx]
  northing <- northing[valid_idx]
  resolution <- resolution[valid_idx]
  
  if (length(easting) == 0) {
    stop("No valid inputs found.", call. = FALSE)
  }
  
  # create from coordinates
  res <- bng_from_coords(easting, northing, resolution)
  
  # expand results
  grid_refs[valid_idx] <- res
  
  new_bng_reference(grid_refs)
}

# geom_to_bng <- function () {}

# geom_to_bng_intersection <- function() {}


#' Convert eastings and northings to BNG grid reference
#' 
#' Internal helper function to create BNG reference strings.
#' @param easting numeric vector of coordinates
#' @param northing numeric vector of coordinates
#' @param resolution numeric vector of resolutions in meters
#' @returns character vector of British National Grid references.
#' @keywords internal
bng_from_coords <- function(easting, northing, resolution) {
  # look-up scale equivalents of resolution
  idx <- findInterval(resolution + (resolution + .1), 
                      sort(list_bng_resolution("whole")))
  
  scale <- sort(list_bng_resolution("whole"))[idx]  # i.e. 500m -> 1000
  digits <- nchar(resolution) - 1
  
  # set-up padding
  digits <- paste0("%0", 5 - digits, "d")
  
  # is suffix required for the resolution?
  quads <- resolution %in% list_bng_resolution("quad")
  
  # part 1: convert grid index prefixes
  pe <- trunc(easting / 100000)
  pn <- trunc(northing / 100000)
  
  # look up the letters
  prefix <- bng_prefixes[pe + pn * 7 + 1]
  
  # part 2: convert remaining coordinates
  x <- trunc(easting %% 100000 / scale)
  y <- trunc(northing %% 100000 / scale)
  
  # pad string with leading zeroes
  x <- sprintf(digits, x)
  y <- sprintf(digits, y)
  
  # part 3: find suffixes
  sx <- trunc(easting %% 100000 %% scale / resolution)
  sy <- trunc(northing %% 100000 %% scale / resolution)
  
  # look up suffix
  suffix <- bng_suffixes[sx + sy * 2 + 1]
  suffix[!quads] <- ""
  
  # construct final format
  grid_ref <- apply(cbind(prefix, x, y, suffix), 1, 
                    function(i) {
                      paste(i[!is.na(i) & i != ""], collapse = " ")
                    })
  
  grid_ref
}


#' Convert BNG reference to eastings and northings
#' 
#' Internal helper function to create coordinate positions.
#' @param ref BNG reference as a character vector
#' @param position character indicating which grid position to return
#' @details
#' This internal function does not do extensive error checking/handling. The
#' calling functions need to handle inserting NAs for invalid references.
#' 
#' @returns matrix with two columns of easting and northings, respectively.
#' @keywords internal
bng_to_coords <- function(ref, position) {
  ref <- gsub(" ", "", ref)
  
  # extract prefix letters
  prefix <- get_prefix(ref)
  
  idx <- match(prefix, bng_prefixes)
  idx <- arrayInd(idx, .dim = c(7, 13)) - 1
  
  # look up the main coordinates based on prefix
  e <- idx[1, 1] * 100000
  n <- idx[1, 2] * 100000
  
  # extract coordinates (if present)
  xy <- get_digits(ref)
  
  # find the resolution
  l <- nchar(xy) / 2
  res <- 10^(5 - l)
  
  # convert coordinates to numbers at resolution
  if (any(l > 0)) {
    x <- as.numeric(substr(xy, 1, l)) * res
    y <- as.numeric(substr(xy, l + 1, nchar(xy))) * res
    
    x[is.na(x)] <- 0
    y[is.na(y)] <- 0
  } else {
    x <- 0
    y <- 0
  }
  
  # extract suffixes (if present)
  suffix <- get_suffix(ref)
  suffix <- match(suffix, bng_suffixes) - 1
  suffix[is.na(suffix)] <- 0
  
  if (any(suffix > 0)) {
    # update resolution for suffix
    res[which(suffix > 0, arr.ind = T)] <- 
      res[which(suffix > 0, arr.ind = T)] / 2
    
    # look-up suffix adjustments
    sx <- trunc(suffix %% 2)
    sy <- trunc(suffix / 2)
    
    # "zero-out" some moves
    sx <- sx * res
    sy <- sy * res
    
    sx[is.na(sx)] <- 0
    sy[is.na(sy)] <- 0
  } else {
    sx <- 0
    sy <- 0
  }
  
  # lower-left positions
  e <- e + x + sx
  n <- n + y + sy
  
  switch(position,
         "lower-right" = {e <- e + res},
         "upper-left"  = {n <- n + res},
         "upper-right" = {e <- e + res
                          n <- n + res
                         },
         "centre"      = {e <- e + res / 2
                          n <- n + res / 2
                         }
        )
  
  matrix(c(e, n), ncol = 2)
}
