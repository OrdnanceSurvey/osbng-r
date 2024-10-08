
# bbox_to_bng <- function()
  
# bng_to_bbox <- function()

# bng_to_grid_geom <- function() {}

bng_to_xy <- function(bng_ref, position, ...) {
  validate_bng_ref(bng_ref)
  
  position <- match.arg(position, c("lower-left", "upper-left", "upper-right", "lower-right", "centre"))
}

xy_to_bng <- function() {}

# geom_to_bng <- function () {}

# geom_to_bng_intersection <- function() {}


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
