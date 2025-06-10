#' Map BNG References to Spatial Objects
#' 
#' Generate a spatial data frame of BNG references and grid square geometries.
#' @param xmin,ymin,xmax,ymax Optional bounding box coordinates.
#' @param ... Additional arguments. Not currently used.
#' @details
#' These convenience functions generate a spatial data frame of BNG references
#' and grid square geometries at pre-determined resolutions. This function
#' combined \code{bbox_to_bng()} and \code{bng_to_grid_geom()} into a data
#' frame. Optionally, the grid can be for a defined bounding box area of
#' interest. If the bounding box is omitted, then all grid squares within the
#' valid bounds of the BNG are returned.
#' 
#' Only selected resolutions are provided to avoid excessively large data
#' frames. For additional spatial data files of all resolutions, please see the
#' [\code{osbng-grids} GitHub
#' repo](https://github.com/OrdnanceSurvey/osbng-grids/).
#' 
#' The \code{sf} packge is required for this function.
#' @returns Data frame object of type \code{sf} with the grid reference and the
#'   grid square geometry.
#'   
#' @examplesIf require("sf")
#' bng_grid_100km()
#' 
#' bng_grid_1km(529476, 179654, 532170, 181116)
#' 
#' @seealso [bbox_to_bng()]
#' @export
#' @rdname bng_grid
bng_grid_100km <- function(xmin, ymin, xmax, ymax, ...) {
  
  # Default to the full GB boundary
  if (any(missing(xmin), missing(ymin), missing(xmax), missing(ymax))) {
    bbox <- bng_bounds
  } else {
    bbox <- c(xmin, ymin, xmax, ymax)
  }
  
  bbox_bng_grid(c(bbox[1], 
                  bbox[2], 
                  bbox[3], 
                  bbox[4]), 
                resolution = "100km")
}

#' @export
#' @rdname bng_grid
bng_grid_50km <- function(xmin, ymin, xmax, ymax, ...) {
  
  # Default to the full GB boundary
  if (any(missing(xmin), missing(ymin), missing(xmax), missing(ymax))) {
    bbox <- bng_bounds
  } else {
    bbox <- c(xmin, ymin, xmax, ymax)
  }
  
  bbox_bng_grid(c(bbox[1], 
                  bbox[2], 
                  bbox[3], 
                  bbox[4]), 
                resolution = "50km")
}

#' @export
#' @rdname bng_grid
bng_grid_10km <- function(xmin, ymin, xmax, ymax, ...) {
  
  # Default to the full GB boundary
  if (any(missing(xmin), missing(ymin), missing(xmax), missing(ymax))) {
    bbox <- bng_bounds
  } else {
    bbox <- c(xmin, ymin, xmax, ymax)
  }
  
  bbox_bng_grid(c(bbox[1], 
                  bbox[2], 
                  bbox[3], 
                  bbox[4]), 
                resolution = "10km")
}

#' @export
#' @rdname bng_grid
bng_grid_5km <- function(xmin, ymin, xmax, ymax, ...) {
  
  # Default to the full GB boundary
  if (any(missing(xmin), missing(ymin), missing(xmax), missing(ymax))) {
    bbox <- bng_bounds
  } else {
    bbox <- c(xmin, ymin, xmax, ymax)
  }
  
  bbox_bng_grid(c(bbox[1], 
                  bbox[2], 
                  bbox[3], 
                  bbox[4]), 
                resolution = "5km")
}

#' @export
#' @rdname bng_grid
bng_grid_1km <- function(xmin, ymin, xmax, ymax, ...) {
  
  # Default to the full GB boundary
  if (any(missing(xmin), missing(ymin), missing(xmax), missing(ymax))) {
    bbox <- bng_bounds
  } else {
    bbox <- c(xmin, ymin, xmax, ymax)
  }
  
  bbox_bng_grid(c(bbox[1], 
                  bbox[2], 
                  bbox[3], 
                  bbox[4]), 
                resolution = "1km")
}


#' Helper function to generate grid features
#' Given a resolution, returns all the valid BNG grid squares covering an area
#' of interest.
#' @param bbox optional 
#' @param resolution character. Grid resolution to return. Must be one of
#'   "100km", "50km", "10km", "5km", "1km".
#' @param ... Additional arguments. Not currently used.
#' @details The \code{sf} package is required for this function.
#' @returns Data frame object of type \code{sf} with the grid reference and the
#'   grid square geometry.
#' 
#' @keywords internal
#' @noRd
bbox_bng_grid <- function(bbox,
                          resolution = c("100km", 
                                         "50km", 
                                         "10km", 
                                         "5km", 
                                         "1km"),
                          ...) {
  chk_sf_installed()
  match.arg(resolution)
  
  bng_refs <- bbox_to_bng(bbox[1], 
                          bbox[2], 
                          bbox[3], 
                          bbox[4], 
                          resolution = resolution)
  
  bng_refs <- data.frame(bng_refs)
  
  bng_refs$geometry <- bng_to_grid_geom(bng_refs$bng_reference, format = "sf")
  bng_refs <- st_sf(bng_refs, 
                    sf_column_name = "geometry", 
                    crs = 27700)
  
  bng_refs
}
