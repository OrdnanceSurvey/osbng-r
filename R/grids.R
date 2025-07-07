#' Map BNG References to Spatial Objects
#' 
#' Generate a spatial data frame of BNG references and grid square geometries.
#' @param xmin,ymin,xmax,ymax Optional bounding box coordinates.
#' @param ... Additional arguments. Not currently used.
#' @details
#' These convenience functions generate a spatial data frame of BNG references
#' and grid square geometries at pre-determined resolutions. This function
#' combines \code{bbox_to_bng()} and \code{bng_to_grid_geom()} into a data
#' frame. Optionally, the grid can be for a defined bounding box area of
#' interest. If the bounding box is omitted, then all grid squares within the
#' valid bounds of the BNG are returned.
#' 
#' Only selected resolutions are provided to avoid excessively large data
#' frames. For additional spatial data files of all resolutions, please see the
#' [\code{osbng-grids} GitHub
#' repo](https://github.com/OrdnanceSurvey/osbng-grids/).
#' 
#' The \code{sf} package is required for this function.
#' 
#' @returns Data frame object of type \code{sf} with the grid reference as a
#'   \code{BNGReference} object and the grid square polygon geometry.
#'   
#' @examplesIf require("sf")
#' bng_grid_100km()
#' 
#' bng_grid_1km(529476, 179654, 532170, 181116)
#' 
#' @seealso [bbox_to_bng()], [geom_to_bng()]
#' @export
#' @name bng_grid
bng_grid_100km <- function(xmin, ymin, xmax, ymax, ...) {
  
  bbox <- chk_bbox(xmin, ymin, xmax, ymax)
  bbox_bng_grid(bbox, resolution = "100km")
}

#' @export
#' @rdname bng_grid
bng_grid_50km <- function(xmin, ymin, xmax, ymax, ...) {
  
  bbox <- chk_bbox(xmin, ymin, xmax, ymax)
  bbox_bng_grid(bbox, resolution = "50km")
}

#' @export
#' @rdname bng_grid
bng_grid_10km <- function(xmin, ymin, xmax, ymax, ...) {
  
  bbox <- chk_bbox(xmin, ymin, xmax, ymax)
  bbox_bng_grid(bbox, resolution = "10km")
}

#' @export
#' @rdname bng_grid
bng_grid_5km <- function(xmin, ymin, xmax, ymax, ...) {
  
  bbox <- chk_bbox(xmin, ymin, xmax, ymax)
  
  bbox_bng_grid(bbox, resolution = "5km")
}

#' @export
#' @rdname bng_grid
bng_grid_1km <- function(xmin, ymin, xmax, ymax, ...) {
  
  bbox <- chk_bbox(xmin, ymin, xmax, ymax)
  bbox_bng_grid(bbox, resolution = "1km")
}


#' List the components of the British National Grid
#' 
#' Helper functions to provide access to the bounds and set of 100km grid
#' reference identifiers.
#' @param arranged logical. Should the grid reference letters be arranged into a
#'   2D matrix? Default is \code{FALSE}.
#' @details
#' When \code{arranged} is \code{TRUE}, the matrix arrangement matches the
#' British National Grid, but note the orientation. The first element "SV" would
#' be mapped in the southwest corner.
#' 
#' @returns a character vector of 2-letter identifiers for all valid 100km grid
#'   squares. When \code{arranged} is \code{TRUE} this vector is coerced into a
#'   2D matrix.  
#'  
#' @examples
#' list_bng_prefixes()
#' 
#' list_bng_prefixes(arranged = TRUE)
#'
#' @export
#' @name list_bng
list_bng_prefixes <- function(arranged = FALSE) {
  if (arranged) {
    m <- matrix(bng_prefixes, nrow = 13, ncol = 7, byrow = TRUE)
    return(m)
  } else {
    return(bng_prefixes)
  }
}

#' @param named logical. Should the bounding box vector include the names (e.g.
#'   'xmin')? Default is \code{TRUE}.
#' @returns a numeric vector with four values for the xmin, ymin, xmax, and ymax
#'   coordinates for the valid extant of the British National Grid.
#' @examples
#' list_bng_bounds()
#' 
#' @export
#' @rdname list_bng
list_bng_bounds <- function(named = TRUE) {
  if (named) {
    return(bng_bounds)
  } else {
    return(unname(bng_bounds))
  }
}


#' Helper function to validate bbox input
#' Confirms the inputs to the functions that generate BNG feature grid data
#' frames.
#' @param xmin,ymin,xmax,ymax Optional bounding box coordinates.
#' @details
#' If bounding box inputs are missing, then the default is to use the full BNG
#' extent.
#' 
#' @returns A numeric vector of four bounding box values.
#' @keywords internal
#' @noRd
chk_bbox <- function(xmin, ymin, xmax, ymax) {
  
  # Default to the full GB boundary
  if (any(missing(xmin), missing(ymin), missing(xmax), missing(ymax))) {
    bbox <- bng_bounds
  } else {
    
    # Should only be 1 digit
    if (any(length(xmin) > 1, 
            length(xmax) > 1, 
            length(ymin) > 1, 
            length(ymax) > 1)) {
      stop("Please supply only one bounding box.", call. = FALSE)
    }
    
    bbox <- c(xmin, ymin, xmax, ymax)
  }
  
  bbox
}


#' Helper function to generate grid features
#' Given a resolution, returns all the valid BNG grid squares covering an area
#' of interest.
#' @param bbox optional 
#' @param resolution character. Grid resolution to return. Must be one of
#'   "100km", "50km", "10km", "5km", "1km".
#' @param ... Additional arguments. Not currently used.
#' @details The \code{sf} package is required for this function. Resolutions
#'   below 1km are not supported to avoid memory issues.
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
  bng_refs <- sf::st_sf(bng_refs, 
                        sf_column_name = "geometry", 
                        crs = 27700)
  
  bng_refs
}
