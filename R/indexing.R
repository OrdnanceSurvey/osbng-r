
#' Convert bounding boxes
#' 
#' Create British National Grid reference from bounding boxes or convert grid
#' reference objects into bounding boxes.
#' @param xmin,ymin,xmax,ymax numeric vector of bounding box coordinates
#' @param ... additional parameters, not currently used
#' @details
#' The relationship between the bounding box and the returned BNG grid squares
#' depends on the alignment of the bounding box with the BNG index system:
#' 
#' If the bounding box edges align with the BNG system (e.g. xmin, ymin, xmax,
#' ymax are multiples of the specified resolution), only the grid squares
#' entirely contained within the bounding box are returned. Grid squares that
#' intersect but are not fully contained within the bounding box are excluded.
#' 
#' If the bounding box edges are not aligned with the BNG system, grid squares
#' that are partially overlapped by the bounding box are also included. In this
#' case, the function ensures all relevant grid squares that the bounding box
#' touches are returned, including those at the edges.
#' 
#' Validates and normalises the bounding box coordinates to the BNG index system
#' extent. If bounding box coordinates fall outside of the BNG system extent,
#' the coordinates are snapped to the bounds of the BNG system.
#' 
#' Bounding boxes are expressed as four coordinates (min x, min y, max x, max
#' y). Coordinates must be in British National Grid projection (EPSG:27700).
#' These functions do not support coordinate transformations.
#' 
#' @returns 
#'   * \code{bng_to_bbox}: numeric vector of bounding easting and northing
#'   coordinates. If multiple references are supplied to \code{bng_ref} then a
#'   matrix of coordinates is returned. 
#'   
#'   * \code{bbox_to_bng}: vector of type \code{BNGReference} objects. If multiple
#'   bounding boxes are supplied, then a list of results is returned.
#'   
#'   * \code{bng_to_grid_geom} converts the bounding box coordinates into a
#'   polygon geometry object.
#'   
#' @examples
#' bbox_to_bng(400000, 100000, 500000, 200000, "50km")
#' 
#' bbox_to_bng(285137.06, 78633.75, 299851.01, 86427.96, 5000)
#' 
#' @export
#' @rdname bng_to_bbox
#' @aliases bbox_to_bng 
bbox_to_bng <- function(...) UseMethod("bbox_to_bng")

#' @export
bbox_to_bng.numeric <- function(xmin, ymin, xmax, ymax, resolution, ...) {
  # check inputs
  if (missing(xmin) || missing(ymin) || missing(xmax) || missing(ymax)) {
    stop("Please provide bounds.", call. = FALSE)
  }
  
  if (missing(resolution)) {
    stop("Please provide a target grid reference resolution.", call. = FALSE)
  }
  
  chk_resolution <- is_valid_bng_resolution(resolution)
  
  if (all(chk_resolution == FALSE)) {
    stop("No valid resolutions provided.",
         call. = FALSE)
  }
  
  if (any(chk_resolution == FALSE)) {
    warning("Invalid resolutions detected. NAs returned.", call. = FALSE)
  }
  
  # expand values to allow vector of resolutions
  args <- expand_args(xmin, ymin, xmax, ymax, resolution)
  xmin <- args[[1]]
  ymin <- args[[2]]
  xmax <- args[[3]]
  ymax <- args[[4]]
  resolution <- args[[5]]
  
  results <- lapply(seq_along(xmin), function(i) {
    if (is_valid_bng_resolution(resolution[i])) {
      res <- internal_resolution_to_numeric(type.convert(resolution[i], 
                                                         as.is = T))
      
      xbound <- (ceiling(xmax[i] / res) * res)
      ybound <- (ceiling(ymax[i] / res) * res)
      
      if (xbound <= xmin[i] | ybound <= ymin[i]) {
        return(NA)
      }
      
      # compute grid of coordinates
      offxmn <- seq(xmin[i], xbound - 1, by = res)
      offymn <- seq(ymin[i], ybound - 1, by = res)
      coords_min <- expand.grid(offxmn, offymn)
      
      refs <- rep(NA, nrow(coords_min))
      refs <- xy_to_bng(coords_min, c(1, 2), res)
      
      return(new_bng_reference(na.omit(refs)))
    } else {
      return(NA)
    }
  })
  
  if (length(results) == 1L) {
    results <- results[[1]]
  }
  
  results
}

#' @export
bbox_to_bng.matrix <- function(x, resolution, ...) {
  # convert to numeric vector approach
  xmin <- x[, 1]
  ymin <- x[, 2]
  xmax <- x[, 3]
  ymax <- x[, 4]
  
  bbox_to_bng(xmin, ymin, xmax, ymax, resolution, ...)
}


#' @param bng_ref vector of type \code{BNGReference} objects
#' @examples
#' bng_to_bbox(as_bng_reference("SU"))
#' 
#' bng_to_bbox(as_bng_reference("SU 3 1"))
#' 
#' bng_to_bbox(as_bng_reference("SU 3 1 NE"))
#' 
#' bng_to_bbox(as_bng_reference("SU 37289 15541"))
#' 
#' @export
#' @rdname bng_to_bbox
#' @aliases bbox_to_bng
bng_to_bbox <- function(bng_ref, ...) {
  validate_bng_ref(bng_ref)
  
  ll <- bng_to_xy(bng_ref, position = "lower-left")
  ur <- bng_to_xy(bng_ref, position = "upper-right")
  
  bb <- cbind(ll, ur)
  bb
}


#' @param format character indicating the type of geometry object to return.
#'   Default is "geos" while "sf" returns an object of class \code{sfc}.
#' @examples
#' bng_to_grid_geom(as_bng_reference("SU"))
#' 
#' bng_to_grid_geom(as_bng_reference("SU 3 1"))
#' 
#' bng_to_grid_geom(as_bng_reference("SU 3 1 NE"))
#' 
#' bng_to_grid_geom(as_bng_reference("SU 37289 15541"))
#' 
#' @import geos
#' @export
#' @rdname bng_to_bbox
#' @aliases bng_to_grid_geom
bng_to_grid_geom <- function(bng_ref, format = c("geos", "sf", "wkt"), ...) {
  validate_bng_ref(bng_ref)
  
  # check inputs
  format <- match.arg(format)
  resolution <- internal_get_resolution(bng_ref)
  
  chk_resolution <- is_valid_bng_resolution(resolution)
  
  # exclude invalid inputs
  bng_ref <- bng_ref[chk_resolution]
  resolution <- resolution[chk_resolution]  #  & chk_reference
  
  # process geometry
  coords <- bng_to_xy(bng_ref)
  
  geom <- geos::geos_create_rectangle(coords[, 1],
                                      coords[, 2],
                                      coords[, 1] + resolution,
                                      coords[, 2] + resolution)
  
  if (format == "wkt") {
    geom <- geos::geos_write_wkt(geom)
    
  } else if (format == "sf") {
    chk_sf_installed()
    
    geom <- sf::st_as_sfc(geom) 
    sf::st_crs(geom) <- sf::st_crs(27700)
  }
  
  geom
}


#' Convert BNG References
#' 
#' Create British National Grid references from coordinates at a specific
#' resolution or convert grid reference objects to coordinates at a grid
#' position.
#' @param bng_ref vector of type \code{BNGReference} objects
#' @param position character indicating which point location of the BNG grid
#'   square is returned. Default is the lower-left corner.
#' @param ... additional parameters, not currently used
#' @details
#' Coordinates must be in British National Grid projection (EPSG:27700) using
#' eastings and northings in meters. These functions do not support coordinate
#' transformations.
#' 
#' @returns 
#' * \code{xy_to_bng}: vector of \code{BNGReference} objects
#' 
#' * \code{bng_to_xy}: two-column matrix of eastings and northings
#' 
#' @examples
#' bng_to_xy(as_bng_reference("SU"), "lower-left")
#' 
#' bng_to_xy(as_bng_reference("SU 3 1"), "lower-left")
#' 
#' bng_to_xy(as_bng_reference("SU 3 1 NE"), "centre")
#' 
#' bng_to_xy(as_bng_reference("SU 37289 15541"), "centre")
#' 
#' @export
#' @rdname bng_to_xy
#' @aliases xy_to_bng
bng_to_xy <- function(bng_ref, position = c("lower-left", 
                                            "upper-left", 
                                            "upper-right", 
                                            "lower-right", 
                                            "centre"), 
                      ...) {
  # check inputs
  validate_bng_ref(bng_ref)
  position <- match.arg(position)
  
  # prepare return
  coords <- matrix(NA, nrow = length(bng_ref), ncol = 2)
  
  # drop invalid inputs
  valid_idx <- !is.na(bng_ref)
  bng_ref <- bng_ref[valid_idx]
  
  # conversion
  res <- bng_to_coords(bng_ref, position)
  coords[valid_idx, ] <- res
  
  coords
}


#' @param easting numeric vector of coordinates
#' @param northing numeric vector of coordinates
#' @param resolution target BNG grid resolution. Can be specified as a numeric
#'   or character vector
#' 
#' @examples
#' xy_to_bng(437289, 115541, "100km")
#' 
#' xy_to_bng(437289, 115541, "10km")
#' 
#' xy_to_bng(437289, 115541, "5km")
#' 
#' xy_to_bng(437289, 115541, 1)
#'   
#' @export
#' @rdname bng_to_xy
#' @aliases xy_to_bng
xy_to_bng <- function(...) UseMethod("xy_to_bng")

#' @export
#' @rdname bng_to_xy
#' @aliases xy_to_bng
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
  
  if (all(chk_easting == FALSE) && all(chk_northing == FALSE)) {
    stop("No valid coordinates provided.", call. = FALSE)
  } 
  
  # allow vectors of resolutions
  args <- expand_args(easting, northing, resolution)
  
  easting <- args[[1]]
  northing <- args[[2]]
  resolution <- args[[3]]
  
  # convert resolution to numeric values
  resolution <- internal_resolution_to_numeric(resolution)
  
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
  
  if (any(!chk_easting) || any(!chk_northing)) {
    warning("Invalid coordinates detected.", call. = FALSE)
  }
  
  # create from coordinates
  res <- bng_from_coords(easting, northing, resolution)
  
  # expand results
  grid_refs[valid_idx] <- res
  
  new_bng_reference(grid_refs)
}

#' @param x two column matrix of eastings and northings
#' @examples
#' # example code
#' 
#' @export
#' @rdname bng_to_xy
#' @aliases xy_to_bng
xy_to_bng.matrix <- function(x, resolution, ...) {
  
  # convert to numeric vector approach
  e <- x[, 1]
  n <- x[, 2]
  
  xy_to_bng(e, n, resolution)
}

#' @param df data.frame with columns of coordinates to convert
#' @param cols column names or indices within \code{df} holding coordinates
#' @examples
#' # example code
#' 
#' @export
#' @rdname bng_to_xy
#' @aliases xy_to_bng
xy_to_bng.data.frame <- function(df, 
                                 cols = c("eastings", "northings"), 
                                 resolution, 
                                 ...) {
  # check inputs
  if (length(cols) != 2) {
    stop("Please provide two column names.", call. = FALSE)
  }
  
  if (any(is.character(cols)) && any(!cols %in% names(df))) {
    stop("Columns not found in data frame.", call. = FALSE)
  }
  
  # convert to numeric vector approach
  xy_to_bng(df[, cols[1]], df[, cols[2]], resolution, ...)
}


#' Spatial index for geometries
#' 
#' Returns a set of BNG Reference objects given a geometry and a specified
#' resolution.
#' @param geom geometry object of type \code{geos-geometry} or \code{sf}
#' @param resolution spatial resolution of the BNG cell expressed in string or
#'   integer values
#' @details
#' The BNG Reference objects returned represent the grid squares intersected by
#' the input geometry. BNG Reference objects are de-duplicated in cases where
#' two or more parts of a multi-part geometry intersect the same grid square.
#' 
#' This function is useful for spatial indexing and aggregation of geometries
#' against the BNG. For geometry decomposition by the BNG index system, use
#' \code{geom_to_bng_intersection instead}.
#' 
#' @returns
#' 
#' \code{geom_to_bng}: list of vectors of \code{BNGReference} objects where the
#' number of items in the list equal \code{length(geom)}.
#' 
#' \code{geom_to_bng_intersection}: list of nested lists with
#' \code{length(geom)}. Each nested list contains three named items:
#' * "BNGReference" - \code{BNGReference} objects representing the grid squares corresponding to the decomposition.
#' * "is_core" - logical vector indicating whether the grid square geometry is entirely contained by the input geometry. This is relevant for Polygon geometries and helps distinguish between "core" (fully inside) and "edge" (partially overlapping) grid squares.
#' * "geom" - The geometry representing the intersection between the input geometry and the grid square. This can one of a number of geometry types depending on the overlap. When "is_core" is \code{TRUE}, "geom" is the same as the grid square geometry.
#' 
#' @examples
#' geom_to_bng(geos::geos_make_point(430000, 110000), "100km")
#' 
#' geom_to_bng(geos::geos_make_linestring(c(430000, 430010, 430010), c(110000,
#' 110000, 110010)), "5m")
#' 
#' geom_to_bng_intersection(geos::geos_make_point(430000, 110000), "100km")
#' 
#' geom_to_bng_intersection(geos::geos_make_linestring(c(430000, 430010,
#' 430010), c(110000, 110000, 110010)), "5m")
#' 
#' geom_to_bng_intersection(geos::geos_make_polygon(c(375480.64511692,
#' 426949.67604058, 465166.20199588, 453762.88376729, 393510.2158297,
#' 375480.64511692), c(144999.23691181, 160255.02751493, 153320.57724078,
#' 94454.79935802, 91989.21703833, 144999.23691181)), "50km")
#' 
#' @import geos
#' @export
#' @rdname geom_to_bng
#' @aliases geom_to_bng_intersection
geom_to_bng <- function(geom, resolution, ...) UseMethod("geom_to_bng")

#' @export
#' @rdname geom_to_bng
#' @aliases geom_to_bng_intersection
geom_to_bng.geos_geometry <- function(geom, resolution, ...) {
  
  if (missing(resolution)) {
    stop("Please provide a target grid reference resolution.", call. = FALSE)
  }
  
  # match length of inputs
  args <- expand_args(geom, resolution)
  geom <- args[[1]]
  resolution <- args[[2]]
  
  # check resolution
  chk_resolution <- is_valid_bng_resolution(resolution)
  
  if (all(chk_resolution == FALSE)) {
    stop("No valid resolutions detected.", call. = FALSE)
  } else if (any(chk_resolution == FALSE)) {
    warning("Invalid resolution detected. NAs returned.", call. = FALSE)
  }
  
  # convert to numeric representation
  resolution <- internal_resolution_to_numeric(resolution)
  
  # if geoms are all points, then return bng by coords
  if (all(geos::geos_type(geom) == "point")) {
    return(xy_to_bng(cbind(geos::geos_x(geom),
                           geos::geos_y(geom)),
                     resolution = resolution))
  } 
  
  geom_bng_intersects(geom, resolution)
}

#' @export
#' @rdname geom_to_bng
#' @aliases geom_to_bng_intersection
geom_to_bng.sf <- function(geom, resolution, ...) {
  
  chk_sf_installed()
  
  if (missing(resolution)) {
    stop("Please provide a target grid reference resolution.", call. = FALSE)
  }
  
  geom <- geos::as_geos_geometry(geom)
  geom_bng_intersects(geom, resolution)
}


#' @param format character indicating the type of geometry object to return.
#'   Default is "geos" while "sf" returns an object of class \code{sfc}.
#' 
#' @import geos
#' @export
#' @rdname geom_to_bng
#' @aliases geom_to_bng_intersection
geom_to_bng_intersection <- function(geom, 
                                     resolution, 
                                     format = c("geos", "sf", "wkt"), 
                                     ...) {
  UseMethod("geom_to_bng_intersection")
} 

#' @export
#' @rdname geom_to_bng
#' @aliases geom_to_bng_intersection
geom_to_bng_intersection.geos_geometry <- function(geom, 
                                                   resolution, 
                                                   format = c("geos", 
                                                              "sf", 
                                                              "wkt"), 
                                                   ...) {
  
  if (missing(resolution)) {
    stop("Please provide a target grid reference resolution.", call. = FALSE)
  }
  
  format <- match.arg(format) 
  
  # match length of inputs
  args <- expand_args(geom, resolution)
  geom <- args[[1]]
  resolution <- args[[2]]
  
  # check resolution
  chk_resolution <- is_valid_bng_resolution(resolution)
  
  if (all(chk_resolution == FALSE)) {
    stop("No valid resolutions detected.", call. = FALSE)
  } else if (any(chk_resolution == FALSE)) {
    warning("Invalid resolution detected. NAs returned.", call. = FALSE)
  }
  
  # convert to numeric representation
  resolution <- internal_resolution_to_numeric(resolution)
  
  # main processing loop
  results <- lapply(seq_along(geom), function(i) {
    res <- resolution[i]
    g <- geos::geos_unnest(geom[i], max_depth = 99)
    
    if (geos::geos_type_id(g) >= 4) {
      stop("Cannot unnest collection further.", call. = FALSE)
    }
    
    refs <- geom_to_bng(g, res)
    contains <- geos::geos_contains(g, bng_to_grid_geom(refs))
    geometry <- geos::geos_intersection(g, bng_to_grid_geom(refs))
    
    if (format == "wkt") {
      geometry <- geos::geos_write_wkt(geometry)
      
    } else if (format == "sf") {
      chk_sf_installed()
      
      geometry <- sf::st_as_sfc(geometry) 
      sf::st_crs(geometry) <- sf::st_crs(27700)
    }
    
    return(list("BNGReference" = refs, 
                "is_core" = contains, 
                "geom" = geometry))
  })
  
  results
}

#' @export
#' @rdname geom_to_bng
#' @aliases geom_to_bng_intersection
geom_to_bng_intersection.sf <- function(geom, 
                                        resolution, 
                                        format = c("geos", "sf", "wkt"), 
                                        ...) {
  
  chk_sf_installed()
  
  if (missing(resolution)) {
    stop("Please provide a target grid reference resolution.", call. = FALSE)
  }
  
  # if the user submitted 'sf' then assume 'sf' return
  if (missing(format)) {
    format <- "sf"
  } else {
    format <- match.arg(format) 
  }
  
  geom <- geos::as_geos_geometry(geom)
  geom_to_bng_intersection(geom, resolution, format, ...)
}


#' Convert eastings and northings to BNG grid reference
#' 
#' Internal helper function to create BNG reference strings.
#' @param easting numeric vector of coordinates
#' @param northing numeric vector of coordinates
#' @param resolution numeric vector of resolutions in meters
#' @returns character vector of British National Grid references.
#' @keywords internal
#' @noRd
bng_from_coords <- function(easting, northing, resolution) {
  scale <- internal_get_scale(resolution)
  digits <- nchar(scale) - 1
  
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
  
  # exclude 50km digits
  x[resolution >= 50000] <- NA
  y[resolution >= 50000] <- NA
  
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
#' @noRd
bng_to_coords <- function(ref, position) {
  ref <- gsub(" ", "", as.character(ref))
  
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
  suffix[is.na(suffix)] <- -1
  
  if (any(suffix >= 0)) {
    # update resolution for suffix
    res[which(suffix >= 0, arr.ind = TRUE)] <- 
      res[which(suffix >= 0, arr.ind = TRUE)] / 2
    
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
                          n <- n + res},
         "centre"      = {e <- e + res / 2
                          n <- n + res / 2}
        )
  
  matrix(c(e, n), ncol = 2)
}


#' Generate a list of BNG indices for a geometry
#' 
#' Using the bounding box, return the set of BNG tiles that intersect with the
#' geometry.
#' @param geom object of type \code{geos-geometry}.
#' @param resolution numeric value of the BNG grid resolution to use.
#' @returns list of where each element of the list corresponds to the input
#'   geometry and contains a vector of BNG reference objects.
#' @keywords internal
#' @noRd
geom_bng_intersects <- function(geom, resolution) {
  # get BNG references under the bounding box
  allrefs <- lapply(seq_along(geom), function(i) {
    res <- resolution[i]
    g <- geos::geos_unnest(geom[i], max_depth = 99)
    
    if (geos::geos_type_id(g) >= 4) {
      stop("Cannot unnest collection further.", call. = FALSE)
    }
    
    if (all(geos::geos_type(g) == "point")) {
      refs <- xy_to_bng(cbind(geos::geos_x(g),
                              geos::geos_y(g)),
                        resolution = res)
      return(unique(refs))
      
    } else {
      bbox <- geos::geos_extent(g)
      
      refs <- bbox_to_bng(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, res)
      ints <- geos::geos_intersects(g, bng_to_grid_geom(refs))
      
      return(unique(refs[ints]))
    }
  })
  
  if (length(allrefs) == 1L) {
    allrefs <- allrefs[[1]]
  }
  
  allrefs
}
