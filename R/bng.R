
#' Create BNG reference objects
#' 
#' Convert or test user input (strings) to a British National Grid reference
#' object.
#' @param bng_ref A character vector of grid references to be created or tests.
#' @details
#' The BNG is structured using a hierarchical system of grid squares at various
#' resolutions. At its highest level, the grid is divided into 100 km by 100 km
#' squares, each of which is identified by a two-letter code. Successive levels
#' of resolution further subdivide the grid squares into finer detail, down to
#' individual 1-meter squares.
#' 
#' @returns An object of type \code{BNGReference}.
#' 
#' @examples
#' as_bng_reference("TQ1234")
#' 
#' as_bng_reference(c("TQ1234", "SU5678"))
#' 
#' @seealso [is_valid_bng()], [BNGReference]
#' @export
#' @name as_bng_reference
as_bng_reference <- function(bng_ref, ...) UseMethod("as_bng_reference")

#' @export
#' @rdname as_bng_reference
as_bng_reference.BNGReference <- function(bng_ref, ...) {
  bng_ref
}

#' @export
#' @rdname as_bng_reference
as_bng_reference.character <- function(bng_ref, ...) {
  # check for valid formats
  chk_reference <- is_valid_bng(bng_ref)
  
  if (all(chk_reference == FALSE)) {
    stop("No valid BNG grid references detected.", call. = FALSE)
  } else if (!all(chk_reference)) {
    # some are valid, some are not
    bng_ref[!chk_reference] <- NA
    warning("Invalid BNG grid references detected. NAs returned.", 
            call. = FALSE)
  }
  
  # check for equal resolution
  res <- get_bng_resolution(bng_ref)
  
  if (length(unique(res)) > 1) {
    warning("Varying resolutions detected.",
            call. = FALSE)
  }
  
  new_bng_reference(bng_ref)
}

#' @export
#' @rdname as_bng_reference
is_bng_reference <- function(bng_ref) {
  inherits(bng_ref, "BNGReference")
}

#' @name BNGReference
#' @title BNG Reference objects
#' @description Functions to support working with objects of type
#'   \code{BNGReference}.
#' @param x object of class \code{BNGReference}.
#' @param i record selection.
#' @param value a suitable replacement value
#' @details
#' The BNG is structured using a hierarchical system of grid squares at various
#' resolutions. At its highest level, the grid is divided into 100 km by 100 km
#' squares, each of which is identified by a two-letter code. Successive levels
#' of resolution further subdivide the grid squares into finer detail, down to
#' individual 1-meter squares.
#' 
#' @seealso [is_valid_bng()], [as_bng_reference()]
#' @export
`[.BNGReference` <- function(x, i) {
  new_bng_reference(NextMethod())
}


#' @export
#' @rdname BNGReference
`[[.BNGReference` <- function(x, i) {
  new_bng_reference(NextMethod())
}


#' @export
#' @rdname BNGReference
`[<-.BNGReference` <- function(x, i, value) {
  if (inherits(value, "BNGReference")) {
    new_bng_reference(NextMethod())
  } else {
    stop("Value must be of type `BNGReference`.", call. = FALSE)
  }
}


#' @export
#' @rdname BNGReference
`[[<-.BNGReference` <- function(x, i, value) {
  if (inherits(value, "BNGReference")) {
    new_bng_reference(NextMethod())
  } else {
    stop("Value must be of type `BNGReference`.", call. = FALSE)
  }
}


#' @export
#' @rdname BNGReference
print.BNGReference <- function(x, ...) {
  
}


#' @export
#' @rdname BNGReference
c.BNGReference <- function(...) {
  # check resolutions all equal?
  lst <- list(...)
  classes <- sapply(lst, function(x) class(x)[1])
  ucls <- unique(classes)
  stopifnot(length(ucls) == 1)
  
  ret <- unlist(lapply(lst, unclass), recursive = FALSE)
  ret <- new_bng_reference(ret)
  ret
}


#' @export
#' @rdname BNGReference
unique.BNGReference <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x, incomparables, ...)]
}


#' @keywords internal
#' @noRd
new_bng_reference <- function(x) {
  x <- gsub(" ", "", x)
  structure(x, class = "BNGReference")
}


#' @keywords internal
get_bng_resolution <- function(bng_ref) {
  
}


#' @keywords internal
get_bng_resolution_string <- function(bng_ref) {
  
}
