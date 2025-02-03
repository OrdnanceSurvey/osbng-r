
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
  res <- internal_get_resolution(bng_ref)
  
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


#' Printing BNG Reference Class
#' 
#' Supporting formatting and printing of \code{BNGReference} objects.
#' @param x an object of type \code{BNGReference}.
#' @param compact boolean. Should standard spaces be added or removed for
#'   "pretty" printing? Default is \code{FALSE} to add spaces.
#' @param ... additional parameters
#' @details
#' Standard spaces are added: 1) after the two-letter prefix, 2) between
#' eastings and northings, and 3) before a quadrant suffix, when those
#' components exist in a grid reference.
#' 
#' @returns 
#' * \code{format} provides a standard formatting of BNG reference objects
#' * \code{print} outputs the BNG references and invisibly returns the object.
#' @examples
#' x <- as_bng_reference("SU1234")
#' print(x)
#' 
#' print(x, compact = TRUE)
#' 
#' @export
#' @name print.BNGReference
print.BNGReference <- function(x, ...) {
  res <- unique(get_bng_resolution_string(x))
  
  if (all(is.na(res)) == TRUE) {
    print(unclass(x))
    return(invisible(x))
  }

  if (length(res) > 1) {
    cat(sprintf("<%s[%s] with multiple resolutions>\n", class(x)[1], length(x)))
  } else {
    cat(sprintf("<%s[%s] with Resolution=%s>\n", class(x)[1], length(x), res))
  }

  out <- format(x, ...)
  print(out, quote = FALSE, ...)

  invisible(x)
}


#' @export
#' @rdname print.BNGReference
format.BNGReference <- function(x, compact = FALSE, ...) {
  validate_bng_ref(x)
  stopifnot(is.logical(compact))
  
  # reconstruct formatted reference
  if (compact == FALSE) {
    # split references
    prefix <- get_prefix(x)
    en <- get_digits(x)
    
    e <- substr(en, 1, nchar(en) / 2)
    n <- substr(en, (nchar(en) / 2) + 1, nchar(en))
    
    suffix <- get_suffix(x)
    
    formatted <- apply(cbind(prefix, e, n, suffix), 1, 
                       function(i) {
                         paste(i[!is.na(i) & i != ""], collapse = " ")
                       })
  } else {
    formatted <- gsub(" ", "", as.character(x))
  }
  
  # fill invalid reference strings
  formatted[formatted == ""] <- NA
  sprintf("<%s>", formatted)
}


#' #' @export
#' as.character.BNGReference <- function(x, ...) {
#'   format(x, ...)
#' }


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


#' @export
#' @rdname BNGReference
as.data.frame.BNGReference <- function(x, ...) {
  df <- data.frame(row.names = seq_along(x))
  df$bng_reference <- x
  
  df
}


#' @keywords internal
#' @noRd
new_bng_reference <- function(x) {
  x <- gsub(" ", "", x)
  structure(x, class = "BNGReference")
}
