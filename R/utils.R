
#' @keywords internal
#' @noRd
get_prefix <- function(x) {
  as.character(gsub(bng_pattern, "\\1", x))
}


#' @keywords internal
#' @noRd
get_digits <- function(x) {
  x <- as.character(gsub(bng_pattern, "\\2", x))
  x <- gsub(" ", "", x)  # remove centre space
  x
}


#' @keywords internal
#' @noRd
get_suffix <- function(x) {
  as.character(gsub(bng_pattern, "\\3", x))
}


#' @keywords internal
#' @noRd
expand_args <- function(...) {
  # Based on: https://stackoverflow.com/a/9335687
  dots <- list(...)
  max_length <- max(lengths(dots))
  lapply(dots, rep, length.out = max_length)
}


#' check sf is installed
#' @keywords internal
#' @noRd
chk_sf_installed <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  }
}
