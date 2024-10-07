
#' @keywords internal
#' @noRd
get_prefix <- function(x) {
  as.character(gsub(bng_pattern, "\\1", x))
}


#' @keywords internal
#' @noRd
get_digits <- function(x) {
  as.character(gsub(bng_pattern, "\\2", x))
}


#' @keywords internal
#' @noRd
get_suffix <- function(x) {
  as.character(gsub(bng_pattern, "\\3", x))
}