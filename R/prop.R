props <- function(...) {
#' @importFrom rlang quo_is_missing
  dots <- enquos(...)

  args <- Filter(Negate(quo_is_missing), dots)
  args
}
