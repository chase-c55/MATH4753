#' Get Mode
#'
#' @param v V is a vector of values
#'
#' @return A number that is the mode of the vector.
#' @export
#'
#' @examples
#' \dontrun{getmode(v)}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
