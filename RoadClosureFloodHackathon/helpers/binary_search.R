#' Binary Search
#'
#' @param x The element to search for.
#' @param data A vector to search.
#'
#' @examples
#' x <- 5:50
#' pos <- binary_search(7, x)
#' x[pos] == 7  # check
#'
#' pos <- binary_search(3, x)
#' pos == -1  # check
binary_search <- function(x, data) {
  L <- 1
  R <- length(data)
  while (L <= R) {
    m <- floor((L + R) / 2)
    if (x > data[m])
      L <- m + 1
    else if (x < data[m])
      R <- m - 1
    else
      return(m)
  }

  # stop(sprintf("The element %d cannot be found.", x))
  return(-1)
}
