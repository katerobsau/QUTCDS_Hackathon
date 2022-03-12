PriorityQueue <- function(index = c(), value = c()) {
  put <- function(ind, val) {
    index <<- append(index, ind)
    value <<- append(value, val)
  }

  get <- function() {
    min_index <- which.min(value)
    if (length(min_index) == 0) return(-1)
    res <- index[min_index]
    remove(min_index)
    res
  }

  remove <- function(ind) {
    index <<- index[-ind]
    value <<- value[-ind]
  }

  list(
    get_index = \() index,
    get_value = \() value,
    length = \() length(index),
    put = put, get = get, remove = remove
  )
}

# # Usage
# p <- PriorityQueue()
# p$put(1, 10)
# p$get_index()
# p$get_value()
# p$put(3, 2)
# p$get_index()
# p$get_value()
# p$get()
# p$get_index()
# p$get_value()
# p$length()
