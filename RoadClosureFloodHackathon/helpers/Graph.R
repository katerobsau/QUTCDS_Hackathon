# Dataframe may have a slow copying-on-modification, try vector instead.
Graph <- function(n = 10) {
  # Attributes
  G_from <- numeric(n)
  G_to <- numeric(n)
  G_attributes <- vector("list", n)
  row_pointer <- 1

  # Methods
  # Get method
  get <- function() {
    list(graph = data.frame(from = G_from, to = G_to),
         attributes = G_attributes) |>
      remove_empty_records()
  }

  # Expand storage
  expand <- function(n) {
    G_from <<- c(G_from, numeric(n))
    G_to <<- c(G_to, numeric(n))
    G_attributes <- append(G_attributes, vector("list", n))
  }

  # Add edges
  add <- function(from, to, attributes = list()) {
    if (row_pointer > length(G_from)) expand(n)
    G_from[row_pointer] <<- from
    G_to[row_pointer] <<- to
    G_attributes[[row_pointer]] <<- attributes
    row_pointer <<- row_pointer + 1
  }

  list(add = add, get = get)
}

remove_empty_records <- function(y) {
  x <- y$graph
  i <- nrow(x)
  while (x[i, 1] == 0 && x[i, 2] == 0) {
    i <- i - 1
    if (i == 0) break
  }
  list(graph = head(x, i),
       attributes = head(y$attributes, i))
}

# # Example
# my_graph <- Graph(2)
# my_graph$get()
# my_graph$add(1, 4, list(dist = 3, time = 8))
# my_graph$get()
# my_graph$add(2, 4, list(dist = 2, time = 5))
# my_graph$add(3, 4, list(dist = 1, time = 2))
# my_graph$get()
