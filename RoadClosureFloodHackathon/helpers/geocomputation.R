# Compute the distance of a route
route_dist <- function(mat0) {
  # return KM
  colnames(mat0) = c("lon", "lat")
  sum(geodist::geodist(mat0, sequential = TRUE)) / 1000
}


#==============================================================================
# Filter the streets by a bounding box
streets_within_bounds <- function(streets, bounds, method = "complete") {
  in_bbox <- lines_in_bbox(streets$geometry, bounds, method)
  streets[which(in_bbox), ]
}


# Check if a point is inside a general polygon ----
points_in_polygon <- function(geometry_list, bounds) {
  geometry_list |>
    map_lgl(function(x) {
      x |>
        as.matrix() |>
        is_point_in_polygon(bounds)
    })
}


# Check if a line is in a bounding box ----
lines_in_bbox <- function(geometry_list, bounds, method = "partial") {
  if (method == "partial") {
    return(lines_in_bbox_partial(geometry_list, bounds))
  }
  if (method == "complete") {
    return(lines_in_bbox_complete(geometry_list, bounds))
  }
  stop("'method' must be 'partial' or 'complete'.")
}


# Check if a line is partially in a bounding box ----
lines_in_bbox_partial <- function(geometry_list, bounds) {
  simplified_bounds <- cbind(range(bounds[,1]), range(bounds[,2]))
  geometry_list |>
    map_lgl(~is_partial_in_bound(as.matrix(.x), simplified_bounds))
}

is_partial_in_bound <- function(x, y) {
  is_range_overlap(range(x[,1]), range(y[,1])) &&
    is_range_overlap(range(x[,2]), range(y[,2]))
}
# is_partial_in_bound(cbind(c(1,2), c(3,4)),
#                     cbind(c(1.5, 2.5), c(3.5, 4.5)))
# [1] TRUE
# is_partial_in_bound(cbind(c(1,2), c(3,4)),
#                     cbind(c(1.5, 2.5), c(4.1, 4.5)))
# [1] FALSE
# is_partial_in_bound(cbind(c(1,2), c(3,4)),
#                     cbind(c(1.9, 2.5), c(3, 4.5)))
# [1] TRUE
# is_partial_in_bound(cbind(c(1,2), c(3,4)),
#                     cbind(c(2.1, 2.5), c(3, 4.5)))
# [1] FALSE


is_range_overlap <- function(r1, r2) {
  contain(r1, min(r2)) || contain(r1, max(r2)) ||
    contain(r2, min(r1)) || contain(r2, max(r1))
}
# is_range_overlap(c(1,3), c(4, 5))
# [1] FALSE
# is_range_overlap(c(1,3), c(3,5))
# [1] TRUE
# is_range_overlap(c(1,4), c(3,5))
# [1] TRUE


contain <- function(range0, value0) {
  (min(range0) <= value0) && (value0 <= max(range0))
}


# Check if a line is fully in a bounding box ----
lines_in_bbox_complete <- function(geometry_list, bounds) {
  simplified_bounds <- cbind(range(bounds[,1]), range(bounds[,2]))
  geometry_list |>
    map_lgl(~is_complete_in_bound(as.matrix(.x), simplified_bounds))
}

is_complete_in_bound <- function(x, y) {
  is_range_subset(x[,1], y[,1]) && is_range_subset(x[,2], y[,2])
}

is_range_subset <- function(x, y) {
  (min(y) <= min(x)) && (max(x) <= max(y))
}


#==============================================================================
#' Find the nearest node on a graph
#'
#' @note The distance is computed using the Euclidean distance.
#'
#' @param x A pair of coordinates.
#' @param x A matrix of coordinates; the node-list of a graph.
#'
#' @return The coordinates of the nearest node.
#'
#' @export
nearest_node <- function(x, nodes, k = 1) {
  d <- pdist::pdist(x, nodes)@dist
  ind <- head(order(d), k)
  list(index = ind, coordinates = nodes[ind, ])
}


#' Check if a matrix of coordinates are in bound
#'
#' @param p A pair of coordinates.
#' @param bounds A matrix of coordinates defining the boundary.
#'
#' @return TRUE / FALSE.
#'
#' @export
#'
#' @references https://stackoverflow.com/questions/217578/how-can-i-determine-whether-a-2d-point-is-within-a-polygon
is_point_in_polygon <- function(p, bounds) {
  is_inside <- FALSE
  x_range <- range(bounds[,1])
  y_range <- range(bounds[,2])

  # If outside bounding box, then return FALSE
  px <- p[1]
  py <- p[2]
  if (px < x_range[1] || px > x_range[2] ||
      py < y_range[1] || py > y_range[2]) {
    return(FALSE)
  }

  i <- 1
  j <- nrow(bounds)
  while (i <= nrow(bounds)) {
    if (( (bounds[i, 2] > py) != (bounds[j, 2] > py) ) &&
        px < (bounds[j, 1] - bounds[i, 1]) * (py - bounds[i, 2]) / (bounds[j, 2] - bounds[i, 2]) + bounds[i,1]) {
      is_inside <- !is_inside
    }
    j <- i
    i <- i + 1
  }
  is_inside
}
