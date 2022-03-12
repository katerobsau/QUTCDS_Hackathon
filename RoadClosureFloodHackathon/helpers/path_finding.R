#==============================================================================
# A* path-finding algorithm
#==============================================================================

Char <- as.character
dist <- \(x, y) sqrt(sum((x - y)^2))
manhattan <- \(x, y) sum(abs(x - y))

#' Pathfinding using the A*-algorithm
#' @param start An integer; the node id.
#' @param end An integer; the node id.
#' @param graph The graph, with both edge-list and node-list.
#' @param get_adjacent (Optional) A function for finding the neighbors of a node.
#' @export
find_path <- function(start, end, graph) {
  coordinates <- graph$nodes
  get_adjacent <- graph$get_adjacent

  heuristic <- \(a, b) manhattan(coordinates[a, ], coordinates[b, ])
  graph_cost <- \(a, b) dist(coordinates[a, ], coordinates[b, ])

  frontier <- PriorityQueue()
  frontier$put(start, 0)
  came_from <- list()
  cost_so_far <- list()
  came_from[[Char(start)]] <- NA
  cost_so_far[[Char(start)]] <- 0

  while (frontier$length() > 0) {
    current <- frontier$get()

    if (current == end)
      break

    # debug(get_adjacent)
    for (next_entry in get_adjacent(current)) {
      new_cost <- cost_so_far[[Char(current)]] + graph_cost(current, next_entry)
      if (!Char(next_entry) %in% names(cost_so_far) ||
          new_cost < cost_so_far[[Char(next_entry)]]) {
        cost_so_far[[Char(next_entry)]] <- new_cost
        priority <- new_cost + heuristic(next_entry, end)
        frontier$put(next_entry, priority)
        came_from[[Char(next_entry)]] <- current
      }
    }
    # if (frontier$length() > 10000) browser()
  }

  list(path = reconstruct_path(came_from, start, end),
       cost = cost_so_far)
}

reconstruct_path <- function(came_from, start, end) {
  current <- end
  path <- c()
  while (current != start) {
    path <- append(path, current)
    current <- came_from[[Char(current)]]
  }
  path <- append(path, start)
  rev(path)
}


#==============================================================================
# Multi-level path-finding algorithm
#==============================================================================

#' Simplify a graph
#' @note This function adds two attributes 'reduced_edges' and 'reduced_nodes'
#' to the graph object and nothing else is modified.
#' @param graph The graph to be simplified.
#' @param street_geometry A reduced list of street geometry.
simplify_graph <- function(graph, street_geometry) {
  street_matrix <- street_geometry |> map(as.matrix)
  total <- do.call(rbind, street_matrix)
  total <- total[, 2:1]

  find_node_id <- \(centry) find_x_position_in_y(centry, nodes) + 1
  keep <- 1:nrow(total) |>
    purrr::map_dbl(~find_node_id(total[.x, ])) |>
    unique()

  edges <- graph$edges
  graph$reduced_edges <- edges[edges$from %in% keep | edges$to %in% keep, ]
  graph$reduced_nodes <- total
  graph
}

#' Find path using multilevel information
#' @param start An integer; the node id of the starting position.
#' @param end An integer; the node id of the ending position.
#' @param graph A reduced graph.
#' @param get_adjacent The adjacent function of the reduced graph.
#' @export
find_path_multilevel <- function(start, end, graph) {
  nodes <- graph$nodes
  reduced_nodes <- graph$reduced_nodes
  find_node_id <- \(entry) find_x_position_in_y(entry, nodes) + 1

  src <- start
  src_approx <- nodes[src, ] |>
    nearest_node(reduced_nodes) |>
    find_node_id()

  dst <- end
  dst_approx <- nodes[dst, ] |>
    nearest_node(reduced_nodes) |>
    find_node_id()

  reduced_graph <- list(nodes = graph$nodes,
                        get_adjacent = adjacent_cache(graph$reduced_edges))
  p2 <- find_path(src_approx, dst_approx, reduced_graph)

  p1 <- find_path(src, src_approx, graph)
  p3 <- find_path(dst_approx, dst, graph)

  p1 |> join_path(p2) |> join_path(p3)
}

join_path <- function(path_1, path_2) {
  list(path = c(path_1$path, path_2$path),
       cost = append(path_1$cost, path_2$cost))
}

# # Usage
# src <- 60
# dst <- 100
# points(nodes[src, ], color = "green")
# points(nodes[dst, ], color = "red")
#
# # Direct
# route <- find_path(src, dst, street_graph)
# lines(nodes[route$path, ])
#
#
# # Multilevel
# filtered_street <- street |>
#   filter(!highway %in% c("footway", "residential", "service"))
# reduced_graph <- simplify_graph(street_graph, filtered_street$geometry)
# route <- find_path_multilevel(src, dst, reduced_graph)
# lines(nodes[route$path, ], color = "orange")
#
#
# # OSRM route
# route_best <- find_path_2(src, dst, nodes)
# route_best$geometry[[1]] %>% as_latlng_matrix() %>% lines(color = "yellow")
