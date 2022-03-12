#' Build a graph from street data
#' @param street_geometry A list of geometry objects.
#' @param one_way A boolean vector; whether the street is one-way.
#' @return A list containing the edges, the (unique) nodes, node ids of the full
#' list of nodes collected from the street geometry.
#' @export
build_graph_from_streets <- function(street_geometry, one_way) {
  street_matrix <- street_geometry |> map(as.matrix)
  total <- do.call(rbind, street_matrix)
  total <- total[, 2:1]

  nodes <- unique(total)
  total_id <- numeric(nrow(total))
  total_id[1] <- 1

  row_counts <- street_matrix |> map(nrow)
  street_end <- cumsum(row_counts)
  header_ids <- street_end + 1

  one_way <- purrr::map2(one_way, row_counts, ~rep(.x, .y)) %>%
    do.call(c, .)

  same_location <- function(x, y) all(x == y)

  find_cid <- function(centry) find_x_position_in_y(centry, nodes) + 1

  # Main
  network_graph <- Graph(1000)

  # Start at the second row because the first node can never be duplicated
  pid <- 1
  crow <- 2
  header_pointer <- 1
  pb <- txtProgressBar(1, nrow(nodes), initial = 1, style = 3)
  for (node_cid in 2:nrow(nodes)) {
    setTxtProgressBar(pb, node_cid)
    node_centry <- nodes[node_cid, ]
    centry <- total[crow, ]

    while (!same_location(node_centry, centry)) {
      cid <- find_cid(centry)
      total_id[crow] <- cid
      # An edge requires two nodes, so skip the edge building if current node is
      # the first node of the street
      if (crow == header_ids[header_pointer]) {
        header_pointer <- header_pointer + 1
      } else {
        network_graph$add(pid, cid, list())
        if (!one_way[crow]) {
          network_graph$add(cid, pid, list())
        }
      }
      # cat(node_cid, "x", cid, " ", sep = "")
      # cat(glue::glue("G({pid}, {cid})"), "\n")

      crow <- crow + 1
      centry <- total[crow, ]
      pid <- cid
    }

    # Same location
    cid <- node_cid
    total_id[crow] <- cid
    if (crow == header_ids[header_pointer]) {
      header_pointer <- header_pointer + 1
    } else {
      network_graph$add(pid, cid, list())
      if (!one_way[crow]) {
        network_graph$add(cid, pid, list())
      }
    }
    # cat(node_cid, "-", cid, " ", sep = "")
    # cat(glue::glue("G({pid}, {cid})"), "\n")

    crow <- crow + 1
    pid <- cid
  }

  g <- network_graph$get()
  list(edges = g$graph, edges_attributes = g$attributes,
       nodes = nodes,
       total_id = total_id,
       get_adjacent = adjacent_cache(g$graph),
       street_start = c(1, head(street_end, -1)), street_end = street_end)
}


find_x_position_in_y <- Rcpp::cppFunction("
int find_cind(NumericVector x, NumericMatrix y) {
  for (int i = 0; i < y.nrow(); i++) {
    if (x[0] == y[i] && x[1] == y[i + y.nrow()]) {
      return i;
    }
  }
  return -1;
}")


#' Apply binary search to finding neighbours on a graph (edgelist)
#' @description This function will sort the graph based on the `from` column,
#' and create a function that uses binary search to locate the neighbors of a
#' given node.
#' @param edgelist The edgelist.
adjacent_cache <- function(edgelist) {
  g_sorted <- edgelist |> arrange(from)
  counts <- g_sorted %>% group_by(from) %>% summarise(count = n())
  order_hashmap <- cumsum(counts$count)

  get_adjacent <- function(x) {
    index <- binary_search(x, counts$from)
    if (index == -1) {
      return(numeric(0))
    } else if (index == 1) {
      adja_rows <- 1:order_hashmap[index]
    } else {
      adja_rows <- (order_hashmap[index-1] + 1):order_hashmap[index]
    }
    g_sorted[adja_rows, ]$to
  }

  return(get_adjacent)
}
