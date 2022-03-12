# This script finds a route between two points

source("helpers/Queue.R")
source("helpers/binary_search.R")
source("helpers/path_finding.R")
source("helpers/geocomputation.R")
load("./output/street_graph", verbose = T)

# 'nodes' contains all the nodes of every street
nodes <- street_graph$nodes
nodes[60, ]
nodes[100, ]

# Find a route from node A to node B using a graph
route <- find_path(60, 100, street_graph)

# Inspect how a route is represented
str(route, max.level = 1)

# Get the sequence of points of the route
point_seq <- nodes[route$path, ]

# Get the route distance and the time to complete the route
d <- route_dist(point_seq)
d / 4 # average km per hour


# Load a leaflet map
m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng=point_seq[,2], lat=point_seq[,1]) # careful with the order
print(m)
