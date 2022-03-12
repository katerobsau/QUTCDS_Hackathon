library(dplyr)
library(purrr)
source("helpers/geocomputation.R")

# Example
brisbane_minibound <- rbind(
  c(152.988009,-27.486649),
  c(153.059378,-27.453065)
)

streets <- feature_lines  # from "load_OSM_data.R"

brisbane_streets <- streets_within_bounds(
  streets = streets,
  bounds = brisbane_minibound,
  method = "complete"
)


# Visualisation
m <- leaflet() |>
  addTiles() |>
  fitBounds(lng1 = brisbane_minibound[1,1], lat1 = brisbane_minibound[1,2],
            lng2 = brisbane_minibound[2,1], lat2 = brisbane_minibound[2,2])
ind <- sample(nrow(brisbane_streets), 200)
for (i in ind) {
  mat <- brisbane_streets$geometry[[i]]
  m <- m |> addPolylines(lng=mat[,1], lat=mat[,2])
}
print(m)
