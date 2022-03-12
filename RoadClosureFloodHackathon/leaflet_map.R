# This script makes a leaflet map

# install.packages("leaflet")
library(leaflet)

coordinates <- read.csv("test.csv")
head(coordinates)

m <- leaflet() %>%
  addTiles() %>%
  # The csv file must have the 'name', 'long' and 'lat' columns
  addMarkers(lng=coordinates$long,
             lat=coordinates$lat,
             popup=coordinates$name)

# Do another "%>% addMarkers(...)" to overlay another set of points

print(m)
