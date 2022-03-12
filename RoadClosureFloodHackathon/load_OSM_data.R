#====================================================================
# OSM data © OpenStreetMap contributors
# OSM data is available under the Open Database Licence.
# OSM copyright and licence: https://www.openstreetmap.org/copyright
#====================================================================

# This script loads OSM data into R
#
# Download OSM data from
# - (Specific region/city) https://download.bbbike.org/
# - (Australia-wide) https://download.geofabrik.de/australia-oceania/australia.html


# install.packages(c("osmextract", "sf"))
library(osmextract)
library(sf)

# Load data / map features
file <- "./data/australia-latest.osm.pbf"
# file <- "./data/queensland.osm.pbf"
# file <- "./data/Brisbane.osm.pbf"

# Get a summary of what's available
feature_counts <- st_layers(file)
feature_counts


# Import the features one-by-one
feature_lines <- oe_read(file) # default is 'lines'
feature_points <- oe_read(file, "points")
feature_mlines <- oe_read(file, "multilinestrings")
feature_mpolygons <- oe_read(file, "multipolygons")
feature_relations <- oe_read(file, "other_relations")

# Notes
# - points => shops, stations, restaurants, etc.
# - lines => streets
# - mlines => routes
# - mpolygons => region, boundary
# - relations => no-left-turn instructions


# Inspect the data
nrow(feature_points)
names(feature_points)
head(feature_points[, c("osm_id", "other_tags", "geometry")])
sum(grepl("restaurant", feature_points$other_tags))
View(feature_points[1:1000, ])

# Notes
# - "geometry" contains the longitude-latitude coordinates
# - Longitude ranges from -180 to 180.
# - Latitude ranges from -90 to 90.
