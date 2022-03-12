# Comparing Daily Precipitation in the Lead Up to and During the 2011 and 2022 Extreme Rainfall Events
#
# This work began in the #past-observations group of the Hackathon
#
# Contributors:
#   Ben R Fitzpatrick (BRF)
#   Kate R Saunders (KRS)
#   Oaks Holland (OH)
#
# Contributions to Date:
#   - Provided the original code to download, clean and summarise data from weather stations with `rnoaa`, `lubridate` and the `tidyverse` from which this code is adapted (KRS) see https://github.com/katerobsau/conversationNSWFloods-
#   - Adapted code from KRS to use `sf` and produce animations (BRF & KRS)
#   - Prepared some of the geospatial data layers for use in this project (OH)
#
# Objectives:
#   - Calculate catchment level summary statistics of daily precipitation in the lead up to and during the 2011 and 2022 extreme rainfall events (BRF and anyone else interested)
#   - Produce visualisations (and potentially animation animations and interactive visualisations) of these summary statistics to discover and communicate key differences between the 2011 and 2022 extreme events (BRF and anyone else interested)
#   

# Ben will edit (tidy, comment and streamline) the code below the point where we are ready to start preparing catchment level summary statistics 

library(rnoaa)
library(tidyverse)
library(ozmaps)
library(sf)
library(ggspatial)
library(viridis)


# get the station meta data
all.stations <- ghcnd_stations()

all.stations

all.stations.sf <- st_as_sf(x = all.stations,
                            coords = c('longitude', 'latitude'),
                            crs = 4326
                   )

# get the QLD state boundaries

qld.border.sf <- ozmap('abs_ste') %>%
                  st_transform(crs = st_crs(all.stations.sf)) %>%
                    filter(NAME == 'Queensland')

ggplot() +
  geom_sf(data = qld.border.sf)

# crop the weather stations to those that are contained within a boundary box created from the QLD border

qld.stations.sf <- st_crop(x = all.stations.sf, y = qld.border.sf)

ggplot() +
  geom_sf(data = qld.border.sf, fill = NA) + 
  geom_sf(data = qld.stations.sf, alpha = 0.2) +
  labs(title = 'RNOAAA Data:\nWeather Stations Cropped to QLD Boundary Box') +
  theme_bw()

ggsave(filename = 'First_Plot.png')



ggplot() +
  geom_sf(data = bris.catch.bbox.sf)


qld.stations.2.sf <- st_intersection(x = qld.stations.sf, y = qld.border.sf)

ggplot() +
  geom_sf(data = qld.border.sf, fill = NA) + 
  geom_sf(data = qld.stations.2.sf, alpha = 0.2) +
  labs(title = 'RNOAAA Data:\nWeather Stations Cropped to QLD Boundary Box') +
  theme_bw()


ggsave(filename = 'Second_Plot.png')


# number of stations within QLD:
pull(qld.stations.2.sf, id) %>% unique() %>% length()

#


### Brisbane River Catchment Boundary Box

bris.catch.bbox.sf <- st_read(dsn = '~/flood_hackathon/precip/data/bris_catch_bbox/bris_catch.shp') %>%
                        st_transform(crs = st_crs(all.stations.sf))


qld.stations.3.sf <- st_intersection(x = qld.stations.2.sf, y = bris.catch.bbox.sf)


ggplot() +
  geom_sf(data = qld.border.sf, fill = NA) + 
  geom_sf(data = qld.stations.3.sf, alpha = 0.1, size = 1) +
  labs(title = 'RNOAAA Data:\nWeather Stations Cropped to\nBrisbane River Catchment Boundary Box') +
  theme_bw()

ggsave(filename = 'Third_Plot.png')


# # # # 

qld.mjr.ww.sf <- st_read(dsn = '~/flood_hackathon/precip/data/qld_mjr_waterways/QSC_Extracted_Data_20220310_185804056000-39392') %>%
                   st_transform(crs = st_crs(all.stations.sf))


ggplot() +
  geom_sf(data = qld.border.sf, fill = NA) + 
  geom_sf(data = qld.stations.3.sf, alpha = 0.1, size = 1) +
  geom_sf(data = qld.mjr.ww.sf, colour = 'blue') + 
  labs(title = 'RNOAAA Data:\nWeather Stations Cropped to\nBrisbane River Catchment Boundary Box') +
  theme_bw()


# # # #


st_intersection(x = , y = bris.catch.bbox.sf)

ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf),
          fill = NA) +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'blue') + 
  geom_sf(data = st_intersection(x = qld.stations.3.sf, y = bris.catch.bbox.sf),
          alpha = 0.2, size = 1) +
  labs(title = 'RNOAAA Data:\nWeather Stations Cropped to\nBrisbane River Catchment Boundary Box') +
  theme_bw()


ggsave(filename = 'Fourth_Plot.png')


# # # #

st_intersection(x = qld.stations.3.sf, y = bris.catch.bbox.sf) %>%
  filter(last_year == 2022 & first_year <= 2011) %>%
    pull(id) %>%
      unique() %>%
        length()

# 247 stations in the boundary box of the Brisbane River's catchment that include data from 2022 & 2011

# # # #

qld.stations.4.sf <- st_intersection(x = qld.stations.3.sf, y = bris.catch.bbox.sf) %>%
                       filter(last_year == 2022 & first_year <= 2011)


qld.stations.4.sf

ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf),
          fill = NA) +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'blue') + 
  geom_sf(data = qld.stations.4.sf,
          alpha = 0.5) +
  labs(title = 'Weather Stations with data from 2011 & 2022\nwithin boundary box of Brisbane River Catchment\n n = 247') +
  theme_bw()

ggsave(filename = 'Fifth_Plot.png')


# # # #

selected.station.ids.chr <- pull(qld.stations.4.sf, id) %>%
                              unique()


system.time(
  brc.stn.data <- meteo_pull_monitors(monitors = selected.station.ids.chr,
                                      keep_flags =  TRUE,
                                      var = "all")
)  

# 468 sec

# save(list = 'brc.stn.data', file = '~/flood_hackathon/precip/data/station_data/brc.stn.data.RData')

load('~/flood_hackathon/precip/data/station_data/brc.stn.data.RData')

colnames(brc.stn.data)

class(brc.stn.data)

# PRCP: Precipitation, in tenths of millimeters

dim(brc.stn.data)

# whooa

library(lubridate)

brcs.prcp.2010.2022.tb <- filter(brc.stn.data, year(date) > 2009) %>%
                            select(id, date, prcp, qflag_prcp) %>%
                              mutate(prcp = ifelse(!(qflag_prcp %in% c(" ", "O")),
                                     NA, prcp)) 


dim(brcs.prcp.2010.2022.tb)

#######

selected.station.ids <- pull(.data = qld.stations.4.sf, id) %>% unique()

filter(.data = all.stations, id %in% selected.station.ids) %>% pull(id) %>% unique() %>% length()


# I had to subset the station data as doing the below with the unfiltered station data crashed R on my laptop (out of RAM)
brcs.prcp.2010.2022.sf <- left_join(x = brcs.prcp.2010.2022.tb,
                                    y = filter(.data = all.stations, id %in% selected.station.ids & element == 'PRCP'),
                                    by = 'id') %>%
                            st_as_sf(coords = c('longitude', 'latitude'),
                                     crs = 4326
                            ) %>%
                              select(-first_year, -last_year)
                            
             
filter(brcs.prcp.2010.2022.sf, year(date) == 2011 & id == 'ASN00039009')

# why is prcp of class character ???

ggplot(data = filter(brcs.prcp.2010.2022.sf, year(date) == 2011 & id == 'ASN00039009')) +
  geom_line(aes(x = date, y = as.numeric(prcp)/10))

pull(brcs.prcp.2010.2022.sf, prcp) %>% as.numeric() %>% summary()

# perhaps these need to be cleaned

mutate(.data = brcs.prcp.2010.2022.sf,
       year = year(date)) %>% 
  ggplot(data = ) +
    geom_line(aes(x = date, y = as.numeric(prcp)/10, group = id), alpha = 0.01) +
    facet_wrap(facets = 'year', scales = 'free_x') +
    labs(x = 'Date', y = 'Precipitation (mm)') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = 'sixth_plot.png')


##






# pre 2011 was drought

# Data for Kate:

brcs.prcp.2022.sf <- filter(.data = brcs.prcp.2010.2022.sf, year(date) > 2021)

brcs.prcp.2022.sf

save(list = 'brcs.prcp.2022.sf', file = '~/flood_hackathon/precip/data/prepared_station_data/brcs.prcp.2022.sf.RData')

# manual animation here

date.seq <- (lubridate::as_date("2022-01-01") - 1:56) %>% as.character()

install.packages('patchwork')


date.seq[1]
library(glue)

filter(brcs.prcp.2022.sf, date %in% seq(as.Date("2022-02-16"), as.Date("2022-03-01"), by = "days")) %>% pull(prcp) %>% as.numeric() %>% max(na.rm = TRUE)/10


pull( brcs.prcp.2022.sf, prcp) %>% as.numeric() %>% summary()

for(i in 1:length(date.seq)){

  plot.i <-  ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf), 
          fill = NA, colour = 'grey') +
  #geom_sf(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #        fill = NA, colour = 'grey') +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'grey', alpha = 0.5) + 
  #geom_sf_label(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #              aes(label = BASIN_NAME), alpha = 0.5, fill = NA, colour = 'grey') + 
  geom_sf(data = filter(.data = brcs.prcp.2022.sf, date == date.seq[i]),
          aes(col = (as.numeric(prcp)/10)^(1/3)), size = 3, alpha = 0.75) +
  scale_colour_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = (c(10,500))^(1/3)) +
  # scale_colour_continuous(type = 'viridis')
  labs(title = glue('SE QLD Station Data {date.seq[i]}'), colour = expression(paste("Daily ", Precipitation^(1/3), "mm")), x = '', y = '') +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true") +
  theme_bw() 

  ggsave(plot = plot.i, filename =  glue('~/flood_hackathon/precip/data/animations/test/frames2/00{i}.png'))

  print(glue('image {i} of {length(date.seq)}'))

 } 


##################

# 2011



date.seq <- seq(as.Date("2010-11-25"), as.Date("2011-01-19"), 
                by = "days")

brcs.prcp.2010.2011.sf <- filter(.data = brcs.prcp.2010.2022.sf, date %in% date.seq)

brcs.prcp.2010.2011.sf

filter(brcs.prcp.2010.2011.sf, date %in% date.seq) %>% pull(prcp) %>% as.numeric() %>% max(na.rm = TRUE)/10

# 350 mm maximum daily rainfall

date.seq <- as.character(date.seq)

for(i in 1:length(date.seq)){

  plot.i <-  ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf), 
          fill = NA, colour = 'grey') +
  #geom_sf(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #        fill = NA, colour = 'grey') +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'grey', alpha = 0.5) + 
  #geom_sf_label(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #              aes(label = BASIN_NAME), alpha = 0.5, fill = NA, colour = 'grey') + 
  geom_sf(data = filter(.data = brcs.prcp.2010.2011.sf, date == date.seq[i]),
          aes(col = (as.numeric(prcp)/10)^(1/3)), size = 3, alpha = 0.75) +
  scale_colour_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = (c(10,500))^(1/3)) +
  # scale_colour_continuous(type = 'viridis')
  labs(title = glue('SE QLD Station Data {date.seq[i]}'), colour = expression(paste("Daily ", Precipitation^(1/3), "mm")), x = '', y = '') +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true") +
  theme_bw() 

  ggsave(plot = plot.i, filename =  glue('~/flood_hackathon/precip/data/animations/test/frames2/00{i}.png'))

  print(glue('image {i} of {length(date.seq)}'))

 } 





####################


# patchwork

library(patchwork)

index <- 1:56

# 2011

date.seq.2011 <- seq(as.Date("2010-11-25"), as.Date("2011-01-19"), 
                    by = "days")

brcs.prcp.2010.2011.sf <- filter(.data = brcs.prcp.2010.2022.sf, date %in% date.seq.2011)

filter(brcs.prcp.2010.2011.sf, date %in% date.seq.2011) %>% pull(prcp) %>% as.numeric() %>% max(na.rm = TRUE)/10

# 350 mm maximum daily rainfall

date.seq.2011 <- as.character(date.seq.2011)

####  2022

date.seq.2022 <- (lubridate::as_date("2022-03-01") - 56:1)

brcs.prcp.2021.2022.sf <- filter(.data = brcs.prcp.2010.2022.sf, date %in% date.seq.2022)


library(glue)

filter(brcs.prcp.2022.sf, date %in% seq(as.Date("2022-02-16"), as.Date("2022-03-01"), by = "days")) %>% pull(prcp) %>% as.numeric() %>% max(na.rm = TRUE)/10

date.seq.2022 <- as.character(date.seq.2022)

for(i in 1:length(index)){

  plot.2011.i <-  ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf), 
          fill = NA, colour = 'grey') +
  #geom_sf(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #        fill = NA, colour = 'grey') +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'grey', alpha = 0.5) + 
  #geom_sf_label(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #              aes(label = BASIN_NAME), alpha = 0.5, fill = NA, colour = 'grey') + 
  geom_sf(data = filter(.data = brcs.prcp.2010.2011.sf, date == date.seq.2011[i]),
          aes(col = (as.numeric(prcp)/10)^(1/3)), size = 3, alpha = 0.75) +
  scale_colour_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = (c(10,500))^(1/3)) +
  # scale_colour_continuous(type = 'viridis')
  labs(title = glue('SE QLD Station Data {date.seq.2011[i]}'), colour = expression(paste("Daily ", Precipitation^(1/3), "mm")), x = '', y = '') +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true") +
  theme_bw() 

  plot.2022.i <-  ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf), 
          fill = NA, colour = 'grey') +
  #geom_sf(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #        fill = NA, colour = 'grey') +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'grey', alpha = 0.5) + 
  #geom_sf_label(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
  #              aes(label = BASIN_NAME), alpha = 0.5, fill = NA, colour = 'grey') + 
  geom_sf(data = filter(.data = brcs.prcp.2021.2022.sf, date == date.seq.2022[i]),
          aes(col = (as.numeric(prcp)/10)^(1/3)), size = 3, alpha = 0.75) +
  scale_colour_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = (c(10,500))^(1/3)) +
  # scale_colour_continuous(type = 'viridis')
  labs(title = glue('SE QLD Station Data {date.seq.2022[i]}'), colour = expression(paste("Daily ", Precipitation^(1/3), "mm")), x = '', y = '') +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true") +
  theme_bw() 
  
  ggsave(plot = (plot.2011.i + plot.2022.i), filename =  glue('~/flood_hackathon/precip/data/animations/test/frames3/00{i}.png'))

  print(glue('image {i} of {length(date.seq)}'))

 } 


plot.2011.i + plot.2022.i










####################




  
lab()

ggsave(file = 'example.png')

####################

library(tidyverse)
library(sf)
#library(plotly)
library(gganimate)
​
# Load in the data
load('~/Downloads/brcs_prcp_2022_sf.RData')
str(brcs.prcp.2022.sf$prcp)
# Basic plot object filtered by data
date_val = '2022-02-18'
ggplot() +
  geom_sf(data = filter(.data = brcs.prcp.2022.sf, date == date_val), 
          aes(col = as.numeric(prcp)/10)) +
  ggtitle(date_val) + 
  theme_bw() 
​
# Example of animated plot from ggplot
install.packages('gapminder')​
install.packages('gapminder')
install.packages('av')
install.packages('gifski')

library('gapminder')
library('av')
library('gifski')



library(gapminder)
​
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
​
# Want to animate by date
library(lubridate)
library(transformr)
​
date_vals = seq(as.Date("2022-02-16"), as.Date("2022-03-01"), 
                by = "days")
subset_bris = filter(brcs.prcp.2022.sf, date %in% date_vals)

subset_bris


subset_bris$prcp = as.numeric(subset_bris$prcp)
str(subset_bris$prcp)
​
ggplot() +
  geom_sf(data =subset_bris, aes(col = as.numeric(prcp)/10)) +
  theme_bw() + 
  transition_time(date) +
  ease_aes('')

filter(subset_bris, is.na(prcp))





######################






save(list = c('qld.border.sf', 'qld_drainage.sf', 'qld.mjr.ww.sf', 'qld_drainage.sf', 'brcs.prcp.2022.sf'), file = '~/flood_hackathon/precip/data/prepared_station_data/se.qld.stn.prcp.2022.sf.RData')

library(transformr)

# got to here might be better to start at 2010 so we can include the summer rain up to the 2011 floods



'~/flood_hackathon/precip/data/qld_local_gov_areas/QSC_Extracted_Data_20220310_192927540000-90260/Local_Government_Areas.shp'





qld_drainage.sf <- st_read(dsn = '~/flood_hackathon/precip/data/qld_drainage_basins/QSC_Extracted_Data_20220311_091642932000-101892/Drainage_basins.shp') %>% 
                     st_transform(crs = st_crs(all.stations.sf))

#

ggplot() +
  geom_sf(data = st_intersection(x = qld.border.sf, y = bris.catch.bbox.sf), 
          fill = NA) +
  geom_sf(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
          fill = NA) +
  geom_sf(data = st_intersection(x = qld.mjr.ww.sf, y = bris.catch.bbox.sf),
          colour = 'cyan', alpha = 0.5) + 
  geom_sf_label(data = st_intersection(x = qld_drainage.sf, y = bris.catch.bbox.sf),
                aes(label = BASIN_NAME), alpha = 0.5, fill = 'white') + 
  theme_bw()  +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true") + 
  labs(title = 'Catchments and Waterways of SE QLD', x = '', y = '') 








#### Copernicus Satellite Imagery

library(ggplot2)
library(sf)
library(ggspatial)

copernicus.flood.extent.sf <- st_read(dsn = '~/flood_hackathon/precip/data/copernicus/EMSR567_AOI03_DEL_PRODUCT_r1_VECTORS_v1_vector/EMSR567_AOI03_DEL_PRODUCT_observedEventA_r1_v1.shp')

copernicus.hydro.sf <- st_read(dsn = '~/flood_hackathon/precip/data/copernicus/EMSR567_AOI03_DEL_PRODUCT_r1_VECTORS_v1_vector/EMSR567_AOI03_DEL_PRODUCT_hydrographyA_r1_v1.shp')

ggplot() +
  geom_sf(data = copernicus.hydro.sf, fill = 'blue') +
  geom_sf(data = copernicus.flood.extent.sf, fill = 'cyan') +
  theme_bw()  +
  annotation_scale(width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true") + 
  labs(title = 'Copernicus Data', x = '', y = '') 



ggsave(filename = 'Copernicus_Hydro_&_Event.png')



  

