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
library(lubridate)
library(ozmaps)
library(sf)
library(ggspatial)
library(viridis)
library(patchwork)
library(jsonlite)

# Load a dataframe with meta data describing all available weather stations from the Global Historical Climatology Network daily (GHCNd) data set

all.stations <- ghcnd_stations()

group_by(all.stations, id) %>%
  count() %>%
    pull(n) %>%
      summary()

# each station (uniquely identified by the column `id` has between 1 and 62 rows of data in this data frame)

# for example:

filter(.data = all.stations, id == pull(all.stations, id) %>% pluck(1))

# thus station id `ACW00011604` has rows describing that this station collects data on the following climatic variables:

filter(.data = all.stations, id == pull(all.stations, id) %>% pluck(1)) %>% pull(element)

# the abrieviations used in the column `element` are defined here:
# <https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt>

# here we are interested in rainfall so we are interested in the variables:

# PRCP = Precipitation (tenths of mm)

# thus we filter the meta data to the rows describing the collection of precipitation data at weather stations

all.stations.prcp <- filter(.data = all.stations, element == 'PRCP')

# now each station that collects precipitation data is represented by a single row (and entry in the `id` column) in `all.stations.prcp`

nrow(all.stations.prcp) == pull(all.stations.prcp, id) %>% unique() %>% length()

nrow(all.stations.prcp)

# and we can see that we have data from 117466 stations globally

# here we are interested in rainfall in the catchment of the Brisbane river in Queensland and neighbouring catchments

# thus to avoid downloading a lot more data than we need (and thereby putting unnecessary load on a shared resource) we need to identify the subset of station IDs that identify the weather stations that are situated within our catchments of interest

# if we download spatial polygons representing the boundaries of catchments in Queensland we can perform this subsetting by identifying which stations are contained within the boundaries of our catchments of interest

# first we need to identify the catchments of interest from pubically available data

# first we can load a spatial object representing the boundaries of the state of Queensland from the `ozmaps` package:

qld.border.sf <- ozmap_data() %>%
                   filter(NAME == 'Queensland')

qld.border.sf

ggplot() +
  geom_sf(data = qld.border.sf) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_bw() +
  labs(title = 'Boundaries of the State of Queensland')
  
# next we can load spatial objects representing the drainage basins of Queensland

# these data were downloaded from https://qldspatial.information.qld.gov.au/catalogue/custom/search.page?q=%22Drainage%20basins%20-%20Queensland%22

qld.catchments.sf <- st_read(dsn = '~/flood_hackathon/precip/data/qld_drainage_basins/QSC_Extracted_Data_20220311_091642932000-101892/Drainage_basins.shp')

ggplot() +
  geom_sf(data = qld.catchments.sf) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_bw() +
  labs(title = 'Boundaries of the Drainage Basins/Catchments\nof the Major Rivers in Queensland')

# then zooming in to the immediate vicinity of the catchment of the Brisbane River and including the catchment names in the plot:


ggplot() +
  geom_sf(data = st_crop(x = qld.catchments.sf, xmin = 145, xmax = 155, ymin = -30, ymax = -25)) + 
  geom_sf_label(data = st_crop(x = qld.catchments.sf, xmin = 145, xmax = 155, ymin = -30, ymax = -25),
                aes(label = BASIN_NAME))
 
# here we will confine our exploration to the data from the weather stations located within Brisbane River and neighbouring catchments, namely:
# Mary, Maroochy, Brisbane, Pine, Moreton-Bay Islands and Logan-Albert

catchments.of.interest <- c('Mary', 'Maroochy', 'Brisbane', 'Pine', 'Moreton-Bay Islands', 'Logan-Albert')

# qcoi = Queensland Catchments of Interest

qcoi.sf <- filter(.data = qld.catchments.sf, BASIN_NAME %in% c('Mary', 'Maroochy', 'Brisbane', 'Pine', 'Moreton Bay Islands', 'Logan-Albert'))

ggplot() +
  geom_sf(data = qcoi.sf) +
  geom_sf_label(data = qcoi.sf,
                aes(label = BASIN_NAME)) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_bw() +
  labs(x = '', y = '', title = 'Catchmets of Interest in Queensland') 

# we can also add on the major water ways in each of these catchments using data downloaded from here:
# <https://www.data.qld.gov.au/dataset/hydrographic-features-queensland-series/resource/6f052df7-bbbe-4eb8-a46f-c86f14498234?>

qld.mjr.ww.sf <- st_read(dsn = '~/flood_hackathon/precip/data/qld_mjr_waterways/QSC_Extracted_Data_20220310_185804056000-39392')

# check the two object use the same coordinate reference systems

st_crs( qld.mjr.ww.sf) == st_crs(qcoi.sf)

# transform the `qld.mjr.ww.sf` to use the CRS of `qcoi.sf`

st_crs( qld.mjr.ww.sf) == st_crs(qcoi.sf)

qld.mjr.ww.sf <- st_transform(x = qld.mjr.ww.sf, crs= st_crs(qcoi.sf))

st_crs( qld.mjr.ww.sf) == st_crs(qcoi.sf)

# find the major waterways contained withing the catchments of interest

qcoi.mjr.ww.sf <- st_intersection(x = qld.mjr.ww.sf, y = qcoi.sf)

# add these waterways to our plot and colour the catchments to make visually distinguishing catchment boundaries from water ways easier

ggplot() +
  geom_sf(data = qcoi.sf, aes(fill = BASIN_NAME), alpha = 0.25) +
  geom_sf(data = qcoi.mjr.ww.sf, colour = 'blue') + 
  geom_sf_label(data = qcoi.sf,
                aes(label = BASIN_NAME),
                alpha = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_bw() +
  labs(x = '', y = '', title = 'Catchmets of Interest in Queensland', fill = 'Basin Name') +
  xlim(151.5, 154)

# we are now ready to use these catchment boundaries to subset the global set of GHCNd weather stations to those contained within the boundaries of these catchments of interest:

# we just need to convert the GHCNd meta data into a simple features object:
# this simple features object will contain spatial point observation describing these weather stations

all.stations.prcp.sf <- st_as_sf(x = all.stations.prcp,
                                coords = c('longitude', 'latitude'),
                                crs = 4326
                        )

all.stations.prcp.sf

qcoi.sf <- st_transform(x = qcoi.sf,
             crs = st_crs(x = all.stations.prcp.sf))

# similarly...

qcoi.mjr.ww.sf <- st_transform(x = qcoi.mjr.ww.sf,
                               crs = st_crs(x = all.stations.prcp.sf))

# subset the weather station meta data to those that are contained within the boundaries of our catchments of interest:

qsoi.sf <- st_intersection(x = all.stations.prcp.sf, y = qcoi.sf)

# note that this also has associated BASIN_NAME (catchment name) with weather station ID in our simple features object containing the station meta data

qsoi.sf

# this will allow us to group station data by catchment to aggregate rainfall data at a catchment level and calculate catchment level summary statistics

ggplot() +
  geom_sf(data = qcoi.sf, aes(fill = BASIN_NAME), alpha = 0.25) +
  geom_sf(data = qcoi.mjr.ww.sf, colour = 'blue') +
  geom_sf(data = qsoi.sf, alpha = 0.5) +
  geom_sf_label(data = qcoi.sf,
                aes(label = BASIN_NAME),
                alpha = 0.75) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_bw() +
  labs(x = '', y = '', title = 'Catchmets of Interest in Queensland\nPoints represent Locations of GHCNd Weather Stations\nBlue Lines represent Major Water Ways', fill = 'Basin Name') +
  xlim(151.5, 154)

ggsave(filename = '~/flood_hackathon/precip/data/plots/Weather_Stations_in_Catchments_of_Interest.png')

selected.station.ids.chr <- pull(qsoi.sf, id) %>%
                              unique()

system.time(
  stn.data <- meteo_pull_monitors(monitors = selected.station.ids.chr,
                                  keep_flags =  TRUE,
                                  var = "all")
)

# 837.7 sec

# from `meteo_pull_monitors` help page:

# prcp: Precipitation, in tenths of mm

# The weather flags, which are kept by specifying keep_flags = TRUE are:

# *_mflag: Measurement flag, which gives some information on how the observation was measured.

# *_qflag: Quality flag, which gives quality information on the measurement, like if it failed to pass certain quality checks.

# *_sflag: Source flag. This gives some information on the weather collection system (e.g., U.S. Cooperative Summary of the Day, Australian Bureau of Meteorology) the weather observation comes from.

# More information on the interpretation of these flags can be found in the README file for the NCDC's Daily Global Historical Climatology Network's data at http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

# This function converts any value of -9999 to a missing value for the variables "prcp", "tmax", "tmin", "tavg", "snow", and "snwd". However, for some weather observations, there still may be missing values coded using a series of "9"s of some length. You will want to check your final data to see if there are lurking missing values given with series of "9"s.


# save(list = c('stn.data', 'qcoi.sf', 'qcoi.mjr.ww.sf', 'qsoi.sf'), file = '~/flood_hackathon/precip/data/station_data/station_data_selected_catchments.RData')

load('~/flood_hackathon/precip/data/station_data/station_data_selected_catchments.RData')

class(stn.data)

dim(stn.data)

pull(stn.data, prcp) %>% class() # why has `prcp` been downloaded as a character vector?

pull(stn.data, prcp) %>% unique() 

# looks like the NA is being interpreted as a character string rather than a missing value

pull(stn.data, prcp) %>% unique() %>% .[624] %>% class()


pull(stn.data, qflag_prcp) %>% unique()

#  " " NA  "S" "L" "O" "D" "K" "G" "X"

# QFLAG variables take values:

#           Blank = did not fail any quality assurance check
#           D     = failed duplicate check
#           G     = failed gap check
#           I     = failed internal consistency check
#           K     = failed streak/frequent-value check#
#           L     = failed check on length of multiday period 
#           M     = failed megaconsistency check
#           N     = failed naught check
#           O     = failed climatological outlier check
#           R     = failed lagged range check
#           S     = failed spatial consistency check
#           T     = failed temporal consistency check
#           W     = temperature too warm for snow
#           X     = failed bounds check
#	    Z     = flagged as a result of an official Datzilla 
#	           investigation

# thus acceptable values for qflag variables are:

# a single space character ' ' or the character 'O'

pull(stn.data, qflag_prcp) %>% unique()


stn.prcp.tb <- select(.data = stn.data, id, date, prcp, qflag_prcp) %>%
                 mutate(prcp.mm = if_else(condition = qflag_prcp %in% c(' ', 'O'),
                                           true = as.numeric(prcp)/10, # this isn't ideal it would be better if we could obtain prcp as a numeric column from `rnoaa`
                                           false = NA_real_,
                                           missing = NA_real_
                                 )
                 ) 

pull(stn.prcp.tb, prcp.mm) %>% summary()


# what range of the number of observations available from stations?

group_by(stn.prcp.tb, id) %>%
  count() %>%
    ggplot(aes(x = n/365)) + geom_histogram() + labs(x = 'Years of Observations Available at Individual Stations')


group_by(stn.prcp.tb, id) %>% 
  count() %>%
    left_join(y = st_drop_geometry(qsoi.sf), by = 'id') %>%
      ggplot(aes(x = n/365)) +
        geom_histogram() +
        facet_wrap(facets = 'BASIN_NAME', ncol = 2) + 
        labs(x = 'Years of Observations Available at Individual Stations', title = 'Station Data Faceted by Catchment/Drainage Basin') +
        theme_bw()

ggsave(filename = '~/flood_hackathon/precip/data/plots/Years_of_Observations_available_at_Stations_faceted_by_catchment.png')


# save RAM by not associating spatial position with large tibble of daily observations (instead use link between station ID and catchment created from station ID meta data and catchment outlines)

stn.prcp.bsn.tb <- left_join(x = stn.prcp.tb, y = st_drop_geometry(qsoi.sf), by = 'id')

# In 2011 Brisbane river peaked on 13th Jan

# monthly mean station rainfall within catchments 3 months prior to 2011 event:

stn.prcp.mnth.mean.2010.Oct.Nov.Dec.tb <- filter(stn.prcp.bsn.tb, year(date) %in% c(2010) & month(date) %in% c(10,11,12)) %>%
    mutate(month = month(date)) %>%
      group_by(BASIN_NAME, month, id) %>%
        summarise(stn.prcp.mnth.sum = sum(prcp.mm, na.rm = TRUE))  %>% 
          group_by(BASIN_NAME, month) %>%   
            summarise(bsn.mnth.sum.prcp = sum(stn.prcp.mnth.sum),
                      bsn.mnth.n.stations = length(unique(id)),
                      bsn.mth.mean.prcp = bsn.mnth.sum.prcp/bsn.mnth.n.stations) %>%
              mutate(mnth.chr = case_when(month == 10 ~ 'Oct 2010',
                                          month == 11 ~ 'Nov 2010',
                                          month == 12 ~ 'Dec 2010')
              ) %>%
                mutate(mnth.chr.ro = fct_relevel(.f = mnth.chr,
                                                'Oct 2010', 'Nov 2010', 'Dec 2010')
                )

ggplot() +
  geom_sf(data = full_join(x = qcoi.sf, y = stn.prcp.mnth.mean.2010.Oct.Nov.Dec.tb, by = 'BASIN_NAME'),
          aes(fill = bsn.mth.mean.prcp)) +
  facet_wrap(facets = 'month', nrow = 1) +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey') +
  labs(title = 'Catchment Level Means of Monthly Rainfall Totals at Weather Stations', fill = 'Precipitation (mm)')

dev.new()

clmm.2010.Oct.Nov.Dec.p <- ggplot() +
  geom_sf(data = full_join(x = qcoi.sf, y = stn.prcp.mnth.mean.2010.Oct.Nov.Dec.tb, by = 'BASIN_NAME'),
          aes(fill = bsn.mth.mean.prcp)) +
  facet_wrap(facets = 'mnth.chr.ro', nrow = 1) +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = c(0,550)) +
  labs(title = 'Catchment Level Means of Monthly Rainfall Totals at Weather Stations', fill = 'Precipitation (mm)')

clmm.2010.Oct.Nov.Dec.p



####

# Feb 28 2022 peak Brisbane river

stn.prcp.mnth.mean.2021.2022.Nov.Dec.Jan.tb <- filter(stn.prcp.bsn.tb,
                                                      (as_date('2021.11.01') <= date) &
                                                      (date <= as_date('2022.01.31'))
                                               ) %>%
    mutate(month = month(date)) %>%
      group_by(BASIN_NAME, month, id) %>%
        summarise(stn.prcp.mnth.sum = sum(prcp.mm, na.rm = TRUE))  %>% 
          group_by(BASIN_NAME, month) %>%   
            summarise(bsn.mnth.sum.prcp = sum(stn.prcp.mnth.sum),
                      bsn.mnth.n.stations = length(unique(id)),
                      bsn.mth.mean.prcp = bsn.mnth.sum.prcp/bsn.mnth.n.stations) %>%
              mutate(mnth.chr = case_when(month == 11 ~ 'Nov 2021',
                                          month == 12 ~ 'Dec 2021',
                                          month == 1 ~ 'Jan 2022')
              ) %>%
                mutate(mnth.chr.ro = fct_relevel(.f = mnth.chr,
                                                'Nov 2021', 'Dec 2021', 'Jan 2022')
                )

clmm.2021.2022.Nov.Dec.Jan.p <- ggplot() +
  geom_sf(data = full_join(x = qcoi.sf, y = stn.prcp.mnth.mean.2021.2022.Nov.Dec.Jan.tb, by = 'BASIN_NAME'),
          aes(fill = bsn.mth.mean.prcp)) +
  facet_wrap(facets = 'mnth.chr.ro', nrow = 1) +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = c(0,550)) +
  labs(title = 'Catchment Level Means of Monthly Rainfall Totals at Weather Stations', fill = 'Precipitation (mm)')

clmm.2021.2022.Nov.Dec.Jan.p

dev.new()


labs()



comp.p1 <- (clmm.2010.Oct.Nov.Dec.p + labs(title = '')) / (clmm.2021.2022.Nov.Dec.Jan.p + labs(title = '')) + plot_annotation(title = 'Catchment Level Means of Monthly Rainfall Totals at Weather Stations') + plot_layout(guides = 'collect') 

comp.p1

ggsave(filename = '~/flood_hackathon/precip/data/plots/Catchment_Level_Mean_Rainfall_3_Months_Preceeding_2011_2022_Extreme_Rainfall_Events.png')


### More interesting temporal aggregations suggested by Kate Saunders: 1 day prior, 3 days prior, 7 day priors and a month prior to the maximum level of the river in the catchment

# let's start with the catchment of the Brisbane river

# load the river level data:

test <- fromJSON(file = '~/flood_hackathon/river_levels/river_heights_brisbane/file_0.json')

str(test)




###


date.max.brl.2022e <- as_date('2022.02.28') # date of maximum level of Brisbane river during 2022 extreme event (currently taken from https://en.wikipedia.org/wiki/2022_eastern_Australia_floods)

date.max.brl.2022e - 90

interval(start = (date.max.brl.2022e - 90), end = date.max.brl.2022e) %>% time_length(unit = 'days')


stn.prcp.90dp.2022e.tb <- filter(stn.prcp.bsn.tb, date %within% interval(start = (date.max.brl.2022e - 90), end = date.max.brl.2022e)) %>%
                            group_by(BASIN_NAME, id) %>%
                                 summarise(stn.prcp.90d.sum = sum(prcp.mm, na.rm = TRUE)) %>%
                                   group_by(BASIN_NAME) %>%
                                     summarise(bsn.sum.stn.prcp.90d.sum = sum(stn.prcp.90d.sum, na.rm = TRUE),
                                               bsn.n.stn = length(id),
                                               bsn.mean.stn.prcp.90d.sum = bsn.sum.stn.prcp.90d.sum/bsn.n.stn
                                     )

stn.prcp.90dp.2022e.tb

ggplot() +
  geom_sf(data = left_join(x = qcoi.sf, y = stn.prcp.90dp.2022e.tb, by = 'BASIN_NAME'),
          aes(fill = bsn.mean.stn.prcp.90d.sum)) +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey') + # + , limits = c(0,550))
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_bw()

  
### make a function to compute Basin level Means of Station Precipitation Sums over Intervals

bsn.mean.stn.prcp.int.sum.f <- function(
                                    stn.data = stn.prcp.bsn.tb, # tibble of daily precipitation data for each station in each basin of interest 
                                    bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), # character vector of Basin Names for which to perform calculation
                                    end.date = '2022.02.28', # end date as character vector of form yyyy.mm.dd
                                    int.width = 90,# numeric vector length one interval width in days
                                    prcp.scale.limits = NULL, # numeric vector of length two giving upper and lower bound for precipitation scale limits for colour scale in plots
                                    bsn.sf = qcoi.sf # simple features object containing spatial polygons representing the boundaries of the drainage basins (catchments) specified in `bsn.id`
                                  ){

  data.sum <- filter(stn.data, date %within% interval(start = (as_date(end.date) - int.width), end = as_date(end.date)) &
                                     BASIN_NAME %in% bsn.id) %>%
                      group_by(BASIN_NAME, id) %>%
                        summarise(stn.prcp.sum = sum(prcp.mm, na.rm = TRUE)) %>%
                          group_by(BASIN_NAME) %>%
                            summarise(bsn.sum.prcp = sum(stn.prcp.sum, na.rm = TRUE),
                                      bsn.n.stn = length(id),
                                      bsn.mean.prcp = bsn.sum.prcp/bsn.n.stn
                            )

  plot.output <- ggplot() +
                   geom_sf(data = left_join(x = bsn.sf, y = data.sum, by = 'BASIN_NAME'),
                           aes(fill = bsn.mean.prcp)) +
                   scale_fill_gradient(high = "#132B43", low = "#56B1F7", na.value = 'grey', limits = prcp.scale.limits) + # + , limits = c(0,550))
                   annotation_north_arrow(location = "tr", which_north = "true") +
                   theme_bw() +
                   labs(fill = 'Precipitation (mm)', title = glue('{int.width} day period up to and including {end.date}') )

  return(plot.output)

}  

prcp.scale.limits.value <- c(0, 1400)

bsn.prcp.sum.mean.90d.2022.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2022.02.28', 
                                                            int.width = 90,
                                                            prcp.scale.limits = prcp.scale.limits.value, 
                                                            bsn.sf = qcoi.sf 
                                )

prcp.scale.limits.value.30d <- c(0, 1000)

bsn.prcp.sum.mean.30d.2022.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2022.02.28', 
                                                            int.width = 30,
                                                            prcp.scale.limits = prcp.scale.limits.value.30d, 
                                                            bsn.sf = qcoi.sf 
                                )

prcp.scale.limits.value.7d <- c(0, 1000)

bsn.prcp.sum.mean.7d.2022.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2022.02.28', 
                                                            int.width = 7,
                                                            prcp.scale.limits = prcp.scale.limits.value.7d, 
                                                            bsn.sf = qcoi.sf 
                                )


prcp.scale.limits.value.3d <- c(0, 900)

bsn.prcp.sum.mean.3d.2022.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2022.02.28', 
                                                            int.width = 3,
                                                            prcp.scale.limits = prcp.scale.limits.value.3d, 
                                                            bsn.sf = qcoi.sf 
                                )

bsn.prcp.sum.mean.3d.2022.p



# In 2011 Brisbane river peaked on 13th Jan

bsn.prcp.sum.mean.90d.2011.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2011.01.13', 
                                                            int.width = 90,
                                                            prcp.scale.limits = prcp.scale.limits.value, 
                                                            bsn.sf = qcoi.sf 
                                )

bsn.prcp.sum.mean.30d.2011.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2011.01.13', 
                                                            int.width = 30,
                                                            prcp.scale.limits = prcp.scale.limits.value.30d, 
                                                            bsn.sf = qcoi.sf 
                                )

bsn.prcp.sum.mean.7d.2011.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2011.01.13', 
                                                            int.width = 7,
                                                            prcp.scale.limits = prcp.scale.limits.value.7d, 
                                                            bsn.sf = qcoi.sf 
                                )

bsn.prcp.sum.mean.3d.2011.p <- bsn.mean.stn.prcp.int.sum.f(stn.data = stn.prcp.bsn.tb, 
                                                            bsn.id = pull(stn.prcp.bsn.tb, BASIN_NAME) %>% unique(), 
                                                            end.date = '2011.01.13', 
                                                            int.width = 3,
                                                            prcp.scale.limits = prcp.scale.limits.value.3d, 
                                                            bsn.sf = qcoi.sf 
                                )





(bsn.prcp.sum.mean.3d.2022.p | bsn.prcp.sum.mean.7d.2022.p | bsn.prcp.sum.mean.30d.2022.p | bsn.prcp.sum.mean.90d.2022.p) / (bsn.prcp.sum.mean.3d.2011.p | bsn.prcp.sum.mean.7d.2011.p | bsn.prcp.sum.mean.30d.2011.p | bsn.prcp.sum.mean.90d.2011.p ) + plot_annotation(title = 'Catchment Level Means of Total Precipitation at Weather Stations')


ggsave(filename = '~/flood_hackathon/precip/data/plots/Catchment_Level_Mean_Rainfall_Various_Periods_Leading_Up_to_2011_and_2022_Flooding.png')


### Animations of cumulative rainfall


### Unedited code from hackathon days below...







### example: maximum rainfall in 5 day period at each station over history of record collection at station

# group_by(id) %>%
#   arrange(date) %>%
#     mutate(prcp.lag1 = lag(x = prcp.mm, n = 1),
#            prcp.lag2 = lag(x = prcp.mm),
#            ...
#           prcp.lag5 = lag(x = prcp.mm, n = 5) %>%
#           prcp.5.day.prcp = sum(prcp.lag1 + prcp.lag2 + ... + prcp.l5)
#       summarise(max(prcp.5.day.prcp))

# for above be comparable between stations I think we need to make sure that we have one row for each station for each date between global min date and global max date

# colour points on map by this
# fill catchment with mean

# calculate % of historic max rain in 5 day period that fell at each station in 5 days preceeding maximum river level during each event

# Feb 28 2022 peak Brisbane river  (later may want to do this separately for each catchment)

# 13 Jan 2011  peak Brisbane river

# PRCP: Precipitation, in tenths of millimeters

# qflag_prcp 



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



  

