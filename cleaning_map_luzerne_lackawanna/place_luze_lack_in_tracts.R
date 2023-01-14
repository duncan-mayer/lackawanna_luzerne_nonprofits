# place nonprofits in census tracts 
library(sf)
library(tidyverse)
d <- read.csv("luzerne_lackawanna_data/lack_luz_nonprofit_dat/lackawanna_luzerne_located.csv")[-1]
pat <- tigris::tracts(state = "PA", county = "luzerne", year = 2020)
ggplot() + geom_sf(data = pat, fill= "white") + geom_point(data = d, aes(x = longitude, y= latitude))


d <- d %>% st_as_sf(coords = c( "longitude","latitude"),
                    crs = st_crs(pat))

#d$tract_2010 <- as.character(st_within(d, oht)) # this is fast for 1e6 points
head(d)
dim(d)


dluz <- d[d$fips == "42069",]
my_points_tract <- st_join(dluz, pat)
my_points_tract$tract_2010 <- my_points_tract$TRACTCE
my_points_tract<- my_points_tract[is.na(my_points_tract$tract_2010)==FALSE, ]


patl <- tigris::tracts(state = "PA", county = "lackawanna", year = 2020)

my_points_tractl <- st_join(d, patl)
my_points_tractl$tract_2010 <- my_points_tractl$TRACTCE
my_points_tractl<- my_points_tract[is.na(my_points_tractl$tract_2010)==FALSE, ]

d_fin <- rbind(my_points_tract, my_points_tractl)
# geocoding rate 94.75 percent 
dim(d_fin)[1]/ dim(d)[1]

d_fin <- st_set_geometry(d_fin, NULL)

write.csv(d_fin, file = "luzerne_lack_2010_tracts.csv")
