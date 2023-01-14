library('tigris')

d <- read.csv("luzerne_lackawanna_data/lack_luz_nonprofit_dat/lackawanna_luzerne_located.csv")[-1]
tract <- list()
pb <- txtProgressBar(min = 0, max = length(d$latitude), style = 3)
for (i in seq_along(d$ein)) {
tract[[i]] <- call_geolocator_latlon(d$latitude[[i]], d$longitude[[i]])
setTxtProgressBar(pb, i)
}

tract <- do.call(rbind.data.frame, tract)
tract$id <- tract$c..420792146002007....420691128004018....420792005001005....420691111003023...
tract$c..420792146002007....420691128004018....420792005001005....420691111003023... <- NULL
head(tract$id)
length(tract$id)
dim(d)
d$tract <- tract$id
head(d)
d$tract <- stringi::stri_sub(d$tract,from = 6, to = 11) 

d_fin <- d[d$tract %in% pat$TRACTCE | d$tract %in% patl$TRACTCE,]
dim(d_fin)[1] / dim(d)[1]
write.csv(d_fin, file = "luzerne_lack_2020_tracts.csv")
