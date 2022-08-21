# map 

library('tidyverse')
library('sf')
library('patchwork')
library('viridis')

d <- read.csv("luzerne_lackawanna_data/lack_luz_nonprofit_dat/luzerne_lack_2020_tracts.csv")[-1]
dagg <- d %>% group_by(tract) %>% summarise(total_orgs = n())

dagg$tract <- as.character(dagg$tract)

luz_shp <- sf::st_read("luzerne_lackawanna_data/luzerne_county_2020_shp/luzerne_county_2020.shp")
luz_shp$tract <- luz_shp$TRACTCE
dluz <- tigris::geo_join(luz_shp, dagg, by = "tract")
dluz[is.na(dluz$total_orgs)==TRUE ,]$total_orgs <- 0
lack_shp <- sf::st_read("luzerne_lackawanna_data/lack_county_2020_shp/lack_county_2020.shp")
lack_shp$tract <- lack_shp$TRACTCE
dlack <- tigris::geo_join(lack_shp, dagg, by = "tract")
dlack[is.na(dlack$total_orgs)==TRUE ,]$total_orgs <- 0


dlack <- dlack %>% mutate(total_discrete =
                        case_when(
                          #total_orgs < 5 ~ "1-4",
                          #total_orgs >= 5 & total_orgs < 11 ~ "less than 10",
                          total_orgs < 11 ~ "10 or less",
                          total_orgs >= 11 & total_orgs < 20 ~ "11-19",
                          total_orgs >= 20 & total_orgs < 100 ~ "20-99",
                          total_orgs >= 100 ~ "100 or more"))

dluz <- dluz %>% mutate(total_discrete =
                            case_when(
                              #total_orgs < 5 ~ "1-4",
                              #total_orgs >= 5 & total_orgs < 11 ~ "less than 10",
                              total_orgs < 11 ~ "10 or less",
                              total_orgs >= 11 & total_orgs < 20 ~ "11-19",
                              total_orgs >= 20 & total_orgs < 100 ~ "20-99",
                              total_orgs >= 100 ~ "100 or more"))
dlack$total_discrete <- factor(dlack$total_discrete, levels = c("10 or less","11-19" ,"20-99","100 or more"))
dluz$total_discrete <- factor(dluz$total_discrete, levels = c("10 or less","11-19" ,"20-99","100 or more"))

(map1a <- ggplot(data = dluz, mapping = aes(fill = total_discrete)) +
    geom_sf() + 
    scale_fill_viridis(option = "plasma", discrete = TRUE ) + 
    guides(fill=guide_legend(title="Luzerne county nonprofts",title.position="top")) +
    theme_classic(base_family = "Times", base_size = 8) +
    theme(legend.key.size = unit(.4, 'cm'), 
          legend.position = c(.02, .25), 
          legend.direction = "vertical",
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
)

(map1b <- ggplot(data = dlack, mapping = aes(fill = total_discrete)) +
    geom_sf() + 
    scale_fill_viridis(option = "plasma", discrete = TRUE) + 
    guides(fill=guide_legend(title="Lackawanna county nonprofts",title.position="top")) +
    theme_classic(base_family = "Times", base_size = 8) +
    theme(legend.key.size = unit(.4, 'cm'), 
          legend.position = c(.02, .25), 
          legend.direction = "vertical",
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
)

(fig_fin <- map1a + map1b)

cowplot::save_plot(filename = "lack_luz2county_fig.png", 
                   plot = fig_fin, nrow = 1, ncol = 2, dpi = 1500, base_height = 5, base_width = 5)

