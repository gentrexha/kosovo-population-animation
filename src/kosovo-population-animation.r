library(readr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(maps)
library(ggthemes)
library(gganimate)
library(hablar)

# Load data
url_csv <- '../data/population.csv'
ks_pop <- read_csv(url_csv)
ks_pop <- na.omit(ks_pop)
ks_pop <- ks_pop %>% convert(int(year))

# Set map
setwd("C:/Projects/Personal/kosovo-population-animation/src/")

shapefile <- readOGR(dsn=path.expand("../data/kosovo-shapefile"), "XK_EA_2018")

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)


# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2) + theme_map()

# Draw points and animate
# For more info:
# https://github.com/thomasp85/gganimate

anim <- map +
  geom_point(aes(x = X, y = Y, size = population),
             data = ks_pop, alpha = .5, color = "steelblue") + 
  scale_size_continuous(range = c(1, 20), 
                          breaks = c(10000, 25000, 50000, 100000, 200000)) + 
  labs(size = 'Population', title="Year: {frame_time}") +
  transition_time(year) +
  ease_aes('cubic-in-out') +
  enter_fade() +
  exit_fade()

anim

anim_save("kosovo-population.gif", animation = last_animation())