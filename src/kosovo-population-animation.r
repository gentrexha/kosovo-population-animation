# Load data
library(readr)
library(dplyr)

url_csv <- '../data/population.csv'
ks_pop <- read_csv(url_csv)

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

# Draw points
library(ggplot2)
library(maps)
library(ggthemes)

map_w_points <- map +
  geom_point(aes(x = X, y = Y, size = population),
             data = ks_pop[which(ks_pop$year=='31-12-2018'), ], alpha = .5) + 
  scale_size_continuous(range = c(1, 20), 
                          breaks = c(10000, 25000, 50000, 100000)) + 
  labs(size = 'population')

print(map_w_points)