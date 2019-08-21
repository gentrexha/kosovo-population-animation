library(ggplot2)
library(maps)
library(ggthemes)
library(rgdal)

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
            color = 'gray', fill = 'white', size = .2)

print(map) 

# Using the ggplot2 function coord_map will make things look better and it will also let you change
# the projection. But sometimes with large shapefiles it makes everything blow up.
map_projected <- map +
  coord_map()

print(map_projected)