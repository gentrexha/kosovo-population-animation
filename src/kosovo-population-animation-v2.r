library(readr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(maps)
library(ggthemes)
library(gganimate)
library(hablar)
library(mapproj)
library(scales)
library(RColorBrewer)
library(ggmap)
library(transformr)
library(magick)

# set wd
setwd("C:/Projects/Personal/kosovo-population-animation/src/")

# Load data
url_csv <- "../data/population.csv"
ks_pop <- read_csv(url_csv)
ks_pop <- na.omit(ks_pop)
ks_pop <- ks_pop %>% convert(int(year))

# Set map
shapefile <- readOGR(dsn = path.expand("../data/kosovo-shapefile"), 
    "XK_EA_2018", use_iconv = TRUE, encoding = "UTF-8")

# Next the shapefile has to be converted to a dataframe for
# use in ggplot2
shapefile_df <- fortify(shapefile, name = "XK_NAME")

# Adding id to each row
id_map <- data.frame(id = c(0:39), komuna = shapefile$XK_NAME, 
    stringsAsFactors = FALSE)
write.csv(id_map, "id_map.csv")

# Add id to ks_pop, right?
merged_df <- merge(shapefile_df, ks_pop, by = "id", all.x = TRUE)
final_df <- merged_df[order(merged_df$order), ]

# aggregate data to get mean latitude and mean longitude for
# each state
cnames <- aggregate(cbind(long, lat) ~ komuna_me_e, data = final_df, 
    FUN = function(x) mean(range(x)))

# new cpalette
getPalette = colorRampPalette(brewer.pal(9, "Greens"))

# basic plot
ggplot() + geom_polygon(data = final_df[which(final_df$year == 
    1948), ], aes(x = long, y = lat, group = group, fill = population), 
    color = "black", size = 0.25) + coord_map() + labs(title = "Population in Kosovo in 1948") + 
    scale_fill_distiller(name = "Population", palette = "Greens", 
        direction = 1, breaks = pretty_breaks(n = 7), limits = c(min(final_df$population, 
            na.rm = TRUE), max(final_df$population, na.rm = TRUE))) + 
    theme_nothing(legend = TRUE) + geom_text(data = cnames, aes(long, 
    lat, label = komuna_me_e), size = 3, fontface = "bold")

# Final plots
for (year in c(1948:2018)) {
    int_plot <- ggplot() + geom_polygon(data = final_df[which(final_df$year == 
        year), ], aes(x = long, y = lat, group = group, fill = population), 
        color = "black", size = 0.25) + coord_map() + labs(title = paste("Population of Kosovo in ", 
        toString(year))) + scale_fill_distiller(name = "Population", 
        palette = "Greens", direction = 1, breaks = pretty_breaks(n = 7), 
        limits = c(min(final_df$population, na.rm = TRUE), max(final_df$population, 
            na.rm = TRUE))) + theme_nothing(legend = TRUE) + 
        geom_text(data = cnames, aes(long, lat, label = komuna_me_e), 
            size = 3, fontface = "bold")
    ggsave(sprintf("animation-v2/pop_%s.png", toString(year)), 
        plot = int_plot)
}