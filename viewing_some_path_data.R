


# Packages
library(raster)
library(ggplot2)
library(sf)


# Import and view raw data
tor_shps <- st_read('1950-2017-tornado-aspath/1950-2017-torn-aspath/1950-2017-torn-aspath.shp')


# Get ggplot's state shapes
states <- map_data("state")
# Subset to Georgia
GA <- states[states$region == 'georgia', ]


# Only look at the year 2017 and log10-scale damage
ga_tor_shps <- dplyr::filter(tor_shps, yr == 2017)
ga_tor_shps$log_loss <- log10(ga_tor_shps$loss + 1)


# Check for missing property damage values
sum(is.na(ga_tor_shps$loss))


# View the map
ggplot(states) + geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.1) +
  theme_bw() + coord_quickmap() +
  geom_sf(data = ga_tor_shps, aes(col = log_loss, size = wid)) +
  #xlim(min(GA$long) - 1, max(GA$long) + 1) +
  #ylim(min(GA$lat) - 1, max(GA$lat) + 1) +
  coord_sf(xlim = c(min(GA$long) - 1, max(GA$long) + 1),
           ylim = c(min(GA$lat) - 1, max(GA$lat) + 1)) +
  viridis::scale_color_viridis('Property damage', option = 'B', direction = -1, breaks = c(2, 4, 6, 8),
                               labels = c('$100', '$10,000', '$1,000,000', '$100,000,000'))


