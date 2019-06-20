


# Packages
library(raster)
library(ggplot2)
library(sf)


# Set working directory
setwd('tornadoesr_paths/')


# Import the raw data
tor_shps <- st_read('data/1950-2017-tornado-aspath/1950-2017-torn-aspath.shp')
# Check for missing property damage values
stopifnot(sum(is.na(tor_shps_2017$loss)) == 0)


# Only look at the year 2017 and log10-scale damage
tor_shps_2017 <- dplyr::filter(tor_shps, yr == 2017)
tor_shps_2017$log_loss <- log10(tor_shps_2017$loss + 1)


# Find the state with the most 2017 tornadoes
sort(table(tor_shps_2017$st), decreasing = TRUE)[1]
# It's TX
# Get ggplot's state shapes
states <- map_data("state")
# Subset to Texas (for viewing)
TX <- states[states$region == 'texas', ]


# View the paths map
ggplot(states) + geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.1) +
  theme_bw() + coord_quickmap() +
  geom_sf(data = tor_shps_2017, aes(col = log_loss, size = wid)) +
  coord_sf(xlim = c(min(TX$long) - 0.25, max(TX$long) + 0.25),
           ylim = c(min(TX$lat) - 0.25, max(TX$lat) + 0.25)) +
  scale_size_continuous('Width (m)', range = c(0.1, 8)) +
  viridis::scale_color_viridis('Property damage', option = 'B', direction = -1, breaks = c(2, 4, 6, 8),
                               labels = c('$100', '$10,000', '$1,000,000', '$100,000,000')) +
  labs(x = '', y = '', title = 'Tornado Paths Data - Actual Path')



# View the beginning coords map
ggplot(states) + geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.1) +
  theme_bw() + coord_quickmap() +
  geom_point(data = tor_shps_2017, aes(x = slon, y = slat, col = log_loss, size = wid), pch = 21, stroke = 2) +
  coord_sf(xlim = c(min(TX$long) - 0.25, max(TX$long) + 0.25),
           ylim = c(min(TX$lat) - 0.25, max(TX$lat) + 0.25)) +
  scale_size_area('Width (m)', breaks = c(250, 1000, 2000)) +
  viridis::scale_color_viridis('Property damage', option = 'B', direction = -1, breaks = c(2, 4, 6, 8),
                               labels = c('$100', '$10,000', '$1,000,000', '$100,000,000')) +
  labs(x = '', y = '', title = 'Tornado Paths Data - Only Starting Points')



# Import the Storm Events data and process similarly
SE_df <- read.csv('data/StormEvents_details-ftp_v1.0_d2017_c20190516.csv')
###HAVE TO FIX THIS###
######################
######################
SE_df$log_dam <- log10(SE_df$DAMAGE_PROPERTY + 1)

ggplot(states) + geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.1) +
  theme_bw() + coord_quickmap() +
  geom_point(data = SE_df, aes(x = BEGIN_LON, y = BEGIN_LAT, size = TOR_WIDTH, col = log_dam), pch = 21, stroke = 2)
  coord_sf(xlim = c(min(TX$long) - 0.25, max(TX$long) + 0.25),
           ylim = c(min(TX$lat) - 0.25, max(TX$lat) + 0.25)) +
  scale_size_area('Width (m)', breaks = c(250, 1000, 2000)) +
  viridis::scale_color_viridis('Property damage', option = 'B', direction = -1, breaks = c(2, 4, 6, 8),
                               labels = c('$100', '$10,000', '$1,000,000', '$100,000,000')) +
  labs(x = '', y = '', title = 'Storm Events Data - Only Starting Points')


a <- base::merge(x = SE_df, y = tor_shps_2017, by.x = c('BEGIN_LAT', 'BEGIN_LON'), by.y = c('slat', 'slon'))

length(unique(SE_df$BEGIN_LON)) == nrow(SE_df)

a$same_loss <- ((a$DAMAGE_PROPERTY - a$loss) == 0)

ggplot(states) + geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.1) +
  theme_bw() + coord_quickmap() +
  geom_point(data = a, aes(x = BEGIN_LON, y = BEGIN_LAT, size = TOR_WIDTH, col = same_loss), pch = 21, stroke = 1) + 
  scale_size_area('Width (m)', breaks = c(250, 1000, 2000))
  labs(x = '', y = '')
  
plot(a$same_loss ~ a$TOR_LENGTH)
  
  
  
