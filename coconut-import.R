# !
# Load important libraries
library(jsonlite)
library(leaflet)
library(ggplot2)
library(GGally)

# Setting the environment language to English. Helpful if you have to search the
# internet for error messages.
Sys.setenv(LANG = "en")

# Folder where your CoConUT file is
# (in doubt put this script and your CoConUT file there)
basedir <- '/Users/sv'
# Default folder you're working in
setwd(basedir)

# Loading the CoConUT file
coconut_file <- "coconut_file.json" # Name of your file
json_data_coco <- fromJSON(paste(basedir, coconut_file, sep = "/"), flatten = TRUE) 
coconut.df <- json_data_coco[["data"]]
rm(json_data_coco)

# Converts the timestamp into a UNIX timestamp (each second). Fractional seconds 
# are not needed here since CoConUT only records each n seconds (default is 1)
coconut.df$unix_timestamp <- as.numeric(coconut.df$timestamp)/1000

# if columns exist and have wrong names, then rename (older CoConUT versions)
if("wear bpm" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("wear bpm"="wear.bpm")) }
if("wear bpmlist" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("wear bpmlist"="wear.bpmlist")) }
if("wear delay" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("wear delay"="wear.delay")) }
if("ble bpm" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("ble bpm"="ble.bpm")) }
if("ble bpmlist" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("ble bpmlist"="ble.bpmlist")) }
if("ble hrv" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("ble hrv"="ble.hrv")) }
if("ble hrvlist" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("ble hrvlist"="ble.hrvlist")) }
if("ble delay" %in% colnames(coconut.df)) 
    { coconut.df <- plyr::rename(coconut.df, c("ble delay"="ble.delay")) }

# Data preprocessing
coconut.df$gps.speed <- as.numeric(coconut.df$gps.speed)
coconut.df$gps.longitude <- as.numeric(coconut.df$gps.longitude)
coconut.df$gps.latitude <- as.numeric(coconut.df$gps.latitude)
coconut.df$gps.latitude <- as.numeric(coconut.df$gps.latitude)
coconut.df$light.accuracy <- as.numeric(coconut.df$light.accuracy)
coconut.df$ble.bpm <- as.numeric(coconut.df$ble.bpm)
coconut.df$interaction.touch <- as.numeric(coconut.df$interaction.touch)
coconut.df$gps.accuracy <- as.numeric(coconut.df$gps.accuracy)
coconut.df$light.lux <- as.numeric(coconut.df$light.lux)
coconut.df$bluetooth.numOfBTDevices <- as.numeric(coconut.df$bluetooth.numOfBTDevices)
# if you need to preprocess more data, you should do it here

# Not really needed but maybe nice for graphs
coconut.df$second_counter = (coconut.df$unix_timestamp - min(coconut.df$unix_timestamp))

########### Basic file structure and first statistics ##########################

# See which columns are in the data frame with the according type
str(coconut.df)
# Open the data frame
View(coconut.df)
# Summary about the speed values
summary(coconut.df$gps.speed)
# Mean and SD of the speed values
print("Mean and sd of speed:")
mean(coconut.df$gps.speed, na.rm=TRUE)
sd(coconut.df$gps.speed, na.rm=TRUE)

########################## Plots ###############################################

# Scatterplot with the data speed, light and bluetooth
scatterplotdata <- coconut.df[,c(4, 6, 10)]
ggpairs(data=scatterplotdata, title="Scatterplot")

# Speed over the measurement time
p <- ggplot(coconut.df, aes(x = second_counter, y = gps.speed)) + 
  geom_path() +
  labs(x="Second", y="Speed (km/h)")
p

# Touch interaction over the measurement time
p <- ggplot(coconut.df, aes(x = second_counter, y = interaction.touch)) + 
  geom_point() +
  labs(x="Second", y="Touch Interaction")
p

########################## Maps ################################################

# Settings for the map
# This particular map is centered on Vienna
color_palette <- c("green", "yellow", "red")
na_color <- "blue"
ref_lng <- 16.355676
ref_lat <- 48.21790
ref_zoom <- 12
pal = colorNumeric(color_palette, 
                   domain = coconut.df$gps.speed, 
                   na.color = na_color, 
                   reverse = FALSE)

# Drawing an interactive Map with leaflet
m <- leaflet(data = coconut.df) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addCircleMarkers(lng = coconut.df$gps.longitude, 
                   lat = coconut.df$gps.latitude, 
                   color = ~pal(gps.speed), 
                   stroke = FALSE, 
                   fillOpacity = 0.9, 
                   label = ~as.character(round(gps.speed))) %>% 
  addLegend(pal = pal, values = ~gps.speed, title = "Speed (km/h)") %>% 
  setView(lng=ref_lng, lat=ref_lat, zoom = ref_zoom) %>%
  addMiniMap()
m  # Print the map



