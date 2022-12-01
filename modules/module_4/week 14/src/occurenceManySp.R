# goal: create a map, zoomed in on the area where the camp and fishing spot are, that show occurence data for a few species

# load dependencies
library(rgdal)                                                                                                      
library(raster)
library(tidyverse)
library(maps)
library(mapdata)
library(maptools)
library(mapproj)

# load the data: hairgrass, gentoo penguins, wedell seals
occurenceData <- read_csv("modules/module_4/week 14/data/allOccurenceData.csv")
hairgrassData <- read_csv("modules/module_4/week 14/data/hairgrass.csv")
sealData <- read_csv("modules/module_4/week 14/data/sealData.csv")
gentooData <- read_csv("modules/module_4/week 14/data/gentooData.csv")
adelieData <- read_csv("modules/module_4/week 14/data/adelieData.csv")

# create  map of antartcia
worldData <- data(wrld_simpl)

plot(wrld_simpl, 
     xlim = c(-85, -55),
     ylim = c(-75, -60),
     axes = TRUE, 
     col = "grey95",
     xlab = "Longitude",
     ylab = "Latitude")

# Add original observations
points(x = gentooData$longitude, 
       y = gentooData$latitude, 
       col = "orange",
       pch = 20, 
       cex = 0.75)

points(x = sealData$longitude, 
       y = sealData$latitude, 
       col = "blue",
       pch = 20, 
       cex = 0.75)

points(x = adelieData$longitude, 
       y = adelieData$latitude, 
       col = "pink",
       pch = 20, 
       cex = 0.75)

points(x = hairgrassData$longitude, 
       y = hairgrassData$latitude, 
       col = "green",
       pch = 20, 
       cex = 0.75)



# zoomed in near fishing spot
plot(wrld_simpl, 
     xlim = c(-70, -60),
     ylim = c(-69, -65),
     axes = TRUE, 
     col = "grey95",
     xlab = "Longitude",
     ylab = "Latitude")

# Add original observations
points(x = gentooData$longitude, 
       y = gentooData$latitude, 
       col = "orange",
       pch = 20, 
       cex = 0.75)

points(x = sealData$longitude, 
       y = sealData$latitude, 
       col = "blue",
       pch = 20, 
       cex = 0.75)

points(x = hairgrassData$longitude, 
       y = hairgrassData$latitude, 
       col = "green",
       pch = 20, 
       cex = 0.75)

points(x = adelieData$longitude, 
       y = adelieData$latitude, 
       col = "pink",
       pch = 20, 
       cex = 0.75)

