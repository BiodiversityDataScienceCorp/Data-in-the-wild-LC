install.packages("dismo")
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("sp")
install.packages("tidyverse")
install.packages("move")
install.packages("rgdal")

# load dependencies
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library("tidyverse")
library("move")
library("rgdal")

bioclim_data <- getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "modules/module_4/week 14/data/")

# Read in observations
obs_data <- read.csv(file = "data/milkweedCombo copy.csv")

# Determine geographic extent of our data
max_lat <- ceiling(max(obs_data$latitude))
min_lat <- floor(min(obs_data$latitude))
max_lon <- ceiling(max(obs_data$longitude))
min_lon <- floor(min(obs_data$longitude))
geographic_extent <- extent(x = c(min_lon, max_lon, min_lat, max_lat))

# Load the data to use for our base map
data(wrld_simpl)

## observations 

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min_lon, max_lon),
     ylim = c(min_lat, max_lat),
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observation
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()

# species distrubtion model

# Crop bioclim data to geographic extent 
bioclim_data <- crop(x = bioclim_data, y = geographic_extent)

# Drop unused column
obs_data <- obs_data[, c("latitude", "longitude")]

# Reverse order of columns
obs_data <- obs_data[, c("longitude", "latitude")]

# Build species distribution model
bc_model <- bioclim(x = bioclim_data, p = obs_data)

predict_presence <- dismo::predict(object = bc_model, 
                                   x = bioclim_data)

# Plot base map
plot(wrld_simpl, 
     xlim = c(-39, -50),
     ylim = c(-80, -40),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(predict_presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
box()


##### To get antarctica

##### Antarctica bioclim data (in folder called ESM in data)

rastlist <- list.files(path = 'data/ESM', pattern = "*d2.tif$", full.names=TRUE)
allrasters <- lapply(rastlist, raster)
allrasters <- stack(rastlist)

# Build species distribution model
antarcticaBc_model <- bioclim(x = allrasters, p = obs_data)

predict_presence <- dismo::predict(object = antarcticaBc_model, 
                                   x = bioclim_data)

# Plot base map
plot(wrld_simpl, 
     xlim = c(-55, -35),
     ylim = c(-80, -60),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(predict_presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
box()

## mila making model 
rastlist <- list.files(path = 'data/ESM', pattern = "*d2.tif$", full.names=TRUE)
allrasters <- lapply(rastlist, raster)
allrasters <- raster::stack(rastlist)

## the paper used the mapproj library to transform the data
library(mapproj)

# to view individual layers
plot(allrasters, 1, main = "Annual Mean Temperature")
plot(allrasters, 1, main = "Annual Mean Temperature on the Peninsula", xlim = c(-0.45, -0.2), 
     ylim = c(0.1, 0.3))

obs_dataAnt <- obs_data %>% filter(latitude < -60) 
# the problem is that the antarctica bioclim data does not use the same coordinate system 
# mutate observational data so that it is in cartesian coordinates

R = 
  obsDataFormatted <- obs_data %>% 
  mutate(x = R * cos(latitude) * cos(longitude)) %>% 
  mutate(y = R * cos(latitdue) * sin(longitude))

sf::st_crs(allrasters)      
allrastersLatLong <- spTransform(allrasters[1], CRS=CRS("+proj=longlat +datum=WGS84"))
allrastersLatLong <- spTransform(allrasters[[1]], CRS=CRS("+proj=longlat +datum=WGS84"))
allrastersLatLong <- projectRaster(allrasters, "+proj=longlat +datum=WGS84")


# Ethan trying to fix the coordinates (was actually in stereographic (WHY))
# thank you stackoverflow
library(rgdal)
my.points = data.frame(x=c(0,180),y=c(90,0))
my.points = SpatialPoints(my.points, CRS('+proj=longlat'))
my.points = spTransform(my.points, CRS('+proj=stere'))
my.points = data.frame(x=p$x * 18040096/2 , y=p$y * 9020048)
my.points = SpatialPoints(my.points, CRS('+proj=moll'))
my.points = as.data.frame(spTransform(my.points, CRS('+proj=longlat')))
my.points$x = my.points$x + 200
my.points$x[my.points$x > 180] = my.points$x[my.points$x > 180] - 360

presvals <- raster::extract(allrasters, obs_dataAnt)
backgr <- randomPoints(allrasters, 100)
absvals <- raster::extract(allrasters, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata <- na.omit(sdmdata)

pairs(sdmdata[,2:5], cex=0.1, fig=TRUE)

sdmdataSubset <- sdmdata %>% select("pb", "bio1_d2", 'bio10_d2', 'bio11_d2', 'bio12_d2', 'bio13_d2', 'bio14_d2')

sdmdataSubset$pb <- 1

# add the points that are in chile
presvals2 <- raster::extract(bioclim_data, obs_data)
backgr2 <- randomPoints(bioclim_data, 500)
absvals2 <- raster::extract(bioclim_data, backgr2)
pb <- c(rep(1, nrow(presvals2)), rep(0, nrow(absvals2)))
sdmdata2 <- data.frame(cbind(pb, rbind(presvals2, absvals2)))
sdmdata2 <- na.omit(sdmdata2)
# rename columns
sdmdata2 <- sdmdata2 %>% 
  rename(bio1_d2 = bio1) %>% 
  rename(bio10_d2 = bio10) %>% 
  rename(bio11_d2 = bio11) %>% 
  rename(bio12_d2 = bio12) %>% 
  rename(bio13_d2 = bio13) %>% 
  rename(bio14_d2 = bio14)

sdmdata2Subset <- sdmdata %>% select("pb", "bio1_d2", 'bio10_d2', 'bio11_d2', 'bio12_d2', 'bio13_d2', 'bio14_d2')

allSdmData <- rbind(sdmdataSubset, sdmdata2Subset)

m1 <- glm(pb ~ bio1_d2 + bio10_d2 + bio11_d2 + bio12_d2 + bio13_d2 + bio14_d2, data=allSdmData)
class(m1)
summary(m1)

bc2 <- bioclim(sdmdata2Subset[,c('bio1_d2', 'bio10_d2', 'bio11_d2', 'bio12_d2', 'bio13_d2')])
pairs(bc)


# need new raster that includes both locations
# rename columns

p2 <- predict(bioclim_data, bc2)
plot(p2, 
     main = "Species Distribution Model for Antarctic Hairgrass",
     add = F)

# Add original observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Plot base map
plot(wrld_simpl, 
     xlim = c(-55, -35),
     ylim = c(-80, -60),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(p, add = TRUE)

# Add original observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
