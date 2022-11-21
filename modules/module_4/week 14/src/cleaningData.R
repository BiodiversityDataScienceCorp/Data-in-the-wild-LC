install.packages("spocc")

library("tidyverse")
library("spocc")

# goal: create one csv with all of the species

##### Gentoo pengins #########
##query the data from gbif and inat
gentoo <-occ(query="Pygoscelis papua", from=c("inat", "gbif"), limit=4000) ;
gentooGBIF <- gentoo$gbif$data$Pygoscelis_papua
gentooINAT <- gentoo$inat$data$Pygoscelis_papua

##make GBIF and INAT data ready to be merged##

# initial check of the GBIF data
unique(gentooGBIF$occurrenceStatus) #all present, no need to remove
sort(unique(gentooGBIF$individualCount)) #to see if there are places where count = 0

# select only longitude and latitude  from GBIF data
gbifLocation <- select(gentooGBIF,c("latitude", "longitude"))

# select only location from INAT data
inatLocation <- select(gentooINAT, c("location"))

# split location into longitude and latitude in INAT data
inatLocation <- inatLocation %>%
  separate(location, c("latitude", "longitude"), ",")

# make sure longitude and latitude data in INAT is numerical
inatLocation$longitude = as.numeric(inatLocation$longitude)
inatLocation$latitude = as.numeric(inatLocation$latitude)

## combine the data frames of INAT and GBIF ## 
gentooData <- rbind(gbifLocation, inatLocation)

# remove duplicate entries (ie entries that are in gbif from inat)
gentooData <- gentooData[!duplicated(gentooData) ,  ]

# Reverse order of columns
gentooData <- gentooData[, c("longitude", "latitude")]


# create a csv with the combined data longitude and latitude
write_csv(gentooData, "modules/module_4/week 14/data/gentooData.csv")


#### Wedell seals
##query the data from gbif and inat
seals <-occ(query="Leptonychotes weddellii", from=c("inat", "gbif"), limit=4000) ;
sealsGBIF <- seals$gbif$data$Leptonychotes_weddellii
sealsINAT <- seals$inat$data$Leptonychotes_weddellii

##make GBIF and INAT data ready to be merged##

# initial check of the GBIF data
unique(sealsGBIF$occurrenceStatus) #all present, no need to remove
sort(unique(sealsGBIF$individualCount)) #to see if there are places where count = 0

# select only longitude and latitude  from GBIF data
gbifLocationSeals <- select(sealsGBIF,c("latitude", "longitude"))

# select only location from INAT data
inatLocationSeals <- select(sealsINAT, c("location"))

# split location into longitude and latitude in INAT data
inatLocationSeals <- inatLocationSeals %>%
  separate(location, c("latitude", "longitude"), ",")

# make sure longitude and latitude data in INAT is numerical
inatLocationSeals$longitude = as.numeric(inatLocationSeals$longitude)
inatLocationSeals$latitude = as.numeric(inatLocationSeals$latitude)

## combine the data frames of INAT and GBIF ## 
sealData <- rbind(gbifLocationSeals, inatLocationSeals)

# remove duplicate entries (ie entries that are in gbif from inat)
sealData <-sealData[!duplicated(sealData) ,  ]

# Reverse order of columns
sealData <- sealData[, c("longitude", "latitude")]

# create a csv with the combined data, includes source, longitude and latitude
write_csv(sealData, "modules/module_4/week 14/data/sealData.csv")

#### Adelie penguins
#### Wedell seals
##query the data from gbif and inat
adelie <-occ(query="Pygoscelis adeliae", from=c("inat", "gbif"), limit=4000) ;
adelieGBIF <- adelie$gbif$data$Pygoscelis_adeliae
adelieINAT <- adelie$inat$data$Pygoscelis_adeliae

##make GBIF and INAT data ready to be merged##

# initial check of the GBIF data
unique(adelieGBIF$occurrenceStatus) #all present, no need to remove
sort(unique(adelieGBIF$individualCount)) #to see if there are places where count = 0

# select only longitude and latitude  from GBIF data
gbifLocationAdelie<- select(adelieGBIF,c("latitude", "longitude"))

# select only location from INAT data
inatLocationAdelie <- select(adelieINAT, c("location"))

# split location into longitude and latitude in INAT data
inatLocationAdelie <- inatLocationAdelie %>%
  separate(location, c("latitude", "longitude"), ",")

# make sure longitude and latitude data in INAT is numerical
inatLocationAdelie$longitude = as.numeric(inatLocationAdelie$longitude)
inatLocationAdelie$latitude = as.numeric(inatLocationAdelie$latitude)

## combine the data frames of INAT and GBIF ## 
adelieData <- rbind(gbifLocationAdelie, inatLocationAdelie)

# remove duplicate entries (ie entries that are in gbif from inat)
adelieData <- adelieData[!duplicated(adelieData) ,  ]

# Reverse order of columns
adelieData <- adelieData[, c("longitude", "latitude")]

# create a csv with the combined data, includes source, longitude and latitude
write_csv(adelieData, "modules/module_4/week 14/data/adelieData.csv")



























hairgrassData <- read_csv("modules/module_4/week 14/data/hairgrass.csv")
hairgrassData <- hairgrassData %>% select("longitude", "latitude")

hairgrassData$Sp <- "hairgrass"
gentooData$Sp <- "gentoo penguin"
sealData$Sp <- "wedell seal"

allOccurenceData <- rbind(hairgrassData, gentooData, sealData)

# create a csv with the combined data, includes source, longitude and latitude
write_csv(allOccurenceData, "modules/module_4/week 14/data/allOccurenceData.csv")

