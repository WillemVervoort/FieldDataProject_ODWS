# set working directory
setwd("C:/Users/rver4657/ownCloud/working/SSEAC/opendataworkshop2017/exercise")

#libraries
# tidyverse, rgdal, sp
install.packages(c("tidyverse","rgdal","sp"))
library(tidyverse)
library(rgdal)
library(sp)

#load some latitude and longitude data
Sydney <- read.csv("AusLatLongdata.txt")

# create Spatial points data frame using package sp and insert coordinate reference
coord_ref <- CRS("+proj=longlat +datum=WGS84")
Sydney_sp <- SpatialPoints(coords=Sydney[,2:3],
                           proj4string = coord_ref)

# we can plot this
p <- ggplot(aes(x = Longitude, y = Latitude), 
            data = Sydney) + 
  geom_point(col="red", size = 4) +
  geom_text(aes(label = Sydney[,1]))
print(p)                                                                                            

# flatten
# reproject
Sydney_pr <- spTransform(Sydney_sp, 
                         CRS("+init=epsg:28356")) # reproject

# plot again
p <- ggplot(aes(x = Longitude, y = Latitude), 
            data = as.data.frame(Sydney_pr)) + 
  geom_point(col="red", size=4) +
  geom_text(aes(label = Sydney[,1])) + 
  xlab("Easting") + ylab("Southing")
print(p)

# distance Abbotsford (2nd row) to Airport (3rd row)
Dist <- sqrt((Sydney_pr@coords[2,1]-Sydney_pr@coords[3,1])^2 +
               (Sydney_pr@coords[2,2]-Sydney_pr@coords[3,2])^2)
paste("the distance between Abbotsford and Sydney airpoirt =",
      round(Dist,2), "m")

# image analysis
install.packages("imager")
library(imager)

our_image <- load.image("imageanalysis/5.jpg")
plot(our_image)

# make dataframe
image_df <- as.data.frame(our_image)
head(image_df,3)

# SIZE of the picture:
sqrt(nrow(image_df)/3)

# create histograms
image_df <- mutate(image_df,
        channel=factor(cc,labels=c('R','G','B')))
head(image_df,3)

#plot
ggplot(image_df,aes(value,fill=channel))+
  geom_histogram(bins=30)+
  facet_wrap(~ channel)

# spread the data
image_df2 <- image_df %>%
  mutate(cc = NULL) %>%
  spread(key = channel, value = value)
head(image_df2)

# Calculate GCC
image_df3 <- image_df2 %>%
  mutate(GCC = G/(R+B))
head(image_df3)

#plot again
pl <- ggplot(image_df3,aes(GCC)) +
  geom_histogram(bins=50) + xlab("G/(R + B)")
print(pl)