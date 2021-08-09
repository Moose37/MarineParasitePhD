library(tidyverse)
library(sf)
library(raster)

########
##Now that we have both occurrence points and environmental layers sorted ...
# Lets establish some background points. These are points in the model that are used to characterize environments 
# in the study region so that when we predict species ranges, it's not in localities the species "can't get to"
# Two ways of doing this:
# 1) establish a circular "buffer" around occurrence points and join them up.
# 2) establish a bounding box "buffer" around occurrence points.
##
#1) Circular buffer
#we have an issue with buffer formats and how they handle the date line, so I want to put a conditional piece of code to 
#do sp method if points are within 10 deg of the date line, the raster method when not. lets create functions:

buffer <- function(x) {
  # Create spatial object for raster use
  Occ.sp <- sp::SpatialPoints(x, proj4string = crs(envs))
  # create individual buffers around each Occ point. width = degrees.
  # byid: aggregated (joined) = False, keep individual buffers = True
  Occ.buf <- rgeos::gBuffer(Occ.sp, width = 10, byid = FALSE) 
  # And then cross the date line...
  as(Occ.buf, Class = "sf") %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES"), quiet = TRUE) %>%
    st_union() %>%
    sf::as_Spatial()
  
}

#Occ.buf <- buffer(x)

###############
#Lets turn forming a background dataset into a function for use within larger models
#1) Aggregated 10 degree buffer
background.buf <- function (x, i) { #x = Occurence points
  #1/2) Create spatial object/raster buffer object for raster use using conditional 
  #if(max(x$decimalLongitude) > 170 | min(x$decimalLongitude) < -170) {Occ.buf <- method.sf(x)} else {Occ.buf <- method.raster(x)}
  Occ.buf <- buffer (x)
  #2) crop Env. rasters to the study extent
  envs.bg <- raster::crop(envs, Occ.buf) # envs must be a stack, not a brick
  envs.bg <- raster::mask(envs.bg, Occ.buf) # Next, mask the rasters to the shape of the buffers
  #3) Now let's sample a few points within the extent
  dismo::randomPoints(envs.bg[[1]], n = i) %>% 
    as.data.frame() %>% dplyr::select(decimalLongitude = x, decimalLatitude = y) #n = number of random points
}
#plot(Occ.buf)
#bg <- background.buf(Occ.sp.four[,2:3])

##
#2) bbox buffer
###############
#Lets turn forming a background dataset into a function for use within larger models
background.bbox <- function (x, i) { #x = Just Occurence points; i = number of points
  # Establish bounding box 
  Occ.buf <- sp::SpatialPoints(x, proj4string = crs(envs)) %>%
    bbox()
  # Extend bounding box by 10 degrees
  Occ.buf <- raster::extent(if_else(Occ.buf[1]-10 < -180, -180, Occ.buf[1]-10),
                            if_else(Occ.buf[3]+10 > 180, 180, Occ.buf[3]+10),
                            Occ.buf[2]-10,
                            Occ.buf[4]+10)
  
  # Crop environmental rasters to match the background extent
  envs.bg <- raster::crop(envs, Occ.buf) # envs must be a stack, not a brick
  #envs.bg <- raster::mask(envs.bg, Occ.buf)
  # Sample a points within the extent
  dismo::randomPoints(envs.bg[[7]], n = 10000) %>% 
    as.data.frame() %>% dplyr::select(decimalLongitude = x, decimalLatitude = y) #n = number of random points
  
}

#bg = background.bbox(Occ.sp[,2:3])

#Example:
#Occ.clean <- read_rds("C:/Users/tmor201/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt4/Data/Occ_copepod_nest.rds") 
##unnest
#Occ.clean <- unnest(Occ.clean, cols = c(species,data))

#x <- Occ.clean %>%
##  filter(species %in% "Alebion carchariae")

#x <- x[,c(3:2)]

#plot to see
#see <- buffer(x)
#see <- background.buf (x, 10000)
#see <- background.bbox(x, 10000)
#plot(see)
