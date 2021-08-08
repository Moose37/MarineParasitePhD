library(tidyverse)
library(sf)
library(raster)

########
##Now that we have both occurrence points and environmental layers sorted ...
# Lets establish some background points. These are points in the model that are used to characterize environments 
#in the study region so that when we predict species ranges, it's not in localities the species "can't get to"
# Two ways of doing this:
# 1) establish a circular "buffer" around occurence points and join them up.
# 2) establish a bounding box "buffer" around occurence points.
##
#1) Cicular buffer
#we have an issue with buffer formats and how they handle the date line, so I want to put a conditional piece of code to 
#do sf method if points are within 10 deg of the date line, the raster method when not. lets create functions:

method.raster <- function(x) {
  # Create spatial object for raster use
  Occ.sf <- Occ.buf <- sp::SpatialPoints(x, proj4string = crs(envs.present))
  # create individual buffers around each Occ point. width = meters. dissolve = geometries are aggregated
  raster::buffer(Occ.sf, width = 1000000, dissolve = TRUE)
  
}

#Occ.buf <- method.raster(x)

method.sf <- function(x) {
  # convert to simple features object with CRS
  Occ.sf <- sf::st_as_sf(x, coords = c("decimalLongitude", "decimalLatitude"), crs = crs(envs.present))
  #plot(Occ.sf)
  # create buffers and join them together
  sf::st_buffer(Occ.sf, dist = 1000000) %>% #dist = in meters
    sf::st_union() %>% #combine buffers that are "touching" into giant buffers
    sf::st_wrap_dateline() %>% #some buffers cross the date line, so we need to let it know this is happening
    sf::as_Spatial()
}

#Occ.buf <- method.sf(x)

###############
#Lets turn forming a background dataset into a function for use within larger models
background.buf <- function (x) { #x = Occurence points
  #1/2) Create spatial object/raster buffer object for raster use using conditional 
  if(max(x$decimalLongitude) > 170 | min(x$decimalLongitude) < -170) {Occ.buf <- method.sf(x)} else {Occ.buf <- method.raster(x)}
  
  #2) crop Env. rasters to the study extent
  envs.bg <- raster::crop(envs.present, Occ.buf) # envs must be a stack, not a brick
  envs.bg <- raster::mask(envs.bg, Occ.buf) # Next, mask the rasters to the shape of the buffers
  #3) Now let's sample a few points within the extent
  dismo::randomPoints(envs.bg[[1]], n = 10000) %>% 
    as.data.frame() %>% dplyr::select(decimalLongitude = x, decimalLatitude = y) #n = number of random points
}

#bg <- background.buf(Occ.sp.four[,2:3])

##
#2) bbox buffer
###############
#Lets turn forming a background dataset into a function for use within larger models
background.bbox <- function (x) { #x = Just Occurence points
  # Establish bounding box 
  Occ.buf <- sp::SpatialPoints(x, proj4string = crs(envs.present)) %>%
    bbox()
  # Extend bounding box by 10 degrees
  Occ.buf <- raster::extent(if_else(Occ.buf[1]-10 < -180, -180, Occ.buf[1]-10),
                            if_else(Occ.buf[3]+10 > 180, 180, Occ.buf[3]+10),
                            Occ.buf[2]-10,
                            Occ.buf[4]+10)
  # Crop environmental rasters to match the background extent
  envs.bg <- raster::crop(envs.present, Occ.buf) # envs must be a stack, not a brick
  envs.bg <- raster::mask(envs.bg, Occ.buf)
  # Sample a points within the extent
  dismo::randomPoints(envs.bg[[7]], n = 10000) %>% 
    as.data.frame() %>% dplyr::select(decimalLongitude = x, decimalLatitude = y) #n = number of random points
  
}

#bg = background.bbox(Occ.sp[,2:3])
