# Script for making spinning globe with population data
# https://www.reddit.com/r/dataisbeautiful/comments/rman0v/oc_the_worlds_population_brighter_colors_more/
# https://twitter.com/sundellviz/status/1473730669117382660
# By Anders Sundell

rm(list=ls(all=TRUE))

setwd("")
library(tidyverse)
library(raster)
library(tmap)
library(viridis)
library(gifski)
library(grid)
library(gridExtra)

# This is the NASA Earthdata population count, 5km grid cells.
pop <- raster::raster("popcount.tif")

# Set NA values on raster to -1
pop[is.na(pop)] <- -1

# Breaks for colors on map
breaklist <- c(-2, 0, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 1000000, 100000000)
nrbreaks <- length(breaklist)-1

# Orthographic projection
normalproj <- paste0("+proj=ortho +lat_0=0 +lon_0=0")

# Loop through all longitudes
for (i in -180:180){
  rightborder <- i+90
  rightborder_new <- i+90
  leftborder <- i-90
  leftborder_new <- i-90
  
  ortoproj <- paste0("+proj=ortho +lat_0=0 +lon_0=", i)
  
  # This is for managing the date line when the longitudes wrap.
  # Creates extra slices on the other side of the date line if necessary.
  if (rightborder > 180){
    addpiece_rightborder <- -180 + i - 90
    
    addcrop <- extent(-180, addpiece_rightborder,-90,90)
    addpiece <- crop(pop, addcrop)
    
    
    addpiece_rp <- projectRaster(addpiece, crs=ortoproj, res=c(9280, 9280))
    
    rightborder_new <- 180
  }
  
  # This is the same for the other end
  if (leftborder < -180){
    addpiece_leftborder <- 180+i
    
    addcrop <- extent(addpiece_leftborder, 180,-90,90)
    addpiece <- crop(pop, addcrop)
    
    
    addpiece_rp <- projectRaster(addpiece, crs=ortoproj, res=c(9280, 9280))
    
    leftborder_new <- -180
  }
  
# Crop so that only visible part is reprojected
crop <- extent(leftborder_new, rightborder_new,-90,90)
popcrop <- crop(pop, crop)

# Reproject to ortographic
pop_rp <- projectRaster(popcrop, crs=ortoproj, res=c(9280, 9280))

# Add extra slices if necessary
if (rightborder > 180){
  pop_rp <- raster::merge(pop_rp, addpiece_rp, tolerance=3000)
}

if (leftborder < -180){
  pop_rp <- raster::merge(pop_rp, addpiece_rp, tolerance=3000)
}

filestring <- paste0("gifsmaller/long_", i, ".png")

# Make map
map <- tm_shape(pop_rp, bbox=c(-7000000, -7000000, 7000000, 7000000), raster.downsample = TRUE) +
  tm_raster(palette = c("black", viridis(nrbreaks, option="A")), breaks = breaklist) +
  tm_layout(legend.show = FALSE, asp = 1, bg.color = "#111111", inner.margins = 0, outer.margins=0)

# Export png
png(filestring, width=7.4, height=7.4, units="in", res=100)
print(map)
grid.text("@sundellviz", y=0.02, x=0.98, just=c("right", "bottom"), gp=gpar(fontsize=10, col="white"))
grid.text("Population of the earth. Brighter colors:\nMore people. Data: NASA Earthdata.", y=0.02, x=0.02, just=c("left", "bottom"), gp=gpar(fontsize=8, col="white"))
dev.off()
}


# Make Gif
png_files <- list.files("gifsmaller", pattern = ".*png$", full.names = TRUE)
split <- strsplit(png_files, "gifsmaller/long_")
split <- as.numeric(sapply(split, function(x) x <- sub(".png", "", x[2])))
png_files_correct <- png_files[order(split)]

gifski(png_files_correct, gif_file = "animation_smaller.gif", width = 740, height = 740, delay = 0.05, loop=TRUE)
