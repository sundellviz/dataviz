rm(list=ls(all=TRUE))
setwd("/Users/xsunde/Dropbox/Reddit/Empires/")
library(tidyverse)
library(rgdal)
library(viridis)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rnaturalearth)
library(viridis)

library(reshape)
library(gdistance)
library(readxl)

library(shiny)


crs.wgs84 <- CRS("+init=EPSG:4326")
crs.laea <- CRS("+init=EPSG:3035")
crs.mercator <- CRS("+init=EPSG:3395")
crs.albers <- raster::crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

help(ne_download)

crs.laea <- crs.albers

worldmap <- ne_download(scale=50, type="land", category="physical", returnclass = "sf")
seamap <- ne_download(scale=50, type="ocean", category="physical", returnclass = "sf")
#lakemap <- ne_download(scale=110, type="lakes", category="physical", returnclass = "sf")
rivermap <- ne_download(scale=50, type="rivers_lake_centerlines", category="physical", returnclass = "sf")
st_crs(worldmap) <- crs.wgs84
st_crs(seamap) <- crs.wgs84


worldmap_valid <- st_make_valid(worldmap)


#Europe 
#ext_europe <- c(xmin = -13, ymin = 22.4, xmax = 50.1, ymax = 71.4)

# Asia
ext_europe <- c(xmin = 61.6, ymin = 5.2, xmax = 149.9, ymax = 46.8)

# America
ext_europe <- c(xmin = -169.2, ymin = 6.1, xmax = -38.3, ymax = 72.6)

europemap <- st_crop(worldmap_valid, ext_europe)
sea_crop <- raster::crop(as(seamap, Class="Spatial"), extent(europemap))
sea_crop_rp <- spTransform(sea_crop, crs.laea)

europemap <- st_transform(europemap, crs.laea)

europe_rivers <- st_crop(rivermap, ext_europe)
europe_rivers <- st_transform(europe_rivers, crs.laea)

# MAKE GRID
area_honeycomb_grid <- st_make_grid(europemap, c(80000, 80000), what = "polygons", square = FALSE)

europeland <- st_union(europemap, by_feature=FALSE)

hexcrop <- st_intersection(europeland, area_honeycomb_grid)
hexcrop <- st_as_sf(hexcrop)


hexcrop <- hexcrop %>%
  dplyr::rename("geometry" = 1) %>%
  mutate(hexnr = seq(1:nrow(hexcrop)))



hexcrop$selected <- 0
hexcrop$inempire <- 0

hexmid <- st_centroid(hexcrop) %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(hexnr = hexcrop$hexnr)



emptyraster <- raster()
extent(emptyraster) <- extent(hexcrop)
res(emptyraster) <- 50000

crs(emptyraster) <- crs.laea

sp_land <- as(europeland, Class="Spatial")
sp_rivers <- as(europe_rivers, Class="Spatial")

landraster <- rasterize(sp_land, emptyraster)
searaster <- rasterize(sea_crop_rp, emptyraster)
#searaster <- raster("shiny/assets/searaster.grd")
#landraster <- raster("shiny/assets/landraster.grd")



riverraster <- rasterize(sp_rivers, emptyraster)
riverraster[!is.na(riverraster)] <- 1


hexcrop_centroids <- st_centroid(hexcrop) %>%
  as(Class = "Spatial")

# SAVE FILE
st_write(hexcrop, "assets/hexcrop_america.shp", append=FALSE)


st_write(europeland, "assets/land_america.shp", append=FALSE)
st_write(europe_rivers, "assets/rivers_america.shp", append=FALSE)

writeRaster(landraster,filename="assets/landraster_america.grd", overwrite = TRUE)
writeRaster(riverraster,filename="assets/riverraster_america.grd", overwrite = TRUE)
writeRaster(searaster,filename="assets/searaster_america.grd", overwrite = TRUE)

save(hexcrop_centroids, file="assets/hexcrop_centroids_america.shp")
save(hexmid, file="assets/hexmid_america.Rdata")


