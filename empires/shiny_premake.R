rm(list=ls(all=TRUE))
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

library(shiny)


crs.wgs84 <- CRS("+init=EPSG:4326")
crs.laea <- CRS("+init=EPSG:3035")
help(ne_download)

worldmap <- ne_download(scale=110, type="land", category="physical", returnclass = "sf")
lakemap <- ne_download(scale=110, type="lakes", category="physical", returnclass = "sf")
rivermap <- ne_download(scale=110, type="rivers_lake_centerlines", category="physical", returnclass = "sf")
st_crs(worldmap) <- crs.wgs84

worldmap_valid <- st_make_valid(worldmap)

ext_europe <- c(xmin = -13, ymin = 27.4, xmax = 50.1, ymax = 71.4)

europemap <- st_crop(worldmap_valid, ext_europe)
europemap <- st_transform(europemap, crs.laea)

europe_rivers <- st_crop(rivermap, ext_europe)
europe_rivers <- st_transform(europe_rivers, crs.laea)

# MAKE GRID
area_honeycomb_grid <- st_make_grid(europemap, c(100000, 100000), what = "polygons", square = FALSE)

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

sp_land <- as(europeland, Class="Spatial")
sp_rivers <- as(europe_rivers, Class="Spatial")

landraster <- rasterize(sp_land, emptyraster)
riverraster <- rasterize(sp_rivers, emptyraster)

hexcrop_centroids <- st_centroid(hexcrop) %>%
  as(Class = "Spatial")

# SAVE FILE
save(landraster,file="assets/landraster.Rdata")
save(riverraster,file="assets/riverraster.Rdata")

st_write(hexcrop, "assets/hexcrop.shp", append=FALSE)
st_write(hexcrop_centroids, "assets/hexcrop_centroids.shp", append=FALSE)


