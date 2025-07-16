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
help(ne_download)


# LOAD relief
relief <- raster::brick("shiny/NE1_50M_SR_W/NE1_50M_SR_W.tif")
dem <- raster::brick("shiny/DEM_geotiff/alwdgg.tif")


worldmap <- ne_download(scale=50, type="land", category="physical", returnclass = "sf")
seamap <- ne_download(scale=50, type="ocean", category="physical", returnclass = "sf")
#lakemap <- ne_download(scale=110, type="lakes", category="physical", returnclass = "sf")
rivermap <- ne_download(scale=50, type="rivers_lake_centerlines", category="physical", returnclass = "sf")
st_crs(worldmap) <- crs.wgs84
st_crs(seamap) <- crs.wgs84


worldmap_valid <- st_make_valid(worldmap)



ext_europe <- c(xmin = -13, ymin = 22.4, xmax = 50.1, ymax = 71.4)


europemap <- st_crop(worldmap_valid, ext_europe)
sea_crop <- raster::crop(as(seamap, Class="Spatial"), extent(europemap))
sea_crop_rp <- spTransform(sea_crop, crs.laea)

reliefcrop <- raster::crop(relief, extent(europemap))
crs(reliefcrop) <- crs.wgs84




relief_rp <- projectRaster(reliefcrop, res = 10000, crs = crs.laea)




reliefraster_spdf <- as(relief_rp, "SpatialPixelsDataFrame") %>%
  as.data.frame()

reliefraster_spdf <- reliefraster_spdf %>%
  mutate(across(.cols=c(1, 2, 3), ~ (.x/261)*255))

reliefraster_df <- as.data.frame(reliefraster_spdf) %>% 
  mutate(cols = rgb(NE1_50M_SR_W.1,   
                    NE1_50M_SR_W.2, 
                    NE1_50M_SR_W.3, 
                    maxColorValue = 255)) %>%
  dplyr::select(x, y, cols)

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

#landraster <- rasterize(sp_land, emptyraster)
#searaster <- rasterize(sea_crop_rp, emptyraster)
searaster <- raster("shiny/assets/searaster.grd")
landraster <- raster("shiny/assets/landraster.grd")

demcrop <- raster::crop(dem, extent(europemap))
dem_rp <- projectRaster(demcrop, res = 50000, crs = crs.laea)

crs(demcrop) <- crs.wgs84
dem_lim <- dem_rp
dem_lim[dem_lim <1000] <- NA
dem_lim[dem_lim>=1000] <- 1
origin(dem_lim) <- origin(landraster)
testraster <- landraster+dem_lim
dem_lim <- testraster
dem_lim[!is.na(dem_lim)] <- 1
writeRaster(dem_lim,filename="shiny/assets/mountainraster.grd", overwrite = TRUE)




riverraster <- rasterize(sp_rivers, emptyraster)
riverraster[!is.na(riverraster)] <- 1


hexcrop_centroids <- st_centroid(hexcrop) %>%
  as(Class = "Spatial")

# SAVE FILE
st_write(hexcrop, "shiny/assets/hexcrop.shp", append=FALSE)

st_write(europeland, "shiny/assets/europeland.shp", append=FALSE)
st_write(europe_rivers, "shiny/assets/europe_rivers.shp", append=FALSE)

#PROTECTED writeRaster(landraster,filename="shiny/assets/landraster.grd", overwrite = TRUE)
writeRaster(riverraster,filename="shiny/assets/riverraster.grd", overwrite = TRUE)
#writeRaster(relief_rp,filename="shiny/assets/reliefraster.grd", overwrite = TRUE)
save(reliefraster_df, file="shiny/assets/reliefraster_df.RData")


#PROTECTED writeRaster(searaster,filename="shiny/assets/searaster.grd", overwrite = TRUE)


st_write(hexcrop, "shiny/assets/hexcrop.shp", append=FALSE)
save(hexcrop_centroids, file="shiny/assets/hexcrop_centroids.shp")


### EXAMPLE CITIES
examplecities <- read_excel("examplecities.xlsx")
cities <- SpatialPointsDataFrame(coords = examplecities[,c("long", "lat")],
                                 data = examplecities[,"city"],
                                                      proj4string = crs.wgs84)
cities <- spTransform(cities, crs.laea)
save(cities, file="shiny/assets/examplecities.shp")

