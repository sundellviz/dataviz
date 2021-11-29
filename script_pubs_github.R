# SCRIPT FOR CREATING DOT MAP OF CAFES, PUBS, BARS AND BIERGARTENS
# https://twitter.com/sundellviz/status/1465244018955231234
# https://www.reddit.com/r/dataisbeautiful/comments/r4rmvn/oc_where_to_get_a_drink_in_europe_all_the_bars/

rm(list=ls(all=TRUE))
setwd("")
library(tidyverse)
library(grid)
library(gridExtra)
library(osmdata)
library(sf)
library(sp)
library(tmap)
library(raster)
library(rgdal)

crs.wgs84 <- CRS("+init=EPSG:4326")

### START LOOP HERE
# CREATE GRID
r <- raster(ext = extent(-12.2,47.3,27.6,71.6), res=c(8,6))
p <- rasterToPolygons(r) 

### NGRIDS
ngrids <- nrow(p)

# FOR STORING
cafestore <- list()
barstore <- list()
pubstore <- list()
bierstore <- list()
allstore <- list()

#### START LOOP HERE - LOOP OVER GRID CELLS (MIGHT BE AN ERROR IN GRID 13)####
for (i in 1:ngrids) {
  print(i)
  bb <- as.vector(bbox(p[i,]))

q_cafe <- bb %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "cafe")

q_bar <- bb %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "bar")

q_pub <- bb %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "pub")

#q_restaurant <- bb %>% 
#  opq (timeout = 25*100) %>%
#  add_osm_feature("amenity", "restaurant")

q_biergarten <- bb %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "biergarten")

cafe <- osmdata_sp(q_cafe)
cafe_osm <- cafe$osm_points
if(nrow(cafe_osm)>0){
cafe_clean <- cafe_osm[, "amenity"]
cafe_clean <- cafe_clean[!is.na(cafe_clean$amenity) & cafe_clean$amenity=="cafe",]
cafestore[[i]] <- cafe_clean
}

pub <- osmdata_sp(q_pub)
pub_osm <- pub$osm_points
if(nrow(pub_osm)>0){
pub_clean <- pub_osm[, "amenity"]
pub_clean <- pub_clean[!is.na(pub_clean$amenity) & pub_clean$amenity=="pub",]
pubstore[[i]] <- pub_clean
}

bar <- osmdata_sp(q_bar)
bar_osm <- bar$osm_points
if(nrow(bar_osm)>0){
bar_clean <- bar_osm[, "amenity"]
bar_clean <- bar_clean[!is.na(bar_clean$amenity) & bar_clean$amenity=="bar",]
barstore[[i]] <- bar_clean
}

bier <- osmdata_sp(q_biergarten)
bier_osm <- bier$osm_points
if(nrow(bier_osm)>0){
bier_clean <- bier_osm[, "amenity"]
bier_clean <- bier_clean[!is.na(bier_clean$amenity) & bier_clean$amenity=="biergarten",]
bierstore[[i]] <- bier_clean
}
}

### GATHER UP ALL THE GRID CELLS INTO ONE (1-3 are empty)
alltemp <- rbind(cafestore[[4]])
alltemp <- rbind(alltemp, pubstore[[4]])
alltemp <- rbind(alltemp, barstore[[4]])
for (i in 5:49){
  if(is.null(cafestore[[i]])==FALSE){
  alltemp <- rbind(alltemp, cafestore[[i]])
  }
  if(is.null(pubstore[[i]])==FALSE){
    alltemp <- rbind(alltemp, pubstore[[i]])
  }
  if(is.null(barstore[[i]])==FALSE){
    alltemp <- rbind(alltemp, barstore[[i]])
  }
  if(is.null(bierstore[[i]])==FALSE){
    alltemp <- rbind(alltemp, bierstore[[i]])
  }
}

# OWN COLOR PALETTE
ownpal <- c("#D81B60", "#1E88E5", "#FFC107", "#11ad38")

# REPROJECT TO GOOD PROJECTION FOR EUROPE
crs(alltemp) <- crs.wgs84
crs.laea <- CRS("+init=EPSG:3035")
alltemp_rp <- spTransform(alltemp, crs.laea)

# PLOT
png("europe_points_smaller_black.png", width=20, height=20, units="in", res=300)
tm_shape(alltemp_rp) +
  tm_dots("amenity", palette=ownpal, size=0.01, border.lwd=0, alpha=0.8, legend.show=FALSE) +
  tm_layout(frame=FALSE, bg.color = "black",
            legend.show = TRUE, legend.text.color = "white", legend.text.size=2, legend.position = c(0.02, 0.98), legend.just = c("left", "top")) +
tm_add_legend(type = "symbol", 
              col = ownpal,
              labels = c("Bar", "Biergarten", "Cafe", "Pub"),
              title = "", size=2, shape=19)

grid.text("Sells alcoholic drinks to be consumed on the premises,\ncharacterised by a noisy and vibrant atmosphere and usually don't sell food.",
          x = 0.15, y=0.915, gp=gpar(col="#aaaaaa", lineheight=0.8), just=c("left", "top"))
grid.text("Open-air area where alcoholic beverages along\nwith food is prepared and served.",
          x = 0.15, y=0.895, gp=gpar(col="#aaaaaa", lineheight=0.8), just=c("left", "top"))
grid.text("Informal place that offers casual meals and beverages;\ntypically, the focus is on coffee or tea.",
          x = 0.15, y=0.875, gp=gpar(col="#aaaaaa", lineheight=0.8), just=c("left", "top"))
grid.text("Place selling beer and other alcoholic drinks;\nmay also provide food or accommodation.",
          x = 0.15, y=0.855, gp=gpar(col="#aaaaaa", lineheight=0.8), just=c("left", "top"))
grid.text("@sundellviz",
          x = 0.98, y=0.05, gp=gpar(col="#aaaaaa", lineheight=0.8, fontsize=18), just=c("right", "bottom"))
dev.off()

