# R CODE FOR CREATING MAP ANIMATION OF CITIES CONQUERING EACH OTHER
# By Anders Sundell
# twitter.com/sundellviz
# As seen on https://www.reddit.com/r/dataisbeautiful/comments/ofihx1/oc_simulation_where_larger_european_cities/

# Preparations: Download the "popplaces" and "land" shapefiles from naturalearthdata.com. I used the 10m resolution version for populated places and 50m for land.
# Create an folder called "output_europe" and in it subfolders called "game1" "game2" and so on, depending on how many games you want to run.

rm(list = ls(all=TRUE))
library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(dismo)
library(gridExtra)
library(tmap)
library(grid)
library(sp)
library(geosphere)
library(maptools)
library(extrafont)
library(randomcoloR)
font_import()

# Enter path to project folder
setwd("")
land <- readOGR(dsn = "../Natural Earth/ne_50m_land/", layer = "ne_50m_land")
popplaces <- readOGR(dsn = "../Natural Earth/ne_10m_populated_places/", layer = "ne_10m_populated_places")
bigcities <- popplaces[popplaces$POP_MAX >= 10000,]
bigcities <- bigcities[order(bigcities$POP_MAX),]

# Projections
crs.wgs84 <- CRS("+init=EPSG:4326")
crs.mercator <- CRS("+init=EPSG:3857")
crs.laea <- CRS("+init=EPSG:3035")

crs(land) <- crs.wgs84
crs(bigcities) <- crs.wgs84

# Bounding boxes
bbox_europe <- c(-50, 80, 20, 90)
bbox_everything <- c(-180, 180, -60, 72)
bbox_laea <- c(2200000, 8000000, 1200000, 5400000)
laea_world <- spTransform(land, crs.laea)

# Crop Europe in LAEA projection
europecrop <- raster::crop(laea_world, bbox_laea)
bigcities_laea <- spTransform(bigcities, crs.laea)
bigcitiescrop <- raster::crop(bigcities_laea, bbox_laea)

bigcitiescrop$cityid <- seq(1:nrow(bigcitiescrop))
bigcitiescrop$ownerid <- seq(1:nrow(bigcitiescrop))
bigcitiescrop$ownername <- bigcitiescrop$NAMEASCII
bigcitiescrop$pop <- as.double(bigcitiescrop$POP_MAX)
bigcitiescrop$nrareas <- as.double(1)
bigcitiescrop <- bigcitiescrop[,c("cityid", "ownerid", "ownername", "pop", "nrareas")]

imax <- nrow(bigcitiescrop)

cityvoronoi <- voronoi(bigcitiescrop)
cityvoronoi <- cityvoronoi[,c("cityid", "ownerid", "ownername", "pop", "nrareas")]

voronoi_cities <- raster::intersect(cityvoronoi, europecrop)
voronoi_grouped <- aggregate(voronoi_cities, by="cityid")
voronoi_grouped <- voronoi_grouped[order(voronoi_grouped$cityid),]
cityvoronoidata <- as.data.frame(cityvoronoi)

# Palette
ncolors <- 100
randompal <- distinctColorPalette(100)

# START LOOP FROM HERE
for (game in 1:10){

cityvoronoi <- SpatialPolygonsDataFrame(voronoi_grouped, cityvoronoidata, match.ID = FALSE)
cityvoronoi$mapcolor <- cityvoronoi$ownerid %% ncolors + 1

currentcrop <- bigcitiescrop
storage <- as.data.frame(cityvoronoi) %>%
  mutate(round = 0) %>%
  arrange(desc(pop)) %>%
  slice_head(n=25)

for (i in 1:(imax-1)){
currentcity <- sample(currentcrop$cityid, 1)
currentcityname <- currentcrop$ownername[currentcrop$cityid==currentcity]
nrcities <- nrow(currentcrop)
currentcrop_longlat <- spTransform(currentcrop, crs.wgs84)
temp <- currentcrop %>% as.data.frame() %>%
  mutate(rownumber = seq(1:nrow(currentcrop))) %>%
  filter(cityid==currentcity)
rowid <- temp$rownumber
currentcityvarname <- paste0("V", rowid)

# Identify closest city
distmatrix <- as.data.frame(distm(currentcrop_longlat[currentcrop_longlat$cityid==currentcity,], currentcrop_longlat))
distmatrix <- distmatrix %>%
  dplyr::select(!matches(currentcityvarname))
colnumber <- as.double(which.min(apply(distmatrix,MARGIN=2,min)))

colid <- sub('.', '', colnames(distmatrix[colnumber])) %>%
  as.integer()
closestcityid <- currentcrop$cityid[colid]
closestcityname <- currentcrop$ownername[currentcrop$cityid==closestcityid]

# Compare populations
currentcitypop <- currentcrop$pop[currentcrop$cityid==currentcity]
closestcitypop <- currentcrop$pop[currentcrop$cityid==closestcityid]

if(currentcitypop>=closestcitypop){
  winnerid <- currentcity
  winnername <- currentcityname
  loserid <- closestcityid
} else{
  winnerid <- closestcityid
  winnername <- closestcityname
  loserid <- currentcity
}

# Add current city population to conqueror
currentcrop$pop[currentcrop$cityid==winnerid] <- currentcitypop+closestcitypop
cityvoronoi$pop[cityvoronoi$cityid==winnerid] <- currentcitypop+closestcitypop
cityvoronoi$nrareas[cityvoronoi$cityid==winnerid] <- cityvoronoi$nrareas[cityvoronoi$cityid==winnerid] + 1

# Get owner data for new polygon
ownerdata <- as.data.frame(cityvoronoi[cityvoronoi$cityid==winnerid,])

# Change ownership of loser
cityvoronoi$ownerid[cityvoronoi$ownerid==loserid] <- winnerid

newpoly <- aggregate(cityvoronoi[cityvoronoi$ownerid==winnerid,])
cityvoronoi <- cityvoronoi[cityvoronoi$ownerid!=winnerid,]
newpoly <- SpatialPolygonsDataFrame(newpoly, ownerdata, match.ID = FALSE)
cityvoronoi <- rbind(cityvoronoi, newpoly)

# Remove loser
currentcrop <- currentcrop[currentcrop$cityid!=loserid,]

tempstore <- as.data.frame(cityvoronoi) %>%
  mutate(round = i) %>%
  arrange(desc(pop)) %>%
  slice_head(n=25)
storage <- rbind(storage, tempstore)

# Make map
mapoutput <-  tm_shape(cityvoronoi) +
  tm_borders(col="black") +
  tm_text("ownername", size = "AREA", alpha = 1, col="black", scale=2.2,
          print.tiny = FALSE, size.lowerbound=0.2, fontfamily="Georgia") +
  tm_fill("mapcolor", palette = randompal, alpha=0.4, n=ncolors, style="fixed", breaks=seq(0:ncolors)) +
  tm_shape(europecrop) +
  tm_borders() +
  tm_shape(currentcrop) +
  tm_dots(size=0.06, alpha=0.5) +
  tm_layout(legend.show=FALSE, inner.margins=c(0, 0, 0, 0), outer.margins=c(0, 0, 0, 0), frame = FALSE)

if(i==1 | (i<1300 & i %%50==0) | (i>=1300)){
  png(paste0("output_europe/game", game, "/", i, ".png"), width=1920, height=1080)
  mapvp <- viewport(x=1, y=1, width=0.744, height=1, just=c("right", "top"))
  print(mapoutput, vp=mapvp)
  grid.text("City struggle", x=0.02, y=0.96,
            gp=gpar(fontfamily="Georgia", fontsize=64), just = c("left", "top"))
  
  grid.text(paste0(
"All land is initially divided so that each\n
piece belongs to the closest city with at least\n
10,000 inhabitants. The cities then play a game. \n
\n
Rules of the game:\n
1. A city is chosen at random and paired with its\n
     closest neighboring city.\n
2. The population of the two cities is compared.\n
3. The city with the largest population conquers the\n
     other city, and takes over its population and territory.\n
4. Repeat until only one city remains.\n
\n
City data is taken from naturalearthdata.com and\n
might not be exactly accurate - please take it\n
with a grain of salt. The end result is affected\n
by randomness and therefore varies every time\n
the game is played.\n
\n"),
    x=0.02, y=0.88,
    gp=gpar(fontfamily="Georgia", fontsize=24, lineheight=0.5), just = c("left", "top"))

grid.text(paste0("Game ", game, ". Turn ", i, ". ", imax-i, " cities left.\n",
"Leaderboard: City and population count (millions)\n",
"1. ", tempstore$ownername[1], ": ", round(tempstore$pop[1]/1000000, 2), "\n",
"2. ", tempstore$ownername[2], ": ", round(tempstore$pop[2]/1000000, 2), "\n",
"3. ", tempstore$ownername[3], ": ", round(tempstore$pop[3]/1000000, 2), "\n",
"4. ", tempstore$ownername[4], ": ", round(tempstore$pop[4]/1000000, 2), "\n",
"5. ", tempstore$ownername[5], ": ", round(tempstore$pop[5]/1000000, 2), "\n",
"6. ", tempstore$ownername[6], ": ", round(tempstore$pop[6]/1000000, 2), "\n",
"7. ", tempstore$ownername[7], ": ", round(tempstore$pop[7]/1000000, 2), "\n",
"8. ", tempstore$ownername[8], ": ", round(tempstore$pop[8]/1000000, 2), "\n",
"9. ", tempstore$ownername[9], ": ", round(tempstore$pop[9]/1000000, 2), "\n",
"10. ", tempstore$ownername[10], ": ", round(tempstore$pop[9]/1000000, 2)),
  x=0.02, y=0.4,
  gp=gpar(fontfamily="Georgia", fontsize=24, lineheight=1), just = c("left", "top"))
  
grid.text("Twitter: @sundellviz",
  x=0.02, y=0.03,
  gp=gpar(fontfamily="Georgia", fontsize=24, lineheight=0.5), just = c("left", "bottom"))

  dev.off()
  print(i)
}
}
}
