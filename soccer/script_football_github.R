rm(list=ls(all=TRUE))
# Set working directory

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
#library(stars)
library(nngeo)
library(viridis)
library(GISTools)
library(extrafont)


crs.wgs84 <- CRS("+init=EPSG:4326")
crs.laea <- CRS("+init=EPSG:3035")

# Load country data - download it from naturalearthdata.com and put it in the folder.
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip
countries <- readOGR(dsn="ne_50m_admin_0_countries/", layer="ne_50m_admin_0_countries")

# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_breakaway_disputed_areas.zip
disp <- readOGR(dsn="ne_50m_admin_0_breakaway_disputed_areas/", layer="ne_50m_admin_0_breakaway_disputed_areas")
crimea <- disp[disp$NAME=="Crimea",]

countrydata <- as.data.frame(countries)

crs(crimea) <- crs.wgs84
crs(countries) <- crs.wgs84
countriescrop <- gDifference(countries, crimea, byid=TRUE)
countriescrop <- SpatialPolygonsDataFrame(countriescrop, data = countrydata, match.ID = FALSE)



# This part gets data on soccer fields from Open Street Map
# ---------------------------------------------------------
# Create grid
r <- raster(ext = extent(-12.2,47.3,27.6,71.6), res=c(8,6))
p <- rasterToPolygons(r) 

all <- raster(ext = extent(-12.2,47.3,27.6,71.6), res=c(2,2))
all_p <- rasterToPolygons(all) 
all_rp <- spTransform(all_p, crs.laea)

ngrids <- nrow(p)


soccerstore <- list()
# Loop over grid - can be divided in parts, takes a long time
for (i in 1:49) {
  print(i)
  bb <- as.vector(bbox(p[i,]))
 
q_soccer <- bb %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "sport", value = "soccer")

soccer <- osmdata_sp(q_soccer)
soccer_osm <- soccer$osm_points
if(nrow(soccer_osm)>0){
soccerstore[[i]] <- soccer_osm
}
}


# Gather up everything in one file
alltemp <- SpatialPoints(soccerstore[[1]])
for (i in 1:49){
  if(is.null(soccerstore[[i]])==FALSE){
  alltemp <- rbind(alltemp, SpatialPoints(soccerstore[[i]]))
  }
}

# This part saves the collected football field data - but it can just be loaded from the file
#saveRDS(alltemp, "allstore_good.RDS")
alltemp <- readRDS("allstore_good.RDS")

# Reproject fields and grid to better projection for Europe
crs(alltemp) <- crs.wgs84
alltemp_rp <- spTransform(alltemp, crs.laea)
crs(all_p) <- crs.wgs84


# Load population data, crop, reproject
# The raster file has to be unpacked from the zip file in the directory.
pop <- raster("popcount25.tif")
crop <- extent(-12.2,47.3,27.6,71.6)
popcrop <- crop(pop, crop)
crs(popcrop) <- crs.wgs84
testproj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
                        +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
popcrop_rp <- projectRaster(popcrop, crs=testproj)

# Merge Europe with Crimea, reproject
europecrop <- crop(countriescrop, crop)
europecrop <- europecrop[,"SOVEREIGNT"]
crimea$SOVEREIGNT <- "Ukraine"
crimea <- crimea[, "SOVEREIGNT"]

europecrop <- rbind(europecrop, crimea)
europecrop_good <- gUnaryUnion(europecrop, id = europecrop@data$SOVEREIGNT)

europecrop <- spTransform(europecrop_good, crs.laea)

# Create hexagonal grid
hexpts <-spsample(all_rp, type="hexagonal", cellsize=50000)
hexpols <- HexPoints2SpatialPolygons(hexpts)
crs(hexpols) <- crs.laea

# Cut hexagons to coastline
alleurope <- gUnaryUnion(europecrop)
hexpols1 <- gIntersection(hexpols, alleurope, byid=TRUE)

# Get population for each hexagon
test1 <- raster::extract(popcrop_rp, hexpols1, fun=sum, na.rm=TRUE)
hexpols1$pop <- as.numeric(test1)

# Count number of fields per hexagon
fieldscount <- sp::over(hexpols1, alltemp_rp, returnList = TRUE)
nrfields <- sapply(fieldscount, length)
hexpols1$nrfields <- nrfields

# Generate key statistics per each hexagon
hexpols1$fieldspop <- (hexpols1$nrfields/4) / (hexpols1$pop/1000)
hexpols1$fieldspop[is.na(hexpols1$fieldspop)] <- 0
hexpols1$fieldspop[hexpols1$fieldspop==Inf] <- 0

# This part saves the results, but they can also be loaded directly
saveRDS(hexpols1, "hexpols.RDS")
saveRDS(europecrop, "europecrop.RDS")

hexpols1 <- readRDS("hexpols.RDS")
europecrop <- readRDS("europecrop.RDS")

# Map-making part
# ---------------------------------------------------------------

# Create breaks
breaklist <- c(-0.01, 0.01, 0.2, 0.4, 0.6, 0.8, 1, 2, 3, 5, Inf)
nrbreaks <- length(breaklist)

# Create new variable according to breaks (this is to make the legend look nicer)
hexpols2 <-as.data.frame(hexpols1) %>%
  mutate(fieldspopcut = cut(fieldspop, 
                      breaks = breaklist,
                      labels = c("0.0", "0.0 - 0.2", "0.2 - 0.4", "0.4 - 0.6",
                                 "0.6 - 0.8", "0.8 - 1.0", "1.0 - 2.0", "2.0 - 3.0", "3.0 - 5.0", "5.0 -")))
hexpols1$fieldscut <- hexpols2$fieldspopcut


# Create map
soccermap <- tm_shape(hexpols1) +
  tm_fill("fieldscut", palette=inferno(n=nrbreaks)) +
  tm_borders(lwd=0, col=NA) +
  tm_shape(europecrop) +
  tm_borders(col="white", lwd=0.3, alpha=0.8) +
  tm_layout(bg.color="black", legend.position = c(0.03, 0.67), legend.text.color = "#dddddd",
            outer.margins=0, asp=1)


# Create png file with titles and text
bottomvp <- viewport(x=0, y=9.5/10, width=1, height=1, just=c("left", "top"))
png("soccertiles.png", width=10, height=10, res=300, units="in", bg="black")
print(soccermap, vp=bottomvp)

grid.text("Number of soccer fields\nper 1000 inhabitants", x=0.03, y=0.97,
          gp=gpar(col="white", fontsize=36, fontfamily="Georgia", lineheight=0.8), just=c("left", "top"))

grid.text(
"All land was divided into hexagonal grid cells, and the number of\n
fields as well as population was then calculated for each cell.\n
Brighter colors means more fields per inhabitant. All fields are\n
counted, not only those in stadiums or arenas. Data from\n
Open Street Map and NASA Earthdata.", x=0.03, y=0.87,
          gp=gpar(col="white", fontsize=10, lineheight=0.5), just=c("left", "top"))

grid.text(
"Most fields:\n
1. Germany\n
2. France\n
3. Italy\n
4. United Kingdom\n
5. Poland\n
\n
Most per inhabitant:\n
1. Liechtenstein\n
2. San Marino\n
3. Norway\n
4. Sweden\n
5. Denmark", x=0.03, y=0.6,
  gp=gpar(col="white", fontsize=10, lineheight=0.5), just=c("left", "top"))


grid.text("@sundellviz", x=0.98, y=0.02, gp=gpar(fontsize = 12, col="#dddddd"), just=c("right", "bottom"))

  dev.off()

  

# Calculate statistics by country
fieldscount_country <- sp::over(europecrop, alltemp_rp, returnList = TRUE)
nrfields_country <- sapply(fieldscount_country, length)

europecrop$nrfieldscountry <- nrfields_country/4
countrypop <- raster::extract(popcrop_rp, europecrop, fun=sum, na.rm=TRUE)
europecrop$pop <- round(as.numeric(countrypop)/1000000, 2)
europecrop$fieldscap <- europecrop$nrfieldscountry/(europecrop$pop*1000)

View(as.data.frame(europecrop))
