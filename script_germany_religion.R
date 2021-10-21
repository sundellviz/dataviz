# R script for making the maps displayed here: https://twitter.com/sundellviz/status/1451087831867211783
# Data on populated places for city labels: https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
# Municipality shapefiles: https://www.zensus2011.de/EN/Media/Background_material/Background_material_node.html
# Religion data: https://www.regionalstatistik.de/genesis//online?operation=table&code=12111-06-01-5&bypass=true&levelindex=0&levelid=1634847300251#abreadcrumb


rm(list=ls(all=TRUE))
# Set working directory
setwd("")
library(tidyverse)
library(rgdal)
library(rgeos)
library(tmap)
library(raster)
library(sf)
library(viridis)
library(extrafont)
font_import()
crs.wgs84 <- CRS("+init=EPSG:4326")
crs.utm32 <- CRS("+init=EPSG:25832")

# Load natural earth data
popplaces <- readOGR(dsn = "../Natural Earth/ne_10m_populated_places/", layer = "ne_10m_populated_places")
germanyplaces <- popplaces[popplaces$ADM0NAME=="Germany",]
germanyplaces_millionplus <- germanyplaces[germanyplaces$POP_MAX>1000000,]
  
plot(germanyplaces_millionplus)
crs(germanyplaces_millionplus) <- crs.wgs84
germanyplaces_millionplus_rp <- spTransform(germanyplaces_millionplus, crs.utm32)

# Municipalities shapefile
municipalities <- readOGR(dsn = "VG250_1Jan2011_UTM32", layer="VG250_Gemeinden", encoding = "utf-8")
municipalities$geocode <- municipalities$AGS
municipalities$geocode_num <- as.numeric(municipalities$AGS)

#crs(municipalities) <- crs.wgs84
proj4string(municipalities)

religion <- read_delim("religion_flat.csv", delim=";") %>%
  mutate(codechars = str_length(`1_Auspraegung_Code`),
         geocode_num = as.numeric(`1_Auspraegung_Code`)) %>%
  dplyr::filter(codechars>5, !is.na(`BEVZ20__Bevoelkerung__Anzahl`))
religion_kreise <- read_delim("religion_flat_kreise.csv", delim=";") %>%
  mutate(codechars = str_length(`1_Auspraegung_Code`),
         geocode_num = case_when(codechars==2 ~ as.numeric(`1_Auspraegung_Code`)*1000000,
                                 codechars==5 ~ as.numeric(`1_Auspraegung_Code`)*1000,
                                 codechars==5 ~ as.numeric(`1_Auspraegung_Code`))) %>%
  filter(!`1_Auspraegung_Code` %in% c("03241001", "05334002", "10041100"))
religion <- rbind(religion, religion_kreise)

religion_slim <- religion %>%
  dplyr::select("geocode" = "1_Auspraegung_Code", "label" = "1_Auspraegung_Label",
                "varcode" = "2_Auspraegung_Code", "population" = "BEVZ20__Bevoelkerung__Anzahl",
                "varlabel" = "2_Auspraegung_Label", geocode_num) %>%
  mutate(codechars = str_length(geocode))



religion_wide <- religion_slim %>%
  filter(varcode %in% c("RELIGION01", "RELIGION02", "RELIGION03") | varlabel=="Insgesamt") %>%
  mutate(varcode = case_when(varlabel=="Insgesamt" ~ "TOTAL",
                             TRUE ~ varcode)) %>%
  dplyr::filter(population!="-") %>%
  dplyr::select(-varlabel) %>%
  pivot_wider(names_from=varcode, values_from=population) %>%
  mutate(across(c(RELIGION01, RELIGION02, RELIGION03, TOTAL), ~as.numeric(.x)),
         across(c(RELIGION01, RELIGION02, RELIGION03, TOTAL), ~replace_na(.x, 0)),
         perc_catholic = (RELIGION01/TOTAL)*100,
         perc_evangelical = (RELIGION02/TOTAL)*100,
         perc_none = (RELIGION03/TOTAL)*100,
         perc_diff = perc_evangelical-perc_catholic)



### LOOK FOR DUPLICATES
test <- religion_wide %>%
  group_by(`geocode`) %>%
  summarize(n=n())


# MERGE
#lau_germany$geocode <- as.numeric(lau_germany$LAU_ID)
#testmerge <- merge(lau_germany, religion_wide, by="geocode")
munimerge <- merge(municipalities, religion_wide, by="geocode_num")


religion_wide %>%
  ggplot(aes(x=perc_none, y=perc_catholic)) +
  geom_point()

munimerge$largest <- as.numeric(munimerge$perc_diff>0)

crs(munimerge) <- crs.utm32
crs.lamberteurope <- CRS("+init=EPSG:3034")
munimerge_rp <- spTransform(munimerge, crs.lamberteurope)

help(sptransform)
breaklist <- seq(0, 100, by=10)

# Make minimaps
evangelical <- tm_shape(munimerge) +
  tm_fill("perc_evangelical", style="fixed", breaks=breaklist, palette = viridis(10),
          colorNA = "#777777", title = "") +
  tm_shape(germanyplaces_millionplus_rp) +
  tm_text("NAME", col="white", size=0.8, alpha=0.5) +
  tm_layout(title="% Protestant", frame = FALSE)

catholical <- tm_shape(munimerge) +
  tm_fill("perc_catholic", style="fixed", breaks=breaklist, palette = viridis(10),
          colorNA = "#777777", title = "") +
  tm_text("GEN", col="white", size=0.2, alpha=0.5) +
  tm_shape(germanyplaces_millionplus_rp) +
  tm_text("NAME", col="white", size=0.8, alpha=0.5) +
          tm_layout(title="% Catholic", frame = FALSE)

other <- tm_shape(munimerge) +
  tm_fill("perc_none", style="fixed", breaks=breaklist, palette = viridis(10),
          colorNA = "#777777", title = "") +
  tm_shape(germanyplaces_millionplus_rp) +
  tm_text("NAME", col="white", size=0.8, alpha=0.5) +
          tm_layout(title="% Other and unaffiliated", frame = FALSE)

notationcol <- "#333333"

png("germany_all.png", width=16, height=9, units="in", res=300)
tmap_arrange(evangelical, catholical, other, ncol=3, nrow=1)
grid.text("Religious affiliation in German municipalities, 2011 census",
          x=0.5, y=0.08, just=c("centre", "bottom"),
          gp=gpar(fontsize=32, fontfamily="Georgia"))

grid.text("Municipal shapefile data from the 2011 Census (zensus2011.de). Information on religious affiliation from the Statische Ämter\n
Des Bundes Und Der Länder (regionalstatistik.de). Missing areas are mostly uninhabitated. Made in R with the tmap package.",
          x=0.5, y=0.065, just=c("centre", "top"),
          gp=gpar(fontsize=14, lineheight=0.5))

grid.text("@sundellviz",
          x=0.98, y=0.02, just=c("right", "bottom"),
          gp=gpar(fontsize=14))

grid.curve(x2=0.845, y2=0.775, x1=0.87, y1=0.88, default.units="npc",
           gp=gpar(col=notationcol, lwd=2), 
           curvature=arcCurvature(90), ncp=8, square=FALSE, arrow=arrow(length=unit(0.005, "npc"), ends="last", type="open",))
grid.text(
  "Border between former\nEast and West Germany",
  x=0.875, y=0.88, gp=gpar(col=notationcol, fontsize=12, lineheight=0.9), just=c("left", "center"))

dev.off()

png("germany_protestant.png", width=9, height=12, units="in", res=300)
evangelical
dev.off()
glimpse(popplaces_)

png("germany_largestredblue.png", width=8, height=10, units="in", res=300)
tm_shape(munimerge_rp) +
  tm_fill("largest", n=2, palette = c("#68a4f3", "#bf5055"),
          colorNA = "#777777")
dev.off()
