# R script to collect google trends data and create animation shown here:
# https://twitter.com/sundellviz/status/1442086387759599618
# and here:
# https://www.reddit.com/r/dataisbeautiful/comments/pvry82/oc_google_search_interest_on_covid19_throughout/
# Made by Anders Sundell


rm(list=ls(all=TRUE))

library(tidyverse)
library(gtrendsR)
library(tmap)
library(rgdal)
library(viridis)
library(raster)
library(sp)
library(gifski)
library(imputeTS)
library(slider)
library(grid)
library(gridExtra)

# Load Gtrends country data
data(countries)
gtrends_countries <- unique(countries$country_code)

# Load natural earth country data
countries <- readOGR(dsn = "Natural Earth/ne_50m_admin_0_countries/", layer = "ne_50m_admin_0_countries")

# Load Our world in data covid dataset
covid <- read_csv("owid-covid-data.csv")

# Manipulate covid data
continents <- covid %>%
  group_by(date, continent) %>%
  summarize(new_cases = sum(new_cases, na.rm=TRUE),
            new_deaths = sum(new_deaths, na.rm=TRUE),
            population = sum(population, na.rm=TRUE)) %>%
  mutate(deaths_million = new_deaths / (population/1000000)) %>%
  group_by(continent) %>%
  mutate(deaths_ma = slider::slide_dbl(deaths_million, mean, .before = 6, .after = 0)) %>%
  ungroup()

continents <- continents %>%
  filter(!is.na(continents))

# This part collects search data
countries_slim <- countries[, "ISO_A2"]

countrylist <- unique(countries_slim$ISO_A2)

firstcountry <- gtrends_countries[[1]]

df <- gtrends(
  keyword = "%2Fg%2F11j2cc_qll",
  geo = firstcountry,
  time = "2019-12-01 2021-09-23",
  gprop = c("web"),
  category = 0,
  hl = "en-US",
  low_search_volume = TRUE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0,
  onlyInterest = TRUE)

store <- df[[1]]


gtrends_mod <- gtrends_countries[!gtrends_countries %in% c("TP", "AN", "YU", "XZ")]
gtrends_mod <- append(gtrends_mod, c("NM"), after=length(gtrends_mod))

for (cname in gtrends_mod[2:length(gtrends_mod)]){
print(cname)
  df <- gtrends(
  keyword = "%2Fg%2F11j2cc_qll",
  geo = cname,
  time = "2019-12-01 2021-09-23",
  gprop = c("web"),
  category = 0,
  hl = "en-US",
  low_search_volume = TRUE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0,
  onlyInterest = TRUE)

Sys.sleep(runif(n=1))

temp <- df[[1]]
store <- rbind(store, temp)
}

#write_csv(store, "coronahits.csv")
#store <- read_csv("coronahits.csv")

store <- store %>%
  mutate(day = lubridate::date(date),
         hitsnum = as.double(hits))

# Line graph to check search data
store %>%
  filter(geo %in% c("ZM")) %>%
ggplot(aes(x=as.Date(date), y=hitsnum, group=geo, col=geo)) +
  geom_line()


crs.wgs84 <- CRS("+init=EPSG:4326")
crs.mercator <- CRS("+init=EPSG:3857")
#crs.mollweide <- CRS("+init=EPSG:54009")

# Minor corrections in geographical data, reproject to Mercator
countries$ISO_A2[countries$ADMIN=="France"] <- "FR"
countries$ISO_A2[countries$ADMIN=="Norway"] <- "NO"
countries$ISO_A2[countries$ADMIN=="Somaliland"] <- "SO"
countries_slim <- countries[countries$SOVEREIGNT!="Antarctica", "ISO_A2"]
countries_slim <- countries_slim[countries_slim$ISO_A2!=-99,]
crs(countries_slim) <- crs.wgs84
countries_rp <- spTransform(countries_slim, crs.mercator)

store <- store %>%
  mutate(hitsnum2 = case_when(is.na(hitsnum) ~ 0,
                              TRUE ~ hitsnum))

# This part interpolates weekly data to daily
clist <- unique(store$geo)

fullstore <- data.frame(date = as.Date(seq(0,660), origin = "2019-12-01"),
                       geo = rep(clist[1], 661))

for (i in 2:length(clist)){
  temp <- data.frame(date = as.Date(seq(0,660), origin = "2019-12-01"),
                     geo = rep(clist[i], 661))
  fullstore <- rbind(fullstore, temp)
}

storetemp <- store %>%
  mutate(date = day+3)

fullstore <- left_join(fullstore, storetemp, by=c("geo", "date")) %>%
  dplyr::select(date, geo, hitsnum2)

fullstore <- fullstore %>%
  group_by(geo) %>%
  mutate(ipol = na.interpolation(hitsnum2, option = "linear")) %>%
  ungroup()

daylist <- unique(fullstore$date)



# This part loops through all days and creates maps as png
for (dayloop in daylist[32:661]){
testday <- fullstore %>%
  filter(as.character(date)==as.character(as.Date(dayloop, origin="1970-01-01"))) %>%
  dplyr::select("ISO_A2" = geo, ipol)

testmap2 <- merge(countries_rp, testday, by="ISO_A2")
filestring <- paste0("gif/png_", as.character(as.Date(dayloop, origin="1970-01-01")), ".png")

breaklist <- seq(0, 100, by=5)
map <- tm_shape(testmap2) +
  tm_fill("ipol", style="fixed", breaks=breaklist, palette = viridis(n = 20, option="magma"),
          colorNA = "#111111", title = "") +
  tm_layout(bg.color="#222222", asp = (16/9), inner.margins = 0, outer.margins = 0,
            legend.position = c("left", "bottom"), legend.text.color = "white",
            legend.text.size = 0.3,
            title.color = "white",
            legend.show = FALSE)

# BOTTOMGRAPH
linegraph <- continents %>%
  filter(date<=dayloop) %>%
  ggplot(aes(x=date, y=deaths_ma, group=continent, col=continent)) +
  geom_line() +
  geom_point(data=filter(continents, date==dayloop), aes(x=date, y=deaths_ma), size=0.6) +
  geom_text(data=filter(continents, date==dayloop), aes(x=date, y=deaths_ma, label=continent), hjust=0, nudge_x=3, size=2) +
  theme_minimal() +
  theme(plot.background = element_rect(fill="#222222"),
        panel.grid = element_blank(),
        axis.text = element_text(color="white", size=4),
        legend.position = "none",
        aspect.ratio = (1.5/12)) +
  lims(x=c(as.Date("2020-01-01"), as.Date("2021-10-01")),
       y=c(0, 12.5)) +
  labs(x="", y="")

linegraph

titletext <- as.character(as.Date(dayloop, origin="1970-01-01"))
subtitletext <- str_wrap("Brighter colors indicate higher search interest for the topic Covid-19,\n
relative to the average for the country. Source: Google Trends.\n
@sundellviz", width=44)

vp1 <- viewport(x=0, y=1, width=1, height=(9/12), just=c("left", "top"))
vp2 <- viewport(x=0, y=(3/12), width=1, height=(3/12), just=c("left", "top"))
png(filename = filestring, width=16, height=12, units = "cm", res=200)
print(map, vp=vp1)
print(linegraph, vp=vp2)
grid.text(x=0.1, y=(2.8/12), label="Deaths per million (seven day average)",
          just = c("left", "top"), gp=gpar(fontsize=6, col="white"))


#grid.rect(x=0, y=1, width=1, height=(2/12),
          #gp=gpar(fill="#222222", col="#222222"))

grid.text(x=0.05, y=0.52, label=
paste0(titletext, "\n
Google search intensity\n
for Covid-19"),
just = c("left", "top"), gp=gpar(fontsize=10, col="white", lineheight=0.5))

grid.text(x=0.05, y=0.41, label=
subtitletext,
          just = c("left", "top"), gp=gpar(fontsize=6, col="white", lineheight=0.9))


dev.off()

}


# Create a gif of all pngs. The mpg posted on reddit and twitter was however made from
# pngs in Camtasia where final notations were added.
png_files <- list.files("gif", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", width = 1600, height = 1200, delay = 0.1)
