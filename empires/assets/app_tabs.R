rm(list=ls(all=TRUE))
setwd("/Users/xsunde/Dropbox/Reddit/Empires/Shiny/assets")
library(tidyverse)
library(rgdal)
library(viridis)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(viridis)
library(rsconnect)
library(gdistance)

library(shiny)

# LOAD DATA
crs.wgs84 <- CRS("+init=EPSG:4326")
crs.laea <- CRS("+init=EPSG:3035")

# STUFF TO LOAD
# 1. hexcrop
# 2. land
# 3. rivers
# 4. sea

nrdynasties <- 15


hexcrop <- st_read("hexcrop.shp") %>%
  mutate(owner = 0,
         ownerfac = factor(0, levels = seq(from = 0, to=nrdynasties)),
         dynasty = as.character(NA))
load("hexmid.rData")

europeland <- st_read("europeland.shp")
europe_rivers <- st_read("europe_rivers.shp")

landraster <- raster("landraster.grd")
riverraster <- raster("riverraster.grd")
searaster <- raster("searaster.grd")
mountainraster <- raster("mountainraster.grd")
load(file = "reliefraster_df.RData")



load(file="hexcrop_centroids.shp")



dynastylist_europe <- c("Habsburg", "Hohenzollern", "Capet",
                 "Plantagenet", "Rurik", "Komnenos",
                 "Wittelsbach", "Arpad", "Oldenburg",
                 "Bourbon", "Romanov", "Anjou", "Piast",
                 "Savoy", "Vasa")

dynastylist <- dynastylist_europe

crs(landraster) <- crs.laea
crs(riverraster) <- crs.laea
crs(mountainraster) <- crs.laea
crs(searaster) <- crs.laea

combinedraster <- landraster
combinedraster[!is.na(landraster)] <- 10
combinedraster[!is.na(searaster)] <- 40
combinedraster[!is.na(mountainraster) & is.na(riverraster)] <- 5
combinedraster[is.na(mountainraster) & !is.na(riverraster)] <- 15
combinedraster[!is.na(mountainraster) & !is.na(riverraster)] <- 15*0.5 + 5*0.5
combinedraster <- as(combinedraster, "SpatialPixelsDataFrame") %>%
  as.data.frame()


#Empty select list



ui <- fluidPage(
  
  h1("Empire simulator"),
  
  p(str_wrap(width = 100,
    string =
"This is a tool to simulate the reach of empires, based on ease of traveling to different locations.
Place capital cities on the map and vary the parameters of travel speed across different terrains
to see how the reach of the 'empires' change. Different tabs show speed on different terrains,
as well as best travel routes between the capitals.")),

span("Made by Sundellviz:"), a("Youtube", href="https://www.youtube.com/sundellviz"),
span(" | "), a("Twitter", href="https://www.twitter.com/sundellviz"),
p(),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(width = 3,
    sliderInput(
      inputId = "landspeed",
      label = "Land travel speed",
      min = 1,
      max = 50,
      value = 10
    ),
    sliderInput(
      inputId = "seaspeed",
      label = "Sea travel speed",
      min = 1,
      max = 50,
      value = 40
    ),
    sliderInput(
      inputId = "riverspeed",
      label = "River travel speed",
      min = 1,
      max = 50,
      value = 15
    ),
    sliderInput(
      inputId = "mountainspeed",
      label = "Mountain travel speed",
      min = 1,
      max = 50,
      value = 5
    ),
    sliderInput(
      inputId = "changespeed",
      label = "Mode change speed",
      min = 1,
      max = 50,
      value = 5
    ),
    sliderInput(
      inputId = "empiresize",
      label = "Max empire size",
      min = 1,
      max = 1000,
      value = 300
    ),
    
    radioButtons(inputId = "empireselector",
                 label = "Capital selector:",
                 choiceNames = dynastylist,
                 choiceValues = seq(from = 1, to = 15),
                 inline = FALSE),
    
    textInput(inputId = "customnames", label = "Custom empire names", value = paste(dynastylist, collapse=",")),
    
    radioButtons(inputId = "hillshadeselector",
                 label = "Background:",
                 choiceNames = c("On", "Off"),
                 choiceValues = c("On", "Off"),
                 inline = TRUE),
    
    hr(),
    
    actionButton("reloadbutton", "Start over")
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 9,
      
      tabsetPanel(type = "tabs",
                  tabPanel("Empire", plotOutput(outputId = "zerosumplot", click = "plot_click", width="1000px", height="1000px")),
                  tabPanel("Speed", plotOutput(outputId = "speedPlot", click = "plot_click", width="1000px", height="1000px")),
                  tabPanel("Routes", plotOutput(outputId = "distPlot", click = "plot_click", width="1000px", height="1000px")),
                  tabPanel("Top list", tableOutput(outputId = "toplist"))
      )
      
      
    )
  ),
  
p(str_wrap(width = 100,
           string =
"The idea of the simulation is to see if the shape of states can be 'explained' by geographical factors.
Using the default configuration, place a capital on the site of Rome and increase empire size to see
something that looks a lot like the shape of the Roman Empire. Probably because the empire spread around
and was bound together by the Mediterranean, Mare Nostrum, 'Our sea'.")),


  
)

###############

server <- function(input, output, session){

  # Reload button
  observeEvent(input$reloadbutton, {
  session$reload()
  })
  
  
  
  hexselect <- eventReactive(input$plot_click, {
    as.double(nearPoints(rv$hexmid, input$plot_click, xvar = "X", yvar = "Y", maxpoints = 1)$hexnr)
  })
  

  emptypoint <- st_sf(id = 1:1, geometry = st_sfc(lapply(1:1, function(x) st_geometrycollection())))
  
  rv <- reactiveValues(hexcrop_rv = hexcrop,
                       raster_acc = data.frame(x=as.double(NA), y=as.double(NA), layer=as.double(1)),
                       c_hex = 500,
                       hexcrop_merged = hexcrop,
                       select_list = list(NULL, NULL, NULL, NULL, NULL, 
                                          NULL, NULL, NULL, NULL, NULL, 
                                          NULL, NULL, NULL, NULL, NULL),
                       empirevalue = as.numeric(1),
                       select_all_sf = as_tibble(999),
                       dynastylist = dynastylist,
                       hexmid = hexmid,
                       examplepaths = as_tibble(999),
                       combinedrasterplot = combinedraster)
  
  
  
  observeEvent(input$customnames,{ 
  templist <-  unlist(strsplit(input$customnames, ","))  
  if(length(templist)>0){
  rv$dynastylist[1:length(templist)] <- templist
  updateRadioButtons(session, "empireselector", choiceNames = rv$dynastylist,
                     choiceValues = seq(from = 1, to = nrdynasties))
  }
    }
  )
  
  
  
  
  # ON CLICK
  observeEvent(input$plot_click,{
    
      combinedraster <- landraster
      combinedraster[!is.na(landraster)] <- input$landspeed
      combinedraster[!is.na(searaster)] <- input$seaspeed
      combinedraster[!is.na(mountainraster) & is.na(riverraster)] <- input$mountainspeed
      combinedraster[is.na(mountainraster) & !is.na(riverraster)] <- input$riverspeed
      combinedraster[!is.na(mountainraster) & !is.na(riverraster)] <- input$riverspeed*0.5 + input$mountainspeed*0.5
      
      rv$combinedrasterplot <- as(combinedraster, "SpatialPixelsDataFrame") %>%
        as.data.frame()
      
      #sealayer[!is.na(searaster)] <- input$seaspeed
      #sealayer[is.na(searaster)] <- 0
      #mountainlayer[!is.na(mountainraster)] <- input$mountainspeed - input$landspeed
      #mountainlayer[is.na(mountainraster)] <- 0
      
      #riverlayer[!is.na(riverraster)] <- input$riverspeed - input$landspeed
      #riverlayer[is.na(riverraster)] <- 0
      
      #combinedraster <- combinedraster + sealayer + mountainlayer + riverlayer
      
      # NOW FOR COST DISTANCE
      ncf <- function(x) (x[1]+x[2])/2
      
      ncf_transitions <- function(x) {
        if(x[1]!=x[2]){
          input$changespeed
        }else{
          (x[1]+x[2])/2
        }
      }
      
      tr1.sym <- transition(combinedraster, transitionFunction = ncf_transitions, directions = 8, symm=FALSE)
      
      combinedraster <- as(combinedraster, "SpatialPixelsDataFrame") %>%
        as.data.frame()
      
      # Correct geography
      tr1.sym.c <- geoCorrection(tr1.sym, type="c", multpl=FALSE, scl=TRUE)
      tr1.sym.r <- geoCorrection(tr1.sym, type="r", multpl=FALSE, scl=TRUE)
      tr1.sym.cor <- (tr1.sym.c+tr1.sym.r)/2
    
    if(length(hexselect()) > 0){
    rv$c_hex <- as.double(hexselect())
    }else{
      rv$c_hex <- rv$c_hex
    }
    
    rv$hexcrop_rv[,"selected"] <-    0
    rv$hexcrop_rv[rv$hexcrop_rv$hexnr==rv$c_hex,"selected"] <-    1

    hexcrop_selected <- rv$hexcrop_rv %>%
      filter(selected==1)
    
    hexcrop_selected_centroids_sf <- st_centroid(hexcrop_selected)
    
    hexcrop_selected_centroids <- hexcrop_selected_centroids_sf %>%
      as(Class = "Spatial")
    
    rv$selectedpoint1 <- hexcrop_selected_centroids_sf
    
    rv$empirevalue <- as.numeric(input$empireselector)
    
    rv$select_list[[rv$empirevalue]] <- hexcrop_selected_centroids[1,]
    
    #if(rv$empirevalue==1){
    #  rv$select_list[[1]] <- hexcrop_selected_centroids[1,]
    #}
    
    
    select_current <- hexcrop_selected_centroids[1,]
    
    one_acc <-  accCost(tr1.sym.cor, select_current)
    raster_acc <- as(one_acc, "SpatialPixelsDataFrame")
    rv$raster_acc <- as.data.frame(raster_acc)

    
    # CREATE CONCATENATED
      valids <- Filter(Negate(is.null), rv$select_list)
    ownerlist <- which(as.integer(lapply(rv$select_list, is.null))==0)

    select_all <- valids[[1]]
    
    if(length(valids)>1){
      select_all <- rbind(select_all, valids[[2]])
    }
    
    if(length(valids)>2){
      select_all <- rbind(select_all, valids[[3]])
    }
    
    if(length(valids)>3){
      select_all <- rbind(select_all, valids[[4]])
    }
    
    if(length(valids)>4){
      select_all <- rbind(select_all, valids[[5]])
    }
    
    if(length(valids)>5){
      select_all <- rbind(select_all, valids[[6]])
    }
    
    if(length(valids)>6){
      select_all <- rbind(select_all, valids[[7]])
    }
    
    if(length(valids)>7){
      select_all <- rbind(select_all, valids[[8]])
    }
    
    if(length(valids)>8){
      select_all <- rbind(select_all, valids[[9]])
    }
    
    if(length(valids)>9){
      select_all <- rbind(select_all, valids[[10]])
    }
    
    if(length(valids)>10){
      select_all <- rbind(select_all, valids[[11]])
    }
    
    if(length(valids)>11){
      select_all <- rbind(select_all, valids[[12]])
    }
    
    if(length(valids)>12){
      select_all <- rbind(select_all, valids[[13]])
    }
    
    if(length(valids)>13){
      select_all <- rbind(select_all, valids[[14]])
    }
    
    if(length(valids)>14){
      select_all <- rbind(select_all, valids[[15]])
    }
    
    
      
    rv$select_all_sf <- st_as_sf(select_all) %>%
      mutate(owner = ownerlist,
             dynasty = as.character(NA),
             dynasty = rv$dynastylist[ownerlist])
    

    
    ### CREATE DISTANCE MATRIX
    distmatrix <- costDistance(tr1.sym.cor, fromCoords = select_all, toCoords = hexcrop_centroids) %>%
      as.matrix() %>%
      as_tibble() %>%
      mutate(origin_hexid = ownerlist)
    
    distmatrix_long <- pivot_longer(distmatrix, cols = -origin_hexid, names_to = "hexnr")

    #class(select_current)
    sp_select_current <- as(select_current, Class="SpatialPoints")

    # Lines to selected cities
    rv$examplepaths <- shortestPath(tr1.sym.cor, origin = sp_select_current, goal = select_all, output="SpatialLines") %>%
      st_as_sf()
    
    
    # Empire of certain size
    #distmatrix_trimmed <- distmatrix_long %>%
    #  arrange(value) %>%
    #  slice_head(n=input$empiresize) %>%
    #  ungroup() %>%
    #  mutate(hexnr = as.numeric(substr(hexnr, 2, input$empiresize)),
    #         newempire = 1)
    

    # Zero sum empires
    distmatrix_zerosum <- distmatrix_long %>%
      mutate(owner = origin_hexid) %>%
      mutate(hexnr = as.numeric(substr(hexnr, 2, 100))) %>%
      group_by(hexnr) %>%
      arrange(value) %>%
      mutate(inhexrank = seq(from = 1, to =n())) %>%
      ungroup()
    
    distmatrix_zerosum <- distmatrix_zerosum %>%
      group_by(owner) %>%
      arrange(value) %>%
      mutate(ownerrank = seq(from = 1, to = n() )) %>%
      ungroup()
    
    distmatrix_zerosum$owner[distmatrix_zerosum$ownerrank > input$empiresize] <- 0
    
    distmatrix_zerosum <- distmatrix_zerosum %>%
      group_by(hexnr) %>%
      filter(inhexrank==1) %>%
      ungroup() %>%
      dplyr::select(owner, hexnr, -value, ownerrank)
    
    
    
    rv$hexcrop_merged <- rv$hexcrop_rv %>%
      dplyr::select(-owner) %>%
      left_join(., distmatrix_zerosum, by="hexnr") %>%
      mutate(ownerfac = factor(owner, levels = seq(from = 0, to = nrdynasties)))
    
    
    rv$dynastydf <- tibble(owner = seq(from = 0, to = nrdynasties, by=1), dynasty=c("Unclaimed", rv$dynastylist))
    # Create top list
    rv$toplist <- distmatrix_zerosum %>%
      mutate(owner = replace_na(owner, 0)) %>%
      group_by(owner) %>%
      summarize(nrhex = n()) %>%
      arrange(desc(nrhex)) %>%
      left_join(., rv$dynastydf, by="owner") %>%
      mutate(empirerank = seq(from = 1, to = n())) %>%
      dplyr::select('Rank' = empirerank, 'Empire' = dynasty, '# hexes' = nrhex)
  
    
    
  })
  
  
  output$distPlot <- renderPlot({
    
    ggplot() +
      geom_raster(data = rv$raster_acc, aes(x=x, y=y, fill=layer), na.rm = TRUE) +
      #geom_sf(data = rv$hexcrop_rv, fill="NA", col="#555555") +
      geom_sf(data = europeland, col="#888888", fill=NA) +
      geom_sf(data = europe_rivers, col="blue") +
      geom_point(data = rv$hexmid, aes(x=X, y=Y), size=1, alpha=0) +
      {if(nrow(rv$examplepaths)>1)geom_sf(data = rv$examplepaths, col="red", size=1)}+
      geom_sf(data = rv$selectedpoint1, col="black", size=4) +
      {if(rv$select_all_sf[[1,1]]!=999)geom_sf(data = rv$select_all_sf, col="black", size=3)} +
      {if(rv$select_all_sf[[1,1]]!=999)geom_sf_text(data = rv$select_all_sf, aes(label=dynasty), col="black", size=5, nudge_y = -80000)} +
      theme_minimal() +
      theme(legend.position = c(0.1, 0.9),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null")) +
      scale_fill_viridis(direction=-1, na.value="transparent") +
      labs(x=element_blank(), y=element_blank(), title=element_blank(), fill="Cost distance")
  })
  
  output$speedPlot <- renderPlot({
    
    ggplot() +
      geom_raster(data = rv$combinedrasterplot, aes(x=x, y=y, fill=layer), na.rm = TRUE) +
      #geom_sf(data = rv$hexcrop_rv, fill="NA", col="#555555") +
      geom_sf(data = europeland, col="#888888", fill=NA) +
      geom_sf(data = europe_rivers, col="blue") +
      geom_point(data = rv$hexmid, aes(x=X, y=Y), size=1, alpha=0) +
      {if(nrow(rv$examplepaths)>1)geom_sf(data = rv$examplepaths, col="red", size=1)}+
      geom_sf(data = rv$selectedpoint1, col="black", size=4) +
      {if(rv$select_all_sf[[1,1]]!=999)geom_sf(data = rv$select_all_sf, col="black", size=3)} +
      {if(rv$select_all_sf[[1,1]]!=999)geom_sf_text(data = rv$select_all_sf, aes(label=dynasty), col="black", size=5, nudge_y = -80000)} +
      theme_minimal() +
      theme(legend.position = c(0.1, 0.9)) +
      scale_fill_viridis(direction=1, na.value="transparent") +
      labs(x=element_blank(), y=element_blank(), title=element_blank(), fill="Speed")
  })
  

  
  ### EMPIRES PLOT
  output$zerosumplot <- renderPlot({
    
    ownpal <- c("NA",
                "#ff595e", "#ffca3a", "#8ac926", "#1982c4", "#6a4c93",
                "#12664f", "#f5dd90", "#f68e5f", "#e4c1f9", "#2dc2bd",
                "#c5dd6c", "#22709e", "#8d229e", "#ed74aa", "#ea5a44")
    
    ownpal_df <- tibble(cols = ownpal, owner = seq(from = 0, to = nrdynasties))
    
    hexcrop_merged2 <- left_join(rv$hexcrop_merged, ownpal_df, by="owner")
    

    ggplot() +
      {if(input$hillshadeselector=="On")geom_tile(data = reliefraster_df, aes(x=x, y=y, fill = cols), alpha=0.6)} +
      geom_sf(data = rv$hexcrop_merged, fill=NA, col="#555555", lwd=0.1) +
      geom_sf(data = hexcrop_merged2, aes(fill=cols), col=NA, alpha=0.6) +
      geom_sf(data = europe_rivers, col="#6bace5", alpha=0.95) +
      {if(rv$select_all_sf[[1,1]]!=999)geom_sf(data = rv$select_all_sf, col="black", size=3)} +
      {if(rv$select_all_sf[[1,1]]!=999)geom_sf_text(data = rv$select_all_sf, aes(label=dynasty), col="black", size=5, nudge_y = -80000)} +
      theme_minimal() +
      theme(legend.position = "none",
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null")) +
      scale_fill_identity() +
      #scale_fill_manual(values=ownpal,
      #                  breaks = seq(from = 0, to = nrdynasties)) +
      labs(x=element_blank(), y=element_blank(), title=element_blank())
    
    
    
  })
  
  # Top list
  output$toplist <- renderTable(rv$toplist)
  
  
}

shinyApp(ui = ui, server = server)



