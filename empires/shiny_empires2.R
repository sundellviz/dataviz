rm(list=ls(all=TRUE))
setwd() # Set working directory
library(tidyverse)
library(rgdal)
library(viridis)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rnaturalearth)
library(viridis)

library(gdistance)

library(shiny)

# LOAD DATA
crs.wgs84 <- CRS("+init=EPSG:4326")
crs.laea <- CRS("+init=EPSG:3035")

hexcrop <- st_read("assets/hexcrop.shp") %>%
  mutate(owner = 0,
         ownerfac = factor(0, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)),
         dynasty = as.character(NA))
load("assets/hexmid.rData")

examplepoint <- st_centroid(hexcrop[1,])

europeland <- st_read("assets/europeland.shp")
europe_rivers <- st_read("assets/europe_rivers.shp")

landraster <- raster("assets/landraster.grd")
riverraster <- raster("assets/riverraster.grd")

load(file="assets/hexcrop_centroids.shp")

dynastylist <- c("Habsburg", "Hohenzollern", "Capet",
                 "Plantagenet", "Rurik", "Komnenos",
                 "Wittelsbach", "Arpad", "Oldenburg",
                 "Bourbon")

crs(landraster) <- crs.laea
crs(riverraster) <- crs.laea
####

#Empty select list

ui <- fluidPage(
  
  # App title ----
  titlePanel("Empire Simulator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
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
      value = 5
    ),
    sliderInput(
      inputId = "changespeed",
      label = "Travel change speed",
      min = 1,
      max = 50,
      value = 5
    ),
    sliderInput(
      inputId = "empiresize",
      label = "Empire size",
      min = 1,
      max = 1000,
      value = 300
    ),
    #selectInput(
    #  inputId = "empireselector",
    #  label = "Empire selector",
    #  choices = c(1, 2, 3, 4, 5)
    #  ),
    
    radioButtons(inputId = "empireselector",
                 label = "Empire Selector:",
                 choiceNames = dynastylist,
                 choiceValues = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 inline = FALSE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "zerosumplot", click = "plot_click", width="800px", height="800px"),
      plotOutput(outputId = "distPlot", click = "plot_click", width="800px", height="800px"),
      #plotOutput(outputId = "empireplot", click = "plot_click", width="800px", height="800px"),
      
      #verbatimTextOutput("info")
      
      
    )
  )
)

###############

server <- function(input, output) {
  
  
  
  hexselect <- eventReactive(input$plot_click, {
    as.double(nearPoints(hexmid, input$plot_click, xvar = "X", yvar = "Y", maxpoints = 1)$hexnr)
  })
  

  emptypoint <- df <- st_sf(id = 1:1, geometry = st_sfc(lapply(1:1, function(x) st_geometrycollection())))
  
  rv <- reactiveValues(hexcrop_rv = hexcrop,
                       raster_acc = data.frame(x=as.double(NA), y=as.double(NA), layer=as.double(1)),
                       c_hex = 500,
                       hexcrop_merged = hexcrop,
                       select_list = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
                       empirevalue = as.numeric(1),
                       select_all_sf = examplepoint)
  
  
  
  # ON CLICK
  observeEvent(input$plot_click,{
    
      landraster[!is.na(landraster)] <- input$landspeed
      landraster[is.na(landraster)] <- input$seaspeed
      
      riverraster[!is.na(riverraster)] <- input$riverspeed
      riverraster[is.na(riverraster)] <- 0
      
      
      combinedraster <- landraster + riverraster
      
      
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
      
      # Correct geography
      tr1.sym.c <- geoCorrection(tr1.sym, type="c", multpl=FALSE, scl=TRUE)
      tr1.sym.r <- geoCorrection(tr1.sym, type="r", multpl=FALSE, scl=TRUE)
      tr1.sym.cor <- (tr1.sym.c+tr1.sym.r)/2
    
    
    
    if(length(hexselect()) > 0){
    rv$c_hex <- as.double(hexselect())
    }else{
      rv$c_hex <- rv$c_hex
    }
    
    print(paste("Selected hex:", rv$c_hex))
    
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
      
    rv$select_all_sf <- st_as_sf(select_all) %>%
      mutate(owner = ownerlist,
             dynasty = as.character(NA),
             dynasty = dynastylist[ownerlist])
    

    
    ### CREATE DISTANCE MATRIX
    distmatrix <- costDistance(tr1.sym.cor, fromCoords = select_all, toCoords = hexcrop_centroids) %>%
      as.matrix() %>%
      as_tibble() %>%
      mutate(origin_hexid = ownerlist)
    
    distmatrix_long <- pivot_longer(distmatrix, cols = -origin_hexid, names_to = "hexnr")
    
    # Empire of certain size
    distmatrix_trimmed <- distmatrix_long %>%
      arrange(value) %>%
      slice_head(n=input$empiresize) %>%
      ungroup() %>%
      mutate(hexnr = as.numeric(substr(hexnr, 2, input$empiresize)),
             newempire = 1)
    

    # Zero sum empires
    distmatrix_zerosum <- distmatrix_long %>%
      mutate(hexnr = as.numeric(substr(hexnr, 2, 100))) %>%
      group_by(hexnr) %>%
      arrange(value) %>%
      slice_head(n=1) %>%
      ungroup() %>%
      group_by(origin_hexid) %>%
      arrange(value) %>%
    mutate(ownerrank = seq(from = 1, to = n() )) %>%
      ungroup() %>%
      dplyr::select(owner = origin_hexid, hexnr, -value, ownerrank)
      
    
    distmatrix_zerosum$owner[distmatrix_zerosum$ownerrank > input$empiresize] <- 0
      
    
      
    

    
    #rv$hexcrop_merged <- left_join(rv$hexcrop_rv, distmatrix_trimmed, by="hexnr") %>%
    #  mutate(inempire = 0,
    #         inempire = case_when(newempire==1 ~ 1,
    #                              TRUE ~ 0))
    
    
    rv$hexcrop_merged <- rv$hexcrop_rv %>%
      dplyr::select(-owner) %>%
      left_join(., distmatrix_zerosum, by="hexnr") %>%
      mutate(ownerfac = factor(owner, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
    
    
    
    
  
    
    
  })
  
  
  ### DISTANCE PLOT
  output$distPlot <- renderPlot({
    
    ggplot() +
      geom_tile(data = rv$raster_acc, aes(x=x, y=y, fill=layer)) +
      geom_sf(data = rv$hexcrop_rv, fill="NA", col="#555555") +
      geom_sf(data = europe_rivers, col="blue") +
      geom_point(data = hexmid, aes(x=X, y=Y), size=1, alpha=0) +
      geom_sf(data = rv$selectedpoint1, col="black", size=4) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis(direction=-1)
  
  })
  
  ### EMPIRE PLOT
  #output$empireplot <- renderPlot({
  #  
  #  ggplot() +
  #    geom_sf(data = rv$hexcrop_merged, aes(fill=as.factor(inempire), col=as.factor(inempire))) +
  #    geom_sf(data = europe_rivers, col="blue") +
  #    geom_sf(data = rv$selectedpoint1, col="black", size=4) +
  #    theme_minimal() +
  #    theme(legend.position = "none") +
  #    scale_fill_manual(values=c("#dddddd", "#cc0088")) +
  #    scale_color_manual(values=c("#aaaaaa", "#59013c"))
  #  
  #  
  #})
  
  
  ### EMPIRES PLOT
  output$zerosumplot <- renderPlot({
    
    ownpal <- c("#eeeeee", "#ff595e", "#ffca3a", "#8ac926", "#1982c4", "#6a4c93",
                "#12664f", "#f5dd90", "#f68e5f", "#e4c1f9", "#2dc2bd")

    ggplot() +
      geom_sf(data = rv$hexcrop_merged, fill=NA, col="#555555", lwd=0.5) +
      geom_sf(data = rv$hexcrop_merged, aes(fill=ownerfac), col=NA, alpha=0.85) +
      geom_sf(data = europe_rivers, col="blue", alpha=0.7) +
      geom_sf(data = rv$select_all_sf, col="black", size=3) +
      geom_sf_text(data = rv$select_all_sf, aes(label=dynasty), col="black", size=5, nudge_y = -80000) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values=ownpal,
                        breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      scale_color_manual(values=c("#aaaaaa", "#59013c"))
    
    
  })
  
  

  
    
}

shinyApp(ui = ui, server = server)



