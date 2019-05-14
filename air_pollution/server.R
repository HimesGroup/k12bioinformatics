library(shiny)
library(leaflet)
library(dplyr)
library(viridis)
library(tidyr)
library(tidyverse)
library(reshape2)
library(gsheet)
source("air_pollution_global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #Introduction
  output$airqdata <- renderDataTable({ df %>%
      dplyr::select(-Timestamp,-Location)
  }, options = list(pageLength=10, searching=FALSE))
    
  #Measurement I
  output$var <- renderUI({
    selectInput("var","Select Measurement Types:",choices = unique(tf$Variables))
  })
  
  #Output
  output$distBoxplot <- renderPlot({
    if(!is.null(input$var)){
      boxplot_func_ap("Variables","Measurement",input$var)
    } else {NULL}
  })

  output$disHist <- renderPlot({
    if(!is.null(input$var)){hist_func("Measurement",input$var)}else{NULL}})
  
  #Measurement II
  output$pvar <- renderUI({
    selectInput("pvar"," ",choices = setdiff(as.vector(unique(tf$Variables)),input$var))
  })
  
  #Output
  output$pdistBoxplot <- renderPlot({
    if(!is.null(input$pvar)){
      boxplot_func_ap("Variables","Measurement",input$pvar)
    }else{NULL}
  })
  
  output$pdisHist <- renderPlot({
    if(!is.null(input$pvar)){
      hist_func("Measurement",input$pvar)
    }else{NULL}
  })
  
  #Scatterplot
  output$Scatplot <- renderPlot({
    if((!is.null(input$var)) & (!is.null(input$pvar))){scatterplot_func(input$var,input$pvar)}else{NULL}})
  
  ## Map
  data <- reactive({
    #all_dates <- setdiff(df$Date,seq(as.Date(input$dates[1]), as.Date(input$dates[2]), by="days"))
    all_dates <- as.character(seq(as.Date(input$dates[1]), as.Date(input$dates[2]), by="days"))
    x <- tf %>% dplyr::filter(Date %in% all_dates,Name %in% input$name,Variables == input$type)
  })
  
  output$Name <- renderUI({
    selectInput("name","Select Name:",choices=unique(tf$Name), multiple=TRUE, selected=unique(tf$Name))
  })
  
  
  #Images
  output$PhImage <- renderImage({
    return(list(
      src = "../databases/upenn.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Philadelphia"))}, deleteFile = FALSE)
  
  output$NYImage <- renderImage({
    return(list(
      src = "../databases/Midtown_Manhattan.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Midtown Manhattan, NY"))}, deleteFile = FALSE)
  
  output$LAImage <- renderImage({
    return(list(
      src = "../databases/Los_Angeles_CA.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Los Angeles, CA"))}, deleteFile = FALSE)
  
  output$MAImage <- renderImage({
    return(list(
      src = "../databases/Miami_Florida.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Miami, Florida"))}, deleteFile = FALSE)
  
  output$PRImage <- renderImage({
    return(list(
      src = "../databases/Pierre_SD.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Pierre, South Dakota"))}, deleteFile = FALSE)
  
  output$BLImage <- renderImage({
    return(list(
      src = "../databases/billings_montana.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Billings, Montana"))}, deleteFile = FALSE)
  
  output$SRImage <- renderImage({
    return(list(
      src = "../databases/Shiprock_New Mexico.tiff",
      height= 200,
      width = 250,
      filetype = "image/tiff",
      alt = "Standing Rock, New Mexico"))}, deleteFile = FALSE)
  
  output$kbarPlot <- renderPlot({
    data <- k12_df %>% dplyr::filter(State %in% input$kcity)
    barplot_func(data)
  })
  
  output$PhPlot <- renderPlot({
    barplot_func_ph(ph_df)
  })
  
  
  #pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
  output$mymap <- renderLeaflet({
    mdata <- data()
    col_status = rev(viridis(256, option = "B"))
    #col_status = brewer.pal(9,"Reds")
    #pal <- colorFactor(palette = col_status,levels = unique(mdata$Measurement))
    pal <- colorNumeric(palette = col_status,domain = c(mdata$Measurement))
    m <- leaflet(data=mdata) %>%
      addTiles() %>%
      setView(-78.35, 39.5, zoom = 5.8) %>%
      #fitBounds(~min(mdata$Longitude), ~min(mdata$Latitude), ~max(mdata$Longitude), ~max(mdata$Latitude)) %>% 
      addLegend(pal = pal,values = unique(mdata$Measurement),position = "bottomleft",title = input$type,opacity = 1) %>%
      addCircleMarkers(lng = ~Longitude,lat = ~Latitude,color = ~pal(Measurement),stroke = FALSE, fillOpacity = 1,  #lapply(input$name,function(x) color_status[[x]])
                 popup = paste("Name", mdata$Name, "<br>",
                               "Timestamp:", mdata$Timestamp, "<br>",
                                paste0(input$type,":"),mdata$Measurement, "<br>")) 
    
    m
  })
  
  output$kmap <- renderLeaflet({
    mdata <- k12_df %>% dplyr::filter(State %in% input$kcity)
    mdata$PM <- round(mdata$PM,2)
    col_status = rev(viridis(256, option = "B"))
    pal <- colorNumeric(palette = col_status,domain = c(mdata$PM))
    m <- leaflet(data=mdata) %>%
      addTiles() %>%
      setView(-98.35, 39.5, zoom = 4) %>%
      addLegend(pal = pal,values = unique(mdata$PM),position = "bottomleft",title = "PM 2.5",opacity = 1) %>%
      addCircleMarkers(lng = ~Longitude,lat = ~Latitude,color = ~pal(PM),stroke = FALSE, fillOpacity = 1,
                       popup = paste("Name:", mdata$City, "<br>",
                                     "PM 2.5:", mdata$PM, "<br>")) 
    
    m
  })
  
})


