library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(gsheet)
source("../basic_functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #Introduction
  output$airqdata <- renderDataTable({ df %>%
      dplyr::select(-Timestamp,-Location)
  }, options = list(pageLength=10, searching=FALSE))
    
  #Measurement
  output$var <- renderUI({
    selectInput("var","Select Measurement Type:",choices = unique(tf$Variables))
  })
  
  output$distBoxplot <- renderPlot({
    data_tf <- tf %>% dplyr::filter(Variables %in% input$var)
    boxplot_func_ap("Variables","Measurement",data_tf)
  })

  output$disHist <- renderPlot({
    data_tf <- tf %>% dplyr::filter(Variables %in% input$var)
    hist_func("Measurement",data_tf, input$var)
  })
  
  ## Map
  data <- reactive({
    all_dates <- setdiff(df$Date,seq(as.Date(input$dates[1]), as.Date(input$dates[2]), by="days"))
    x <- df %>% dplyr::filter(Date %in% all_dates,Name %in% input$name)
  })
  
  output$Name <- renderUI({
    selectInput("name","Select Name:",choices=unique(tf$Name), multiple=TRUE, selected=unique(tf$Name))
  })
  
  
  output$mymap <- renderLeaflet({
    df <- data()
    m <- leaflet(data = df) %>%
      addTiles() %>%
      setView(-78.35, 39.5, zoom = 5.8) %>%
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup = paste("Name", df$Name, "<br>",
                               "Timestamp:", df$Timestamp, "<br>",
                               "Temperature:", df$Temperature, "<br>",
                               "Humidity:", df$Humidity, "<br>",
                               "DustPM:",df$DustPM, "<br>",
                               "AirQuality:", df$AirQuality, "<br>"))
      
    m
  })
  
})
