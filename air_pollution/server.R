library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
source("../basic_functions.R")


# df <- read.csv("../gene_expression/databases/AirQualityData.csv", stringsAsFactors = F)
# df <- tidyr::separate(data=df,
#                       col=Location,
#                       into=c("Latitude", "Longitude"),
#                       sep=",",
#                       remove=FALSE)
# df$Latitude <- stringr::str_replace_all(df$Latitude, "[(]", "")
# df$Longitude <- stringr::str_replace_all(df$Longitude, "[)]", "")
# df$Latitude <- as.numeric(df$Latitude)
# df$Longitude <- as.numeric(df$Longitude)
# df <- df %>% dplyr::select(-ACTION)
# df$Date <- gsub(" .*"," ", df$Timestamp)
# df$Time <- gsub(".* ","", df$Timestamp)
# saveRDS(df, "../gene_expression/databases/AirQualityData.RDS")


df <- readRDS("../gene_expression/databases/AirQualityData.RDS")
tf <- melt(df %>% dplyr::select(Temperature,Humidity,DustPM, Name, AirQuality,Latitude,Longitude),id = c("Name","Latitude","Longitude"))
names(tf) <- c("Name","Latitude","Longitude","Variables","Measurement")

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
    hist_func("Measurement",data_tf)
  })
  
  ## Map
  data <- reactive({
    x <- df %>% dplyr::filter(Date %in% input$date)
  })
  
  output$Date <- renderUI({
    selectInput("date","Select Date:",choices = unique(df$Date))
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
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
