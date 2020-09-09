library(dplyr)
library(viridis)
library(tidyr)
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(gdtools)
library(ggiraph)
source("air_pollution_global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Googlesheet error print
  if (!is.null(message)){
    output$Warning <- renderText(message)
  } 
   
  ###################
  ## INTRODUCTION ##
  ###################
  
  output$airqdata <- renderDataTable({all_crowdsourced_data %>%
      dplyr::select("Group", "Name", "Date", "Time", "Site_Type (Indoor, Outdoor)", "Latitude", "Longitude", "PM2.5", "CO", "Comments")
  }, options = list(pageLength=10, searching=FALSE))
   
   
   ####################
   ## SELECTED DATA ##
   ####################
   
   ## Data for Map
   data <- reactive({
     #all_dates <- setdiff(df$Date,seq(as.Date(input$dates[1]), as.Date(input$dates[2]), by="days"))
     all_dates <- as.character(seq(as.Date(input$dates[1]), as.Date(input$dates[2]), by="days"))
     #check for group_status
     if (isFALSE(group_status)){
       x <- tf %>% dplyr::filter(Date %in% all_dates, Name %in% input$name, Variables == input$type)
     } else if (isTRUE(group_status)){
       x <- tf %>% dplyr::filter(Date %in% all_dates, Name %in% input$name, Variables == input$type, Group %in% input$group)
     }
   })
   
   ## Data for plots
   plot_data <- reactive({
     #data() gives dataset with either PM2.5 or CO not both (depending upon input$type selection)
     #Get both PM2.5 and CO values from the selected options
     temp_df <- data() %>% dplyr::select(Name,Latitude,Longitude,Date,Time)
     data <- merge(tf,temp_df,by=c("Name","Latitude","Longitude","Date","Time"))
   })
   
  
   ###################
   ## VISUALIZATION ##
   ###################
   
   #Images
   #EPA Monitors
   output$epa_monitor <- renderImage({fi="databases/EPA_Monitors.png"
   return(list(src = fi,height= 290,filetype = "image/png",
               alt = "EPA Monitors"))}, deleteFile = FALSE) 
   
   #Sensor Setup
   output$sensor_setup <- renderImage({fi="databases/sensor_setup.png"
   return(list(src = fi,height= 200,filetype = "image/png",
               alt = "Sensor Setup"))}, deleteFile = FALSE) 
   
   #Sensor Working
   output$sensor_working <- renderImage({fi="databases/sensor_working.png"
   return(list(src = fi,height= 200,filetype = "image/png",
               alt = "How Sensors Work"))}, deleteFile = FALSE) 
   
   #City Images
   output$philadelphia <- renderImage({fi="databases/philadelphia_PM.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Philadelphia,PA"))}, deleteFile = FALSE)
   
   output$newyork <- renderImage({fi="databases/Midtown_Manhattan.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Midtown Manhattan,NY"))}, deleteFile = FALSE)
   
   output$los_angeles <- renderImage({fi="databases/Los_Angeles_CA.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Los Angeles,CA"))}, deleteFile = FALSE)
   
   output$miami <- renderImage({fi="databases/Miami_Florida.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Miami,FL"))}, deleteFile = FALSE)
   
   output$pierre <- renderImage({fi="databases/Pierre_SD.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Pierre,South Dakota"))}, deleteFile = FALSE)
   
   output$billings <- renderImage({fi="databases/billings_montana.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Billings,Montana"))}, deleteFile = FALSE)
   
   output$shiprock <- renderImage({fi="databases/Shiprock_New Mexico.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Shiprock, New Mexico"))}, deleteFile = FALSE)
   
   output$portland <- renderImage({fi="databases/Portland_Oregon.tiff"
   return(list(src = fi,height= 200,width=250,filetype = "image/png",
               alt="Portland, Oregon"))}, deleteFile = FALSE)
   
   
  #Measurement I - PM2.5
  #Output
  output$distBoxplot <- renderPlot({
    boxplot_func_ap("Variables", "Measurement", "PM2.5",plot_data())
  })

  output$disHist <- renderPlot({
    hist_func("Measurement","PM2.5",plot_data())})
  
  #Measurement II - CO
  #Output
  output$pdistBoxplot <- renderPlot({
    boxplot_func_ap("Variables", "Measurement", "CO",plot_data())
  })
  
  output$pdisHist <- renderPlot({
    hist_func("Measurement","CO",plot_data())
  })
  
  # Bivariate Scatterplot
  output$Scatplot <- renderPlot({
    #scatterplot_func("PM2.5","CO",data())
    scatterplot_func("PM2.5","CO")
    })
  
  # Barplot for EPA cities
  output$kbarPlot <- renderPlot({
    data <- k12_df %>% dplyr::filter(State %in% input$kcity)
    barplot_func(data)
  })
  
  
  
  ########################
  ## GROUP and NAME UI ##
  ########################
  
  ##Group UI
  observe({
    if(isTRUE(group_status)){
      output$Group <- renderUI({pickerInput("group","Select School:", choices=unique(as.character(all_crowdsourced_data$Group)), multiple=TRUE, selected="J R Masterman")})
    }})
  
  #Name UI - select names to display accroding to group selected
  names_by_group <- reactive({
    if(isTRUE(group_status)){
      n <- unique(as.vector(all_crowdsourced_data %>% dplyr::filter(Group %in% input$group) %>% dplyr::select(Name)))
    } else {
      n <- unique(all_crowdsourced_data$Name)
    }
  })
  
  output$Name <- renderUI({
    selectInput("name","Select Name:", choices=names_by_group(), multiple=TRUE, selected=unique(all_crowdsourced_data$Name))
  })
  
  
  
  ####################
  ## DATA DOWNLOAD ##
  ####################
  
  ##Selected Data Download
  output$selected_data_download <- downloadHandler(
    filename= function(){paste0("Crowdsourced_measures_",gsub(":| ","-",Sys.time()),".csv")},
    content=function(file){
      write.csv(data(), file, row.names = FALSE, quote = FALSE)})
  
  ##EPA Data Download
  output$EPA_data_download <- downloadHandler(
    filename= function(){paste0("EPA_measures_daily_average_Sept2017.csv")},
    content=function(file){
      write.csv(EPA_data_file, file, row.names = FALSE, quote = FALSE)})
  
  ##########################
  ## PM.25 and CO PLOTS ##
  ##########################
  
  #PM 2.5
  output$PMPlot <- renderggiraph({
    pmdata <- ph_df %>% dplyr::filter(Year %in% input$Year, State %in% input$State)
    girafe(ggobj = scatplot_func_ph(pmdata,"PM 2.5 (μg/m3)"))
  })
  
  #CO 
  output$COPlot <- renderggiraph({
    codata <- co_df %>% dplyr::filter(Year %in% input$Year, State %in% input$State) %>% rename(PM2.5=CO)
    girafe(ggobj = scatplot_func_ph(codata, "CO (ppm)"))
  })
  
  
  ###########
  ## MAPS ##
  ###########
  
  #PM/CO public data map
  output$kmap <- renderLeaflet({
    mdata <- k12_df %>% dplyr::filter(State %in% input$kcity)
    mdata$PM <- round(mdata$PM, 2)
    #col_status = terrain.colors(8)[1:7]
    col_status = rev(c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"))
    pal <- colorNumeric(palette = col_status,domain = mdata$PM)
    ppal <- colorNumeric(palette = rev(col_status),domain = mdata$PM)
    m <- leaflet(data=mdata) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(-98.35, 39.5, zoom = 4.1) %>%
      #setView(lng = mean(mdata$Longitude),lat = mean(mdata$Latitude),zoom=4.4) %>%
      addLegend(pal = pal,values = unique(mdata$PM), position="bottomleft", title="PM2.5 (μg/m3)", opacity=1, labFormat=labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addCircleMarkers(lng=~Longitude, lat=~Latitude, color=~ppal(PM), radius=7, stroke=FALSE, fillOpacity=1,
                       popup = paste("Name:", mdata$City, "<br>",
                                     "PM2.5:", mdata$PM, "<br>")) 
    
    m
  })
  
  #Student data map
  output$mymap <- renderLeaflet({
    mdata <- data()
    col_status = heat.colors(8)[1:5]
    #col_status = rev(c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"))
    #col_status = rev(terrain.colors(8)[1:7])
    #pal <- colorFactor(palette = col_status,levels = unique(mdata$Measurement))
    pal <- colorNumeric(palette=col_status, domain=c(mdata$Measurement))
    ppal <- colorNumeric(palette=rev(col_status), domain=c(mdata$Measurement))
    m <- leaflet(data=mdata) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(-75.19, 39.95, zoom=13) %>%
      #fitBounds(~min(mdata$Longitude), ~min(mdata$Latitude), ~max(mdata$Longitude), ~max(mdata$Latitude)) %>% 
      addLegend(pal = pal,values = unique(mdata$Measurement),position ="bottomleft", title=input$type, opacity=1, labFormat=labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addCircleMarkers(lng=~Longitude, lat=~Latitude, fillColor=~ppal(Measurement), stroke=TRUE, fillOpacity=0.5,color= "black", radius = 7,weight = 1.5, #lapply(input$name,function(x) color_status[[x]])
                       popup = paste("Name", mdata$Name, "<br>",
                                     "Timestamp:", mdata$Timestamp, "<br>",
                                     paste0(input$type,":"), mdata$Measurement, "<br>")) 
    
    m
  })
  
  
})


