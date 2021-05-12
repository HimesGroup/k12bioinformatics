
source("global.R")

# server
server <- shinyServer(function(input, output, session) {
  
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
  
  #################
  ## SENSOR DATA ##
  #################
  
  ## Plot map on GO -------------
  map.plot <- eventReactive(input$go, {
    
    #Creates base layer for raster layer to be added to map later
    r <- raster(nrows = 64, ncols = 60, xmn = input$lon.range[1], 
                xmx = input$lon.range[2], ymn = input$lat.range[1], ymx = input$lat.range[2])
    
    lons <- xFromCell(r, 1:ncell(r))
    lats <- yFromCell(r, 1:ncell(r))
    
    step.size.x <- (input$lon.range[2] - input$lon.range[1]) / 60
    step.size.y <- (input$lat.range[2] - input$lat.range[1]) / 64
    
    content <- vector()
    
    #Accounts for fact that time is subsetted by the hour 
    if(input$times[2] != "23:59"){
      upper.ind <- grep(input$times[2], mins) - 1
    }
    else{
      upper.ind <- grep("23:59", mins)
    }
    
    #Subsets data by user-selected date range and time-range
    #Removes rows containing NAs for selected measurement type
    map.data <- app.data %>%
      dplyr::filter(
        date(Timestamp) %in% input$dates[1]:input$dates[2],
        strftime(Timestamp, format = '%H:%M', tz = 'America/New_York') %in% 
          mins[grep(input$times[1], mins) : upper.ind])
    
    sensor.data <- map.data %>% dplyr::filter(Sensor.ID %in% input$sensors)
    
    assign('download.data', subset(sensor.data, 
                                   select = -Count), 
           envir = .GlobalEnv)
    
    # Remove big objects and clear memory -------------
    rm(map.data)
    
    ## Value map layers: -------------
    
    # Crime raster
    # crime.data <- app.data %>% dplyr::filter(!is.na(Crime))
    # assign("map.layer.c", rasterize(crime.data[,3:2], r, crime.data$Crime, fun = sum, na.rm = TRUE), 
    #        envir = .GlobalEnv)
    # 
    # # Poverty raster
    # assign("map.layer.pov", rasterize(pov.shp, r, field = pov.shp$ADI_NAT, fun = mean, na.rm = TRUE),
    #        envir = .GlobalEnv)
    
    # Traffic raster
    assign("map.layer.tr", 
           try(resample(traffic.raster, r, method = "bilinear"), silent = TRUE),
           envir = .GlobalEnv)
    if(length(map.layer.tr) == 1){
      assign("map.layer.tr", rasterize(data.frame(NA, NA), r, na.rm = TRUE), envir = .GlobalEnv)
    }
    
    # Value and density sensor rasters
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      measure.data <- dplyr::filter(sensor.data, !is.na(eval(parse(text = sensor.measures[i]))))
      if(nrow(measure.data) > 0){
        assign("density.raster", 
               rasterize(measure.data[,3:2], r, measure.data$Count, fun = sum, na.rm = TRUE))
        assign(paste0("map.layer", suffix),
               rasterize(measure.data[,3:2], r, measure.data[,sensor.measures[i]], fun = mean, na.rm = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign("density.raster",
               rasterize(data.frame(NA, NA), r, na.rm = TRUE))
        assign(paste0("map.layer", suffix),
               rasterize(data.frame(NA, NA), r, na.rm = TRUE),
               envir = .GlobalEnv)
      }       
      assign(paste0("map.layer", suffix, ".d"), density.raster, envir = .GlobalEnv)
      assign(paste0("map.layer", suffix, ".dlog"), 
             calc(density.raster, fun = function(x){log10(x)}), envir = .GlobalEnv)
    }
    
    ## Content format  -------------
    total_length <- seq(1,length(values(map.layer.pm2.5)))
    lat_lon <- vector()
    lat_lon <- paste0("<b>",
                      "Lat rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] - step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                      "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] + step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>",
                      "Lon rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] - step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                      "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] + step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>") 
    
    ## Google Maps Link
    gmaps <- vector()
    url <- paste0("https://earth.google.com/web/@",gsub(" ", "", format(round(lats[total_length] - step.size.y/2, 5), nsmall = 5), fixed = TRUE),",",
                  gsub(" ", "", format(round(lons[total_length] + step.size.x/2, 5), nsmall = 5), fixed = TRUE),",100000d")
    
    gmaps <- paste0("<b>","<a href =",url,",target=\"_blank\">Google Earth URL</a>","</b>","<br/>")
    
    
    # Temperature
    templ <- vector()
    templ[which(!is.na(values(map.layer.t)))] <- paste0("Avg. temperature: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.t)[which(!is.na(values(map.layer.t)))], digits = 1),"\u00B0C", "</b>"," (", values(map.layer.t.d)[which(!is.na(values(map.layer.t)))], ")","<br/>")
    templ[which(is.na(values(map.layer.t)))] <- paste0("Avg. temperature: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # Humidity
    humidl <- vector()
    humidl[which(!is.na(values(map.layer.h)))] <- paste0("Avg. humidity: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.h)[which(!is.na(values(map.layer.h)))], digits = 1),"%", "</b>"," (", values(map.layer.h.d)[which(!is.na(values(map.layer.h)))], ")","<br/>")
    humidl[which(is.na(values(map.layer.h)))] <- paste0("Avg. humidity: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM1
    pm1l <- vector()
    pm1l[which(!is.na(values(map.layer.pm1)))] <- paste0("Avg. PM<sub>1</sub>: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm1)[which(!is.na(values(map.layer.pm1)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm1.d)[which(!is.na(values(map.layer.pm1)))], ")","<br/>")
    pm1l[which(is.na(values(map.layer.pm1)))] <- paste0("Avg. PM<sub>1</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM2.5l
    pm2.5l <- vector()
    pm2.5l[which(!is.na(values(map.layer.pm2.5)))] <- paste0("Avg. PM<sub>2.5</sub>: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm2.5)[which(!is.na(values(map.layer.pm2.5)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm2.5.d)[which(!is.na(values(map.layer.pm2.5)))], ")","<br/>")
    pm2.5l[which(is.na(values(map.layer.pm2.5)))] <- paste0("Avg. PM<sub>2.5</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM10l
    pm10l <- vector()
    pm10l[which(!is.na(values(map.layer.pm10)))] <- paste0("Avg. PM<sub>10</sub>: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm10)[which(!is.na(values(map.layer.pm10)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm10.d)[which(!is.na(values(map.layer.pm10)))], ")","<br/>")
    pm10l[which(is.na(values(map.layer.pm10)))] <- paste0("Avg. PM<sub>10</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # Traffic
    trafl <- vector()
    tpoints <- point.in.SpatialPolygons(
      xFromCell(map.layer.tr, total_length), 
      yFromCell(map.layer.tr, total_length),
      city.border)
    
    # NA
    trafl[which(is.na(values(map.layer.tr)))] <- paste0("Avg. AADT: ","<b style = \"color:Tomato\">", "no data", "</b>","<br/>","</b>")
    
    # tpoint = 1 and not NA
    indt1 <- intersect(which(!is.na(values(map.layer.tr))),which(tpoints==1))
    trafl[indt1] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[indt1], digits = 0), "</b>","<br/>","</b>")
    
    # tpoint != 1 and not NA
    indt2 <- intersect(which(!is.na(values(map.layer.tr))),which(tpoints!=1))
    trafl[indt2] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[indt2], digits = 0), "</b>",
                           " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>","</b>")
    
    ## Final content vector -------------
    content <- paste0(lat_lon,gmaps,templ,humidl,pm1l,pm2.5l,pm10l,trafl) #crimel,povl,
    
    #Remove big objects -------------
    rm(lat_lon,gmaps,templ,humidl,pm1l,pm2.5l,pm10l,trafl)#crimel,povl,
    
    #Indicies for removing popups with all NA
    inds.df <- cbind(values(map.layer.t),
                     values(map.layer.h),
                     values(map.layer.pm1),
                     values(map.layer.pm2.5),
                     values(map.layer.pm10),
                     #values(map.layer.c),
                     #values(map.layer.pov),
                     values(map.layer.tr)
    )
    
    inds <- apply(inds.df, 1, function(x) all(is.na(x)))
    rm(inds.df)
    
    #Removes popups for which all data are NA
    #Coercing lat and lon to NA works better than removing these rows
    content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
    content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
    content.df[inds, 1:2] <- NA
    
    colors <- brewer.pal(7, "YlOrRd")
    colors.d <- brewer.pal(7, "Purples")
    
    for(i in 1:length(all.measures)){
      suffix <- f.suffix(all.measures[i])
      vals <- values(eval(parse(text = paste0("map.layer", suffix))))
      if(!all(is.na(vals))){
        vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
        assign(paste0("pal", suffix), 
               colorNumeric(palette = colors, 
                            domain = vals, 
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix), 
               colorNumeric(palette = colors, 
                            domain = vals, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      } else{
        assign(paste0("pal", suffix),
               colorNumeric(palette = colors,
                            domain = 0,
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix), 
               colorNumeric(palette = colors, 
                            domain = 0, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
    }
    
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      vals <- values(eval(parse(text = paste0("map.layer", suffix, ".dlog"))))
      if(!all(is.na(vals))){
        vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
        assign(paste0("pal", suffix, ".d"),
               colorNumeric(palette = colors.d, 
                            domain = vals, 
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix, ".d"), 
               colorNumeric(palette = colors.d, 
                            domain = vals, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign(paste0("pal", suffix, ".d"),
               colorNumeric(palette = colors.d,
                            domain = 0,
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix, ".d"), 
               colorNumeric(palette = colors.d, 
                            domain = 0, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
    }
    
    lon.center <- (input$lon.range[2] + input$lon.range[1]) / 2
    lat.center <- (input$lat.range[2] + input$lat.range[1]) / 2
    zoom.no <- f.zoom(input$lon.range[2] - input$lon.range[1], input$lat.range[2] - input$lat.range[1])
    button.js <- paste0("function(btn, map){ map.setView([", lat.center, ", ", lon.center, "], ", zoom.no, "); }")
    
    vals <- values(map.layer.pm2.5)
    if(!all(is.na(vals))){
      vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
    }
    
    vals.d <- values(map.layer.pm2.5.dlog)
    if(!all(is.na(vals))){
      vals.d <- c(0, vals.d, f.top(max(vals.d, na.rm = TRUE)))
    }
    
    ## Initialize leaflet -------------
    content_map <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.6, group = "Measurement value",method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                title = toString(f.titles("PM2.5")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      # addRasterImage(map.layer.pm2.5.dlog, colors = pal.pm2.5.d, opacity = 0.8, group = "Measurement density", method = "ngb") %>%
      # addLegend(pal = leg.pal.pm2.5.d, values = vals.d, opacity = 1, 
      #           title = paste("log\u2081\u2080 # of PM<sub>2.5</sub> data points"),
      #           group = "Measurement density", position = "topright",
      #           labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
      addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
                 primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      )) %>%
      leafem::addMouseCoordinates() %>%
      addLayersControl(baseGroups = all.measures.sub,
                       #overlayGroups = c("Measurement value", "Measurement density"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      showGroup(c("PM\u2082.\u2085")) %>% #, "Measurement value")) %>%
      hideGroup(c(all.measures.sub[which(all.measures.sub != "PM\u2082.\u2085")])) #, "Measurement density"))
                 
    ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###    
    
    #Plot individual maps side-by-side in a grid
    #Make PM2.5 maps
    pm25 <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                title = toString(f.titles("PM2.5")), position = "topright",
                #group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make PM1 maps
    vals1 <- values(map.layer.pm1)
    pm1 <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm1, colors = pal.pm1, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm1, values = vals1, opacity = 1,
                title = toString(f.titles("PM1")), position = "topright",
                #group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make PM10 maps
    vals10 <- values(map.layer.pm10)
    pm10 <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm10, colors = pal.pm10, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm10, values = vals10, opacity = 1,
                title = toString(f.titles("PM10")), position = "topright",
                #group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make temperature maps
    valst <- values(map.layer.t)
    temp <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.t, colors = pal.t, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.t, values = valst, opacity = 1,
                title = toString(f.titles("Temperature")), position = "topright",
                #group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make humidity maps
    valsh <- values(map.layer.h)
    humid <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.h, colors = pal.h, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.h, values = valsh, opacity = 1,
                title = toString(f.titles("Humidity")), position = "topright",
                #group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    # #Make crime maps
    # valsc <- values(map.layer.c)
    # crime <- leaflet(content.df) %>%
    #   setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
    #   addProviderTiles(providers$Esri.WorldTopoMap) %>%
    #   addRasterImage(map.layer.c, colors = pal.c, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
    #   addLegend(pal = leg.pal.c, values = valsc, opacity = 1,
    #             title = toString(f.titles("Crime")), position = "topright",
    #             #group = "Measurement value",
    #             labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    # 
    # #Make poverty maps
    # valspov <- values(map.layer.pov)
    # pov <- leaflet(content.df) %>%
    #   setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
    #   addProviderTiles(providers$Esri.WorldTopoMap) %>%
    #   addRasterImage(map.layer.pov, colors = pal.pov, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
    #   addLegend(pal = leg.pal.pov, values = valspov, opacity = 1,
    #             title = toString(f.titles("Poverty")), position = "topright",
    #             #group = "Measurement value",
    #             labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make traffic maps
    valstr <- values(map.layer.tr)
    tr <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.tr, colors = pal.tr, opacity = 0.6, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.tr, values = valstr, opacity = 1,
                title = toString(f.titles("Traffic")), position = "topright",
                #group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #####Make EPA maps
    epa.dates <- input$dates[1]:input$dates[2]
    
    #PM2.5
    epa.pm25.ras <- getEPAraster('PM2.5', epa.dates) #Also returns epa.df for downloading data
    epa.pm25.ras[epa.pm25.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.pm25.ras <- crop(epa.pm25.ras, extent(county.borders)) %>% mask(county.borders)
    epa.vals.pm25 <- values(epa.pm25.ras)
    
    epa.pal.pm25 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                 domain = epa.vals.pm25,
                                 na.color = "transparent"
    )
    epa.leg.pal.pm25 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                     domain = epa.vals.pm25,
                                     na.color = "transparent",
                                     reverse = TRUE
    )
    
    epa.pm25 <- leaflet() %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.pm25.ras, colors = epa.pal.pm25, opacity = 0.6, method = "ngb") %>%
      addLegend(pal = epa.leg.pal.pm25, values = epa.vals.pm25, opacity = 1,
                title = toString(f.titles.epa('PM2.5')), position = "topright",
                labFormat = myLabelFormat()) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      ))
    
    ##PM10
    epa.pm10.ras <- getEPAraster('PM10', epa.dates) #Also returns epa.df for downloading data
    epa.pm10.ras[epa.pm10.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.pm10.ras <- crop(epa.pm10.ras, extent(county.borders)) %>% mask(county.borders)
    epa.vals.pm10 <- values(epa.pm10.ras)
    
    epa.pal.pm10 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                 domain = epa.vals.pm10,
                                 na.color = "transparent"
    )
    epa.leg.pal.pm10 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                     domain = epa.vals.pm10,
                                     na.color = "transparent",
                                     reverse = TRUE
    )
    
    epa.pm10 <- leaflet() %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.pm10.ras, colors = epa.pal.pm10, opacity = 0.6, method = "ngb") %>%
      addLegend(pal = epa.leg.pal.pm10, values = epa.vals.pm10, opacity = 1,
                title = toString(f.titles.epa('PM10')), position = "topright",
                labFormat = myLabelFormat()) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      ))
    
    ##SO2
    epa.so2.ras <- getEPAraster('SO2', epa.dates) #Also returns epa.df for downloading data
    epa.so2.ras[epa.so2.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.so2.ras <- crop(epa.so2.ras, extent(county.borders)) %>% mask(county.borders)
    epa.vals.so2 <- values(epa.so2.ras)
    
    epa.pal.so2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                domain = epa.vals.so2,
                                na.color = "transparent"
    )
    epa.leg.pal.so2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                    domain = epa.vals.so2,
                                    na.color = "transparent",
                                    reverse = TRUE
    )
    
    epa.so2 <- leaflet() %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.so2.ras, colors = epa.pal.so2, opacity = 0.6, method = "ngb") %>%
      addLegend(pal = epa.leg.pal.so2, values = epa.vals.so2, opacity = 1,
                title = toString(f.titles.epa('SO2')), position = "topright",
                labFormat = myLabelFormat()) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      ))
    
    ##O3
    epa.o3.ras <- getEPAraster('O3', epa.dates) #Also returns epa.df for downloading data
    epa.o3.ras[epa.o3.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.o3.ras <- crop(epa.o3.ras, extent(county.borders)) %>% mask(county.borders)
    epa.vals.o3 <- values(epa.o3.ras)
    
    epa.pal.o3 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                               domain = epa.vals.o3,
                               na.color = "transparent"
    )
    epa.leg.pal.o3 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.o3,
                                   na.color = "transparent",
                                   reverse = TRUE
    )
    
    epa.o3 <- leaflet() %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.o3.ras, colors = epa.pal.o3, opacity = 0.6, method = "ngb") %>%
      addLegend(pal = epa.leg.pal.o3, values = epa.vals.o3, opacity = 1,
                title = toString(f.titles.epa('O3')), position = "topright",
                labFormat = myLabelFormat()) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      ))
    
    ##NO2
    epa.no2.ras <- getEPAraster('NO2', epa.dates) #Also returns epa.df for downloading data
    epa.no2.ras[epa.no2.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.no2.ras <- crop(epa.no2.ras, extent(county.borders)) %>% mask(county.borders)
    epa.vals.no2 <- values(epa.no2.ras)
    
    epa.pal.no2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                domain = epa.vals.no2,
                                na.color = "transparent"
    )
    epa.leg.pal.no2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                    domain = epa.vals.no2,
                                    na.color = "transparent",
                                    reverse = TRUE
    )
    
    epa.no2 <- leaflet() %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.no2.ras, colors = epa.pal.no2, opacity = 0.6, method = "ngb") %>%
      addLegend(pal = epa.leg.pal.no2, values = epa.vals.no2, opacity = 1,
                title = toString(f.titles.epa('NO2')), position = "topright",
                labFormat = myLabelFormat()) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      ))
    
    ##CO
    epa.co.ras <- getEPAraster('CO', epa.dates) #Also returns epa.df for downloading data
    epa.co.ras[epa.co.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.co.ras <- crop(epa.co.ras, extent(county.borders)) %>% mask(county.borders)
    epa.vals.co <- values(epa.co.ras)
    
    epa.pal.co <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                               domain = epa.vals.co,
                               na.color = "transparent"
    )
    epa.leg.pal.co <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.co,
                                   na.color = "transparent",
                                   reverse = TRUE
    )
    
    epa.co <- leaflet() %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.co.ras, colors = epa.pal.co, opacity = 0.6, method = "ngb") %>%
      addLegend(pal = epa.leg.pal.co, values = epa.vals.co, opacity = 1,
                title = toString(f.titles.epa('CO')), position = "topright",
                labFormat = myLabelFormat()) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      ))
    
    #make list of all maps #crime=crime, pov=pov,
    maps <- list(main = content_map, pm25=pm25, pm1=pm1, pm10=pm10,temp=temp,humid=humid,tr=tr,epa.pm25 = epa.pm25,
                 epa.pm10 = epa.pm10, epa.so2 = epa.so2,epa.no2 = epa.no2,epa.o3 = epa.o3, epa.co = epa.co)
    maps
    
  }) #End eventReactive
  
  #Delays plotting until "Go" button is clicked
  
  observeEvent(input$go, {
    output$int.map <- renderLeaflet({
      withProgress(message = "Loading map...", {
        maps <- map.plot()
        #main UI map
        maps$main})
    })
  })
  
  #Initialize 
  all_maps <- reactiveValues(dat = 0)
  
  #Plot individual maps side-by-side in a grid
  observeEvent(input$go, {
    output$all.maps <- renderUI({
      withProgress(message = "Loading map...", {
        maps <- map.plot()
        all_maps$dat <- sync(maps$pm25, maps$temp, maps$humid)})
    })
  })
  
  ###########Here, render main EPA map (actually, no, throw this down below. Initial map should depend on go, and updated EPA map on input EPA go)
  
  #Make custom map grid
  observeEvent(input$grid.update,{
    maps.list <- paste('maps', input$grid.vars, sep = '$') %>%
      as.list()
    output$all.maps <- renderUI({
      withProgress(message = "Loading map...", {
        maps <- map.plot()
        all_maps$dat <- sync(
          lapply(maps.list, function(x) eval(parse(text = x)))
        )})
    })
  })
  
  measure <- reactive({input$int.map_groups[[2]]})
  
  ## Map initialize
  observeEvent(input$int.map_groups, {
    
    map <- leafletProxy("int.map", session) %>%
      clearControls() %>%
      removeMarker(c("null1", "null2", "null3")) %>%
      clearPopups()
    
    ## Measurement value selected -------------
    #if("Measurement value" %in% input$int.map_groups && !"Measurement density" %in% input$int.map_groups){
      
    meas.text <- f.plaintext(measure())
    suffix <- f.suffix(meas.text)
    map.layer <- eval(parse(text = paste0("map.layer", suffix)))
    legend.title <- toString(f.titles(meas.text))
    
    if(measure() %in% all.measures.sub){
      
      if(all(is.na(values(map.layer)))){
        map %>%
          clearImages() %>%
          addLabelOnlyMarkers(
            lng = -75.15, lat = 40.00,
            label = "No data",
            layerId = "null1",
            labelOptions = labelOptions(noHide = TRUE,
                                        style = list(
                                          "color" = "red",
                                          "font-size" = "20px",
                                          "font-family" = "serif",
                                          "border-color" = "rgba(0,0,0,1)"
                                        )))
      }else{
        pal <- eval(parse(text = paste0("pal", suffix)))
        leg.pal <- eval(parse(text = paste0("leg.pal", suffix)))
        vals <- values(map.layer)
        vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
        
        map %>%
          clearImages() %>%
          removeMarker("null1") %>%
          addRasterImage(map.layer, colors = pal, opacity = 0.6, method = "ngb") %>%
          addLegend(pal = leg.pal, values = vals, opacity = 1,
                    title = legend.title, position = "topright",
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) #%>%
          # showGroup(c(input$int.map_groups, "Measurement value")) %>%
          # hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], "Measurement density"))
      }
    }
 #}
    
    # ## Measurement density selected -------------
    # else if("Measurement density" %in% input$int.map_groups && !"Measurement value" %in% input$int.map_groups){
    #   
    #   if(measure() %in% sensor.measures.sub){
    #     
    #     meas.text <- f.plaintext(measure())
    #     suffix <- f.suffix(meas.text)
    #     map.layer <- eval(parse(text = paste0("map.layer", suffix, ".dlog")))
    #     legend.title <- toString(f.titles.d(measure()))
    #     
    #     if(all(is.na(values(map.layer)))){
    #       map %>%
    #         clearImages() %>%
    #         addLabelOnlyMarkers(
    #           lng = -75.15, lat = 40.00,
    #           label = "No data",
    #           layerId = "null1",
    #           labelOptions = labelOptions(noHide = TRUE,
    #                                       style = list(
    #                                         "color" = "red",
    #                                         "font-size" = "20px",
    #                                         "font-family" = "serif",
    #                                         "border-color" = "rgba(0,0,0,1)"
    #                                       )))
    #     }
    #     
    #     else{
    #       
    #       pal <- eval(parse(text = paste0("pal", suffix, ".d")))
    #       leg.pal <- eval(parse(text = paste0("leg.pal", suffix, ".d")))
    #       vals <- values(map.layer)
    #       vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
    #       
    #       map %>%
    #         clearImages() %>%
    #         removeMarker("null1") %>%
    #         addRasterImage(map.layer, colors = pal, opacity = 0.6, method = "ngb") %>%
    #         addLegend(pal = leg.pal, values = vals, opacity = 1,
    #                   title = legend.title, position = "topright",
    #                   labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
    #         showGroup(c(measure(), "Measurement density")) %>%
    #         hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], 
    #                     "Measurement value"))
    #     }
    #     
    #     # Check for crime, poverty and traffic -------------
    #   } else if(measure() %in% other.measures){ 
    #     map %>%
    #       clearImages() %>%
    #       addLabelOnlyMarkers(
    #         lng = -75.15, lat = 40.00,
    #         label = "N/A",
    #         layerId = "null2",
    #         labelOptions = labelOptions(noHide = TRUE,
    #                                     style = list(
    #                                       "color" = "red",
    #                                       "font-size" = "20px",
    #                                       "font-family" = "serif",
    #                                       "border-color" = "rgba(0,0,0,1)"
    #                                     )))
    #   }
    #   
    # }
    # # Both value and density selected -------------
    # else if("Measurement value" %in% input$int.map_groups && "Measurement density" %in% input$int.map_groups){
    #   map %>%
    #     clearImages() %>%
    #     addLabelOnlyMarkers(
    #       lng = -75.15, lat = 40.00,
    #       label = "Please select only \"Measurement value\" or \"Measurement density\".",
    #       layerId = "null3",
    #       labelOptions = labelOptions(noHide = TRUE,
    #                                   style = list(
    #                                     "color" = "red",
    #                                     "font-size" = "20px",
    #                                     "font-family" = "serif",
    #                                     "border-color" = "rgba(0,0,0,1)"
    #                                   )))
    # }
    # # None selected -------------
    # else if(!"Measurement value" %in% input$int.map_groups && !"Measurement density" %in% input$int.map_groups){
    #   map %>%
    #     clearImages() %>%
    #     addLabelOnlyMarkers(
    #       lng = -75.15, lat = 40.00,
    #       label = "Please select either \"Measurement value\" or \"Measurement density\".",
    #       layerId = "null3",
    #       labelOptions = labelOptions(noHide = TRUE,
    #                                   style = list(
    #                                     "color" = "red",
    #                                     "font-size" = "20px",
    #                                     "font-family" = "serif",
    #                                     "border-color" = "rgba(0,0,0,1)"
    #                                   )))
    # }
    
    ##Save raster data for downloading
    download.ras.df <- unique(na.omit(brick(map.layer) %>% rasterToPoints()))
    
    colnames(download.ras.df)[1:ncol(download.ras.df)] <- c("Longitude","Latitude",meas.text)
    
    assign('download.ras.df', as_tibble(download.ras.df[,c(2,1,3)]), envir = .GlobalEnv)
    
    #Download raster button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(strftime(Sys.time(), format = '%Y%m%d%H%M%S'), "-",meas.text,"-RasterData.csv")
      },
      content = function(file) {
        withProgress(message = 'Preparing download...', 
                     write.csv(download.ras.df, file, row.names = FALSE)
        )})
    
  }) #End observeEvent for switching between variables in map
  
  #show download on Go
  observeEvent(input$go, {
    output$download.ras <- renderUI({
      downloadButton("downloadData","Download Data",class = "btn-primary")
    })
  })
  
}) #End server function