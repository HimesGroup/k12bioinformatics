library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(gsheet)
library(tidyr)
library(reshape2)
library(dplyr)
library(RCurl) #to check if url exists
library(lubridate)
library(raster)
library(mapview)
library(data.table)
library(feather)
library(DescTools)
library(stringr)
library(tools)
library(gstat)
library(prevR)
library(leafsync)
library(dplyr)
library(viridis)
library(tidyverse)
library(RColorBrewer)
library(gdtools)
library(ggiraph)
library(RCurl)
library(RJSONIO)
library(parallel)

########################################
## Acquiring data from google sheets ##
#######################################

##Get groups with corresponding URLs
#URL <- "https://docs.google.com/spreadsheets/d/1V5J_TuhfZTFBfPcg1JMavzFrbB2vavd3JMNX1f1oAQw/edit#gid=420394624"
#URL <- "https://docs.google.com/spreadsheets/d/13-F4sAcX5Ph-IKp0W8uTRfT1RmUeEGbwtK7PMVUylws/edit#gid=0"
gsheet_links <- read.csv("databases/gsheet_links_final_JS_July28.csv", as.is=TRUE)
#gsheet_links <- gsheet_links %>% dplyr::filter(Group!="J R Masterman 2020-2021")

#gsheet_links <- read.csv("databases/gsheet_links_final_JS.csv", as.is=TRUE)[1:9,]
#gsheet_links <- gsheet_links %>% dplyr::filter(!Group%in%c("J R Masterman 2020-2021","Maritime Academy Charter HS 2020-2021","Dock Mennonite Academy Offline 2020-2021"))
message = NULL
group_status = FALSE

#Function to handle errors with loading google spreadsheet 
get_gsheet_data <- function(link) {
  curr_message = NULL
  #Case: there is a problem with the google doc url or the column names 
  if(url.exists(link)){
    df <- gsheet2tbl(link)
    #df <- df[,colSums(is.na(df))<nrow(df)]
    if(length(setdiff(c("Timestamp","Name","PM2.5","CO","Location (1,2,3,4)","Comments","Site Type (Indoor/Outdoor)", "Location (Latitude, Longitude)"), names(df)))!= 0){ #"Data_Collection_Time",
      curr_message = paste("Warning: the google sheet did not load properly so it will be skipped:", link)
    } 
  } else if(!url.exists(URL)){
    curr_message = paste("Warning: the google sheet did not load properly so it will be skipped:", link)
  }
  return(df)
}

#Load data from all the google sheets
all_crowdsourced_data <- NULL
groups <- as.vector(gsheet_links$Group)
for (i in c(1:dim(gsheet_links)[[1]])) {
  curr_data <- get_gsheet_data(gsheet_links[i, "URL"])
  if(dim(curr_data)[1]!=0){
    group_status = TRUE
    curr_data <- dplyr::mutate(curr_data, Group = gsheet_links[i, "Group"]) 
    curr_data <- curr_data[, -grep("Class", colnames(curr_data))]
    
    #Set Pollutants
    curr_data$PM2.5 <- as.numeric(curr_data$PM2.5)
    curr_data$CO <- as.numeric(curr_data$CO)
    curr_data$Temperature <- as.numeric(curr_data$Temperature)
    curr_data$Humidity <- as.numeric(curr_data$Humidity)
    
    #Set Name column
    curr_data$Name <- as.character(curr_data$Name)
    
    #Correct colnames format
    colnames(curr_data)[grepl("Site", names(curr_data))] <- "Site Type (Indoor/Outdoor)"
    colnames(curr_data)[grepl("LongLat", names(curr_data))] <- "Location (Latitude, Longitude)"
    colnames(curr_data)[grepl("Location_1234", names(curr_data))] <- "Location (1,2,3,4)"
    colnames(curr_data)[grepl("Group", names(curr_data))] <- "Group"
    colnames(curr_data)[grepl("Data Collection Time", names(curr_data))] <- "Data_Collection_Time"
    colnames(curr_data)[grepl("Raw Data", names(curr_data))] <- "Raw_Data"
    
    #Set Raw Data column 
    if(!is_empty(colnames(curr_data)[grepl("Raw Data", names(curr_data))])){
      colnames(curr_data)[grepl("Raw Data", names(curr_data))] <- "Raw_Data"
    } else if(is_empty(colnames(curr_data)[grepl("Raw_Data", names(curr_data))])){
      curr_data$Raw_Data <- NA
    }

    all_crowdsourced_data <- bind_rows(all_crowdsourced_data, curr_data)
    all_crowdsourced_data <- na.omit(all_crowdsourced_data %>% dplyr::select(-Raw_Data))
  } else{
    next
  }
}

if (dim(all_crowdsourced_data)[1]==0){
  all_crowdsourced_data <- readRDS("databases/AirQualityData_27thJan_2020.RDS")
  message = "Warning: no contents in provided google sheets so old data loaded"
  group_status = FALSE
}

## Wrangle dataframe into customized format
all_crowdsourced_data <- tidyr::separate(data=all_crowdsourced_data,
                                         col="Location (Latitude, Longitude)",
                                         into=c("Latitude", "Longitude"),
                                         sep=" ",
                                         remove=FALSE)
all_crowdsourced_data$Latitude <- as.numeric(all_crowdsourced_data$Latitude)
all_crowdsourced_data$Longitude <- as.numeric(all_crowdsourced_data$Longitude)
all_crowdsourced_data$Group <- as.factor(all_crowdsourced_data$Group)
all_crowdsourced_data$Date <- gsub(" .*"," ", all_crowdsourced_data$Data_Collection_Time)
all_crowdsourced_data$Time <- gsub(".* ","", all_crowdsourced_data$Data_Collection_Time)
all_crowdsourced_data$Date <-  gsub("*.EDT","",strptime(as.character(all_crowdsourced_data$Date), "%m/%d/%Y"))
all_crowdsourced_data <- filter(all_crowdsourced_data, PM2.5<500 & PM2.5>=0)

#Dataframe with column names as row names for "Variables" column
tf <- reshape2::melt(all_crowdsourced_data %>% dplyr::select(Group,Name,Latitude,Longitude,Date,Time,"Site Type (Indoor/Outdoor)",Comments,PM2.5,CO), id=c("Group","Name","Latitude","Longitude","Date","Time","Site Type (Indoor/Outdoor)","Comments"))
names(tf) <- c("Group", "Name", "Latitude", "Longitude", "Date", "Time", "Site_Type", "Comments", "Variables", "Measurement")
clrs <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA") #CB and Dark2

#Debug new data error
tf$Measurement <- as.numeric(tf$Measurement)
tf <- tf %>% dplyr::filter(Measurement <= 100)

##########################################
## Acquiring EPA PM2.5 and CO datasets ##
#########################################

#Get PM2.5 data from EPA file
k12_df <- read.csv("databases/k12_sites.csv")
ph_df <- read.csv("databases/all_k12_sites_PM.csv")
months <- c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sept","Oct","Nov","Dec")
ph_df$Year <- as.factor(ph_df$Year)
ph_df$State <- as.factor(ph_df$State)
ph_df$Month <- factor(ph_df$Month,levels=months)

#Get CO data from EPA file
co_df <- read.csv("databases/all_k12_sites_CO.csv")
co_df$Year <- as.factor(co_df$Year)
co_df$State <- as.factor(co_df$State)
co_df$Month <- factor(co_df$Month,levels=months)

#EPA daily average over Sept 2017
EPA_data_file <- read.csv("databases/EPA_measures_daily_average_Sept2017.csv")

####################
## VISUALIZATION ##
####################

#Set choices of cities for UI
cities <- c("Los Angeles, CA"="CA", "Miami, FL"="FL", "Billings, MO"="MO", "Standing Rock, NM"="NM",
            "Midtown Manhattan, NY"="NY","Portland, OR"="OR","Philadelphia, PA"="PA",
            "Pierre, SD"="SD")

##Set colors
color_status <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")
names(color_status) <- as.vector(unique(tf$Variables))


##HISTOGRAM##
#function
#histogram for selected continuous variables from student data
hist_func <- function(Con, var,dataset){
  data <- dataset %>% dplyr::filter(Variables %in% var)
  df <- data[[Con]]
  bw <- (2 * IQR(df)) / length(df)^(1/3) #Freedman-Diaconis rule 
  #breaks=seq(min(df), max(df)),
  ggplot(data=data, aes_string(Con)) + geom_histogram(col="grey18", fill=color_status[[var]], alpha=0.6, binwidth = bw) + labs(x=var, y="Count") + #xlim(c(min(df),max(df))) + 
    theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14)) ##0072B2 #xlim: 35,50
}


##BOXPLOT##
#function
#boxplot for selected variables from student data
boxplot_func_ap <- function(x,y,var,dataset){
  data <- dataset %>% dplyr::filter(Variables %in% var)
  ggplot(data, aes_string(x=x,y=y)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18",fill=color_status[[var]]) + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=" ", y=y) + geom_jitter(size=1,position = position_jitter(width=0.2)) + theme_bw() + 
    theme(axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))}

##SCATTERPLOT##
#Bivariate scatterplot for PM2.5 and CO, colored by date of entry and shapes by name of student.
scatterplot_func <- function(var, pvar){
  #dtt <- tf %>% dplyr::select(Name,Latitude,Longitude,Date,Time)
  #data <- merge(all_crowdsourced_data,dtt,by=c("Name","Latitude","Longitude","Date","Time")) #Get values from selected input
  data <- all_crowdsourced_data
  sval = rep(0:25,10)
  ggplot(data, aes_string(x=var, y=pvar)) + geom_point(aes(color=Date,shape = Group),size=3) + theme_bw() + scale_shape_manual(values=sval[0:length(data$Group)]) + 
    scale_x_continuous(name="PM2.5",limits = c(0,50))+ #, breaks=c("0","10","15")
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14),)
}

##BARPLOT##
#Barplot of PM.25 values for all 8 cities for Sept 2017
col_status = rev(c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"))
barplot_func <- function(data){
  ggplot(data, aes(x=City, y=PM, fill=PM)) + geom_bar(stat="identity",colour="black") + #scale_fill_viridis_c(option = "inferno",direction = 1) + 
    scale_fill_gradientn(colours=rev(col_status)) +
    labs(x="", y="PM2.5 (μg/m3)") + 
    scale_y_continuous(breaks=scales::pretty_breaks(n=10)) +
    geom_text(aes(label=round(PM,2)), vjust=-0.3) +
    theme_bw() + 
    theme(
      legend.position = "none",
      axis.title=element_text(size=16),
      axis.text.x=element_text(size=16,angle=45, hjust=1),
      axis.text.y=element_text(size=16),
      panel.border = element_blank(), 
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"))
}

##Ggiraph SCATTERPLOT##
#Interactive scatterplot for PM.25 or CO values
scatplot_func_ph <- function(data, title){
  col = brewer.pal(11, "Spectral")
  #col =  col[!col %in% "#FFFFBF"]
  data$PM <- round(data$PM2.5,2)
  ggplot(data, aes(x=Month, y=PM2.5, group=Year,colour=Year,tooltip = PM)) + geom_point_interactive() + geom_line() + 
    labs(x="",y=title) + #scale_color_manual(values = clrs[1:12]) +
    scale_color_manual(values=col) + 
    theme_bw() + 
    theme(legend.text = element_text(size=10),
          axis.title=element_text(size=10),
          axis.text=element_text(size=10)) 
  
}

#############################################
## SAPPHHIRINE : CROWDSOURCED SENSOR DATA ##
#############################################

app.data <- read_feather("sapphirine_data/local_data.feather")

hours <- c("00:00",
           "01:00",
           "02:00",
           "03:00",
           "04:00",
           "05:00",
           "06:00",
           "07:00",
           "08:00",
           "09:00",
           "10:00",
           "11:00",
           "12:00",
           "13:00",
           "14:00",
           "15:00",
           "16:00",
           "17:00",
           "18:00",
           "19:00",
           "20:00",
           "21:00",
           "22:00",
           "23:00",
           "23:59"
)

mins <- as_datetime(hm('0:00')): as_datetime(hm('23:59')) %>%
  as_datetime() %>%
  strftime(format = '%H:%M') %>%
  unique() %>%
  sort()

#Creates character list that will be indexed for temporal subsetting
#Use UTC for online and New York for local

sensor.measures <- c("Temperature", "Humidity", "PM1", "PM2.5", "PM10")
other.measures <- c("Traffic","CO (EPA)") #"Crime", "Area Deprivation Index", 
all.measures <- c(sensor.measures, other.measures)

#Subscript version of measurements
sensor.measures.sub <- c("Temperature", "Humidity", "PM\u2081", 
                         "PM\u2082.\u2085", "PM\u2081\u2080")
all.measures.sub <- c(sensor.measures.sub, other.measures)

titles.list <- c("Avg. Temp. (\u00B0C)", "Avg. Humidity (%)", 
                 "Avg. PM\u2081 Conc. (\u03BCg/m\u00B3)", 
                 "Avg. PM\u2082.\u2085 Conc. (\u03BCg/m\u00B3)", 
                 "Avg. PM\u2081\u2080 Conc. (\u03BCg/m\u00B3)", 
                 "Avg. AADT",
                 "Avg CO Conc. (ppm)")
                # "# of Reported Crimes", "Avg. ADI", 

suffix.list <- c(".t", ".h", ".pm1", ".pm2.5", ".pm10",".tr",".co") #".c", ".pov",

titles.df <- data.frame(cbind(all.measures, titles.list, suffix.list))

epa.titles.df <- cbind(c("SO2", "NO2", "O3", "CO", "PM2.5", "PM10"),
                       c("Avg. SO\u2082 Conc. (ppb)", "Avg. NO\u2082 Conc. (ppb)", 
                         "Avg. O\u2083 Conc. (ppb)", "Avg CO Conc. (ppm)",
                         "Avg. PM\u2082.\u2085 Conc. (\u03BCg/m\u00B3)", 
                         "Avg. PM\u2081\u2080 Conc. (\u03BCg/m\u00B3)")
)

f.titles <- function(y){
  index <- which(titles.df[,1] == y)
  return(titles.df[index, 2])
}

f.plaintext <- function(b){
  if(length(b) > 0){
    if(b == 'PM\u2082.\u2085'){return('PM2.5')}
    else if(b == 'PM\u2081'){return('PM1')}
    else if(b == 'PM\u2081\u2080'){return('PM10')}
    else{return(b)}
  } else{return(b)}
}

f.titles.d <- function(w){paste("log\u2081\u2080 # of", w, "data points")}

f.titles.epa <- function(a){
  index <- which(epa.titles.df[,1] == a)
  return(epa.titles.df[index, 2])
}

f.suffix <- function(z){
  index <- which(titles.df[,1] == z)
  return(titles.df[index, 3])
}

f.zoom <- function(x, y){
  val <- ifelse(x > y, x, y)
  return(as.integer(round(11.47 - 1.5*val, digits = 0)))
}

f.top <- function(x){
  no.string <- toString(as.integer(x))
  lead.digit <- as.numeric(substr(no.string, 1, 1))
  no.digits <- nchar(no.string)
  if(lead.digit == 1){
    if(x == 100){
      return(100)
    }
    else{
      return(RoundTo(x, multiple = 2*10**(no.digits - 2), FUN = ceiling))
    }
  }
  else if(lead.digit >= 2 && lead.digit <= 4){
    return(RoundTo(x, multiple = 5*10**(no.digits - 2), FUN = ceiling))
  }
  else if(lead.digit >= 5){
    return(RoundTo(x, multiple = 10**(no.digits - 1), FUN = ceiling))
  }
}

#pov.shp <- shapefile("sapphirine_data/ADI_data/ADI_data.shp")

our.sensors <- fread("sapphirine_data/LIMEA_AIRBEAM_SUMMARY.csv", 
                     header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]
our.sensors <- paste0("AirBeam:", our.sensors)

sensor.names <- levels(app.data$Sensor.ID)

gpa.borders <- shapefile("sapphirine_data/gpa_counties/gpa_counties.shp") %>%
  spTransform(CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
county.borders <- gpa.borders[gpa.borders$NAME == "Philadelphia",]

city.border <- county.borders[county.borders$NAME == 'Philadelphia',]

traffic.raster <- raster("sapphirine_data/traffic/traffic_raster.grd")

zipcodes <- read.csv("databases/zipcodes_lat_lon.csv")

#Read in EPA data frames
EPA_data <- read_feather('sapphirine_data/EPA_data.feather')

#EPA raster function
getEPAraster <- function(variable, dates){
  
  col.name <- names(EPA_data)[grep(variable, names(EPA_data))]
  
  dat <- EPA_data %>%
    dplyr::select(1:9, .data[[col.name]]) %>%
    dplyr::filter(Date %in% dates) %>%
    dplyr::filter(!is.na(.data[[col.name]]), .data[[col.name]] >= 0)
  
  assign('epa.df', dat, envir = .GlobalEnv) 
  #Creates subsetted frame to be downloaded
  
  dat <- dat %>%
    dplyr::group_by(Latitude, Longitude) %>%
    dplyr::summarise(avg = mean(.data[[col.name]], na.rm = TRUE))
  
  ## Gives averages of daily-averaged values over range of dates (mean of mean-values)
  ## Get one row per location by averaging all entries
  
  if(nrow(dat) > 0){
    coordinates(dat) = ~Longitude+Latitude
    crs(dat) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    ## make base raster
    #find distance between the latitude and longitudes and convert to km (*111 for 1km)
    r <- raster(nrow = 451, ncol = 736, extent(-80.51985, -73.88506, 38.45113, 42.51607)) #1km
    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    ## generate raster (idw with 5 nearest sites)
    gs <- gstat(formula=avg~1, data=dat, nmax = 5)
    nn <- interpolate(r, gs)
    
    return(nn)
  } else{
    print("zero")
    #find distance between the latitude and longitudes and convert to km (*111 for 1km)
    r <- raster(nrow = 451, ncol = 736, extent(-80.51985, -73.88506, 38.45113, 42.51607)) #1km
    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    values(r) <- rep(-1, 331936)
    return(r)
  }
}

#Function for adding circles to EPA monitor legend
addLegendEPA <- function(map, colors, labels, sizes, opacity = 0.8){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, 
                           "px;margin-top: 4.5px;height:", sizes, "px")  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, 
                           "px;margin-top: 0px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}


#Reverse legend direction
myLabelFormat = function(prefix = "", suffix = "", between = " &ndash; ", digits = 3, 
                         big.mark = ",", transform = identity, t.val = Inf) {
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE, 
           big.mark = big.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      cuts <- sort(cuts, decreasing = T) #just added
      paste0(prefix, formatNum(cuts), ifelse(cuts == t.val, "+", ""))
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), 
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n], 
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}

#Get Zipcodes from latitude and longitude
latlon2zip <- function(lat, lon) {
  url <- sprintf("http://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f&zoom=18&addressdetails=1", lat, lon)
  res <- fromJSON(url)
  return(res[["address"]][["postcode"]])
}

