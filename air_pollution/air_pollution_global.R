library(gsheet)
library(tidyr)
library(reshape2)
library(dplyr)
library(RCurl) #to check if url exists

########################################
## Acquiring data from google sheets ##
#######################################

##Get groups with corresponding URLs
#URL <- "https://docs.google.com/spreadsheets/d/1V5J_TuhfZTFBfPcg1JMavzFrbB2vavd3JMNX1f1oAQw/edit#gid=420394624"
#URL <- "https://docs.google.com/spreadsheets/d/13-F4sAcX5Ph-IKp0W8uTRfT1RmUeEGbwtK7PMVUylws/edit#gid=0"
gsheet_links <- read.csv("../databases/gsheet_links.csv", as.is=TRUE)
message = NULL
group_status = FALSE
  
#Function to handle errors with loading google spreadsheet 
get_gsheet_data <- function(link) {
  curr_message = NULL
  #Case: there is a problem with the google doc url or the column names 
  if(url.exists(link)){
    df <- gsheet2tbl(link)
    if(length(setdiff(c("Timestamp","Name","PM2.5","CO","Location (1,2,3,4)","Comments","Site_Type (Indoor, Outdoor)","Data_Collection_Time", "Location (Latitude, Longitude)"), names(df)))!= 0){
      curr_message = paste("Warning: the google sheet did not load properly so it will be skipped:", link)
    } 
  } else if(!url.exists(URL)){
    curr_message = paste("Warning: the google sheet did not load properly so it will be skipped:", link)
  }
  return(df)
}

#Load data from all the google sheets
all_crowdsourced_data <- NULL
for (i in c(1:dim(gsheet_links)[[1]])) {
  curr_data <- get_gsheet_data(gsheet_links[i, "URL"])
  if(dim(curr_data)[1]!=0){
    group_status = TRUE
    curr_data <- mutate(curr_data, Group = gsheet_links[i, "Group"])
    all_crowdsourced_data <- bind_rows(all_crowdsourced_data, curr_data)
  } else{
    next
  }
}

if (dim(all_crowdsourced_data)[1]==0){
  all_crowdsourced_data <- readRDS("../databases/AirQualityData_15thJuly_2019.RDS")
  message = "Warning: no contents in provided google sheets so old data loaded"
  group_status = FALSE
}

# #Error handling 
# message = NULL
# 
# #Case: there is a problem with the google doc url or the column names 
# if(url.exists(URL)){
#   df <- gsheet2tbl(URL)
#   if(length(setdiff(c("Timestamp","Name","Location","PM2.5","CO","Site_Type","Comment"),names(df)))!= 0){
#     message = "Warning: google sheet did not load properly so a saved version is being used instead"
#   } 
# } else if(!url.exists(URL)){
#   message = "Warning: google sheet did not load properly so a saved version is being used instead"
# }
# 
# #Read in saved version
# if (!is.null(message)){
#   df <- readRDS("../databases/AirQualityData_15thJuly_2019.RDS")
# }

## Wrangle dataframe in customized format
all_crowdsourced_data <- tidyr::separate(data=all_crowdsourced_data,
                       col="Location (Latitude, Longitude)",
                       into=c("Latitude", "Longitude"),
                       sep=",",
                       remove=FALSE)
all_crowdsourced_data$Latitude <- as.numeric(all_crowdsourced_data$Latitude)
all_crowdsourced_data$Longitude <- as.numeric(all_crowdsourced_data$Longitude)
all_crowdsourced_data$Date <- gsub(" .*"," ", all_crowdsourced_data$Data_Collection_Time)
all_crowdsourced_data$Time <- gsub(".* ","", all_crowdsourced_data$Data_Collection_Time)
all_crowdsourced_data$Date <-  gsub("*.EDT","",strptime(as.character(all_crowdsourced_data$Date), "%m/%d/%Y"))
all_crowdsourced_data <- filter(all_crowdsourced_data, PM2.5<500 & PM2.5>=0)

#Dataframe with column names as row names for "Variables" column
tf <- melt(all_crowdsourced_data %>% dplyr::select(Group,Name,Latitude,Longitude,Date,Time,"Site_Type (Indoor, Outdoor)",Comments,PM2.5,CO), id=c("Group","Name","Latitude","Longitude","Date","Time","Site_Type (Indoor, Outdoor)","Comments"))
names(tf) <- c("Group", "Name", "Latitude", "Longitude", "Date", "Time", "Site_Type", "Comments", "Variables", "Measurement")
clrs <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA") #CB and Dark2


##########################################
## Acquiring EPA PM2.5 and CO datasets ##
#########################################

#Get PM2.5 data from EPA file
k12_df <- read.csv("../databases/k12_sites.csv")
ph_df <- read.csv("../databases/all_k12_sites_PM.csv")
months <- c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sept","Oct","Nov","Dec")
ph_df$Year <- as.factor(ph_df$Year)
ph_df$State <- as.factor(ph_df$State)
ph_df$Month <- factor(ph_df$Month,levels=months)

#Get CO data from EPA file
co_df <- read.csv("../databases/all_k12_sites_CO.csv")
co_df$Year <- as.factor(co_df$Year)
co_df$State <- as.factor(co_df$State)
co_df$Month <- factor(co_df$Month,levels=months)

#EPA daily average over Sept 2017
EPA_data_file <- read.csv("../databases/EPA_measures_daily_average_Sept2017.csv")

####################
## VISUALIZATION ##
####################

##Set colors
color_status <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")
names(color_status) <- as.vector(unique(tf$Variables))


##HISTOGRAM##
#function
#histogram for selected continuous variables from student data
hist_func <- function(Con, var){
  data <- tf %>% dplyr::filter(Variables %in% var)
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
#histogram for selected variables from student data
boxplot_func_ap <- function(x,y,var){
  data <- tf %>% dplyr::filter(Variables %in% var)
  ggplot(data, aes_string(x=x,y=y)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18",fill=color_status[[var]]) + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=" ", y=y) + geom_jitter(size=1,position = position_jitter(width=0.2)) + theme_bw() + 
    theme(axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))}

##SCATTERPLOT##
#Bivariate scatterplot for PM2.5 and CO, colored by date of entry and shapes by name of student.
scatterplot_func <- function(var, pvar){
  data <- all_crowdsourced_data %>% dplyr::select(var,pvar)
  sval = rep(0:25,10)
  ggplot(all_crowdsourced_data, aes_string(x=var, y=pvar)) + geom_point(aes(color=Date,shape = Name),size=3) + theme_bw() + scale_shape_manual(values=sval[0:length(all_crowdsourced_data$Name)]) + 
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
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

## Making rasters of data

# library(raster)
# library(dplyr)
# library(leaflet)
# library(rgdal)
# 
# 
# mdata <- tf #%>% dplyr::filter(Variables == "Humidity") 
# r <- raster(xmn = -124, xmx = -66, ymn = 17.9, ymx = 50, nrows = 20, ncols = 20)
# map.layer.h <- rasterize(mdata[,3:2], r, mdata[which(mdata$Variables=="Humidity"),"Measurement"], fun = mean, na.rm = TRUE)
# map.layer.d <- rasterize(mdata[,3:2], r, mdata[which(mdata$Variables=="DustPM"),"Measurement"], fun = mean, na.rm = TRUE)
# map.layer.t <- rasterize(mdata[,3:2], r, mdata[which(mdata$Variables=="Temperature"),"Measurement"], fun = mean, na.rm = TRUE)
# map.layer.a <- rasterize(mdata[,3:2], r, mdata[which(mdata$Variables=="AirQuality"),"Measurement"], fun = mean, na.rm = TRUE)
# 
# leaflet(data=mdata) %>% addTiles() %>%
#   addRasterImage(map.layer.a,colors = "BuPu",opacity = 0.8)

