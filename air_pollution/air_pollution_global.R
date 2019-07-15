library(gsheet)
library(tidyr)
library(reshape2)
#TO DO: add error handling in case there is a problem loading the google doc

##Get data
URL <- "https://docs.google.com/spreadsheets/d/1V5J_TuhfZTFBfPcg1JMavzFrbB2vavd3JMNX1f1oAQw/edit#gid=420394624"
df <- gsheet2tbl(URL)
df <- tidyr::separate(data=df,
                       col=Location,
                       into=c("Latitude", "Longitude"),
                       sep=",",
                       remove=FALSE)

#df <- readRDS("../databases/AirQualityData.RDS")
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
#df <- df %>% dplyr::select(-ACTION)
df$Date <- gsub(" .*"," ", df$Timestamp)
df$Time <- gsub(".* ","", df$Timestamp)
df$Date <-  gsub("*.EDT","",strptime(as.character(df$Date), "%m/%d/%Y"))
df <- filter(df, PM2.5<500)
tf <- melt(df %>% dplyr::select(Name,Latitude,Longitude,Date,Time,Site_Type,Comment,PM2.5,CO),id = c("Name","Latitude","Longitude","Date","Time","Site_Type","Comment"))
names(tf) <- c("Name", "Latitude", "Longitude", "Date", "Time", "Site_Type", "Comment", "Variables", "Measurement")
clrs <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA") #CB and Dark2


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

##Set colors
#set colors
color_status <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")
names(color_status) <- as.vector(unique(tf$Variables))


##HISTOGRAM##
#function
hist_func <- function(Con,var){
  data <- tf %>% dplyr::filter(Variables %in% var)
  df <- data[[Con]]
  bw <- (2 * IQR(df)) / length(df)^(1/3) #Freedman-Diaconis rule 
  #breaks=seq(min(df), max(df)),
  ggplot(data=data, aes_string(Con)) + geom_histogram(col="grey18", fill=color_status[[var]],alpha=0.6,binwidth = bw) + labs(x=var, y="Count") + #xlim(c(min(df),max(df))) + 
    theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14)) ##0072B2 #xlim: 35,50
}


##BOXPLOT##
#function
boxplot_func_ap <- function(x,y,var){
  data <- tf %>% dplyr::filter(Variables %in% var)
  ggplot(data, aes_string(x=x,y=y)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18",fill=color_status[[var]]) + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=" ", y=y) + geom_jitter(size=1,position = position_jitter(width=0.2)) + theme_bw() + 
    theme(axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))}

#scatterplot

scatterplot_func <- function(var,pvar){
  data <- df %>% dplyr::select(var,pvar)
  ggplot(df, aes_string(x=var, y=pvar)) + geom_point(aes(color=Date,shape = Name),size=3) + theme_bw() + scale_shape_manual(values=seq(0,length(unique(df$Name)))) + 
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
}

#Barplot I
col_status = rev(c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"))
barplot_func <- function(data){
  ggplot(data, aes(x=State, y=PM, fill=PM)) + geom_bar(stat="identity",colour="black") + #scale_fill_viridis_c(option = "inferno",direction = 1) + 
    scale_fill_gradientn(colours=rev(col_status)) +
    labs(x="",y="PM 2.5 μg/m3 (Sept 2017)") + 
    theme_bw() + 
    theme(
      legend.position = "none",
      axis.title=element_text(size=12),
      axis.text=element_text(size=12),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}

#Barplot II
scatplot_func_ph <- function(data,title){
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
#     addRasterImage(map.layer.a,colors = "BuPu",opacity = 0.8)

