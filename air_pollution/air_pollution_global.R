
##Get data
URL <- "https://docs.google.com/spreadsheets/d/1V5J_TuhfZTFBfPcg1JMavzFrbB2vavd3JMNX1f1oAQw/edit#gid=420394624"
df <- gsheet2tbl(URL)
df <- tidyr::separate(data=df,
                      col=Location,
                      into=c("Latitude", "Longitude"),
                      sep=",",
                      remove=FALSE)
df$Latitude <- stringr::str_replace_all(df$Latitude, "[(]", "")
df$Longitude <- stringr::str_replace_all(df$Longitude, "[)]", "")
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df <- df %>% dplyr::select(-ACTION)
df$Date <- gsub(" .*"," ", df$Timestamp)
df$Time <- gsub(".* ","", df$Timestamp)
df$Date <-  gsub("*.EDT","",strptime(as.character(df$Date), "%m/%d/%Y"))
tf <- melt(df %>% dplyr::select(Temperature,Humidity,DustPM, Name, AirQuality,Latitude,Longitude,Date,Timestamp),id = c("Name","Latitude","Longitude","Date","Timestamp"))
names(tf) <- c("Name","Latitude","Longitude","Date","Timestamp","Variables","Measurement")
clrs <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA") #CB and Dark2


#Get PM2.5 data from EPA file
k12_df <- read.csv("../databases/k12_sites.csv")
ph_df <- read.csv("../databases/philadelphia_PM.csv")
months <- c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sept","Oct","Nov","Dec")
ph_df$Year <- as.factor(ph_df$Year)
ph_df$Month <- factor(ph_df$Month,levels=months)


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
  ggplot(data=data, aes_string(Con)) + geom_histogram(col="#D55E00", fill="#56B4E9",alpha=0.6,binwidth = bw) + labs(x=var, y="Count") + #xlim(c(min(df),max(df))) + 
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
barplot_func <- function(data){
  ggplot(data, aes(x=State, y=PM, fill=PM)) + geom_bar(stat="identity") + scale_fill_viridis_c(option = "inferno",direction = -1) + 
    labs(x="",y="PM 2.5 (Sept 2017)") + 
    theme_bw() + theme(legend.position = "none",axis.title=element_text(size=12),axis.text=element_text(size=12))
}

#Barplot II
scatplot_func_ph <- function(data){
  data$PM <- round(data$PM2.5,2)
  ggplot(data, aes(x=Month, y=PM2.5, group=Year,colour=Year,tooltip = PM)) + geom_point_interactive() + geom_line() + 
    labs(x="",y="PM 2.5 levels") + scale_color_manual(values = clrs[1:12]) +
    theme_bw() + 
    theme(legend.text = element_text(size=10),
          axis.title=element_text(size=10),
          axis.text=element_text(size=10))
}

# library(pargasite)
# k12 <- read.table("k12_sites.txt",header=TRUE,sep="/t")
# long <- k12$Longitude
# lat <- k12$Latitude
# pm_list <- list()
#
# for (i in seq(1,nrow(k12))){
#   pm <- getMonthPollutionEstimate(long[i], lat[i], pollutant = "PM2.5", monthyear="09-2017")
#   pm_list[[i]] <- pm
# }
#
# k12$PM <- unlist(pm_list)
# k12$Location <- paste0(k12$City,",",k12$State)
# write.csv(k12,"k12_sites.csv",row.names = FALSE)

# library(pargasite)
# k12 <- read.csv("../databases/k12_sites.csv")
# pk12 <- k12 %>% dplyr::filter(State == "PA") %>% dplyr::select(Longitude,Latitude)
# dates <- seq(1,12)
# months <- c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sept","Oct","Nov","Dec")
# years <- seq(2007,2017)
# fdates <- unlist(lapply(years, function(x) paste0(dates,"-",x)))
# pm_list <- list()
# y_list <- list()
# count = 1
# 
# for (i in seq(1,length(fdates))){
#   pm <- getMonthPollutionEstimate(pk12$Longitude, pk12$Latitude, pollutant="PM2.5", monthyear = fdates[i])
#   pm_list[[i]] <- pm
#   if (i %% 12 == 0){
#     y_list[[i]] <- years[count]
#     count = count + 1
#   } else {
#     y_list[[i]] <- years[count]
#     }
# }
# 
# ph_df <- data.frame("Dates" = fdates, "PM2.5" = unlist(pm_list),"Year" = unlist(y_list),"Month"= months)
# write.csv(ph_df,"../databases/philadelphia_PM.csv", row.names = FALSE)



