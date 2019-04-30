
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
tf <- melt(df %>% dplyr::select(Temperature,Humidity,DustPM, Name, AirQuality,Latitude,Longitude),id = c("Name","Latitude","Longitude"))
names(tf) <- c("Name","Latitude","Longitude","Variables","Measurement")


#BARPLOT function
barplot_func <- function(x,data){
  g1 <- ggplot(data, aes_string(x=x)) + geom_bar(stat="count",aes_string(fill=x)) + scale_fill_manual(values=sample(colours, length(levels(data[[x]])))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
  return(g1) 
  
}

barplot_both_func <- function(x,y,data){
  g1 <- ggplot(data, aes_string(x=x,y=y)) + geom_bar(stat="identity",aes_string(fill=x)) + scale_fill_manual(values=sample(colours, length(levels(data[[x]])))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
  return(g1) 
  
}

##HISTOGRAM##
#function
hist_func <- function(Con,data,var){
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
boxplot_func <- function(var,age,data){ggplot(data, aes_string(x=var,y=age,fill=var)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18") + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=var, y=age) + geom_jitter() + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))}

boxplot_func_ap <- function(var,age,data){ggplot(data, aes_string(x=var,y=age,fill=var)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18",fill="#66A61E") + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=" ", y=age) + geom_jitter() + theme_bw() + 
    theme(legend.position="none",
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))}



