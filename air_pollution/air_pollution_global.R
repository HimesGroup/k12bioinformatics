
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


