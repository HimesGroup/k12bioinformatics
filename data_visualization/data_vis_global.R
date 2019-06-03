
########################
## HELPER FUNCTIONS  ##
########################

get_discrete_var <- function(data){
  disc_var_lst <- list()
  vars1 <- names(data)[sapply(data, class) == "factor"]
  toMatch <- c("NAME","CODE","LATITUDE","LONGITUDE")
  vars2 <- names(data)[grep(paste(toMatch,collapse="|"),names(data),ignore.case = TRUE)]
  vars3 <- names(data)[grep("ID",names(data))]
  all_vars <- unique(append(vars1,append(vars2,vars3)))
  
  for (i in seq(1,length(all_vars))){
    len = length(unique(data[[all_vars[i]]]))
    if (len >=2 && len <=15){
      #if (len <=15){
      disc_var_lst[i] = all_vars[i]}
  }
  return(unlist(disc_var_lst))
}

get_cont_var <- function(data){
  cont_var_lst <- list()
  vars1 <- names(data)[sapply(data, class) != "factor"]
  toMatch <- c("NAME","CODE","LATITUDE","LONGITUDE")
  vars2 <- names(data)[grep(paste(toMatch,collapse="|"),names(data),ignore.case = TRUE)]
  vars3 <- names(data)[grep("ID",names(data))]
  all_vars <- setdiff(vars1,append(vars2,vars3))
  
  for (i in seq(1,length(all_vars))){
    len = length(unique(data[[all_vars[i]]]))
    if (len >=2 && len <=15){
      cont_var_lst[i] = all_vars[i]}
  }
  return(unlist(cont_var_lst))
}


###############
## FUNCTIONS ##
###############

#set colors
colours <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                             "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA",  #CB and Dark2
                             brewer.pal(11, "Spectral"),
                             brewer.pal(11, "Set3"),
                             brewer.pal(11, "Paired")) 

get_data <- function(data){
  choices = get_discrete_var(data)
  data[choices] <- lapply(data[choices], factor) 
  return(data)
}

set_colors <- function(data){
  choices = get_discrete_var(data)
  data <- get_data(data)
  color_list = list()
  for (i in choices) { 
    color_list =  append(color_list,levels(data[[i]]))
  }
  col = colours[1:length(color_list)]
  names(col) <- color_list
  return(col)
}


##BARPLOT##
barplot_func <- function(x,data){
  data <- get_data(data)
  color_status <- set_colors(data)
  g1 <- ggplot(data, aes_string(x=x,fill=x)) + geom_bar(stat="count") + 
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(y) color_status[[y]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13))
  return(g1) 
  
}

barplot_func_dodge <- function(x,a,data){
  data <- get_data(data)
  color_status <- set_colors(data)
  g1 <- ggplot(data, aes_string(x=x,fill=a)) + geom_bar(stat="count",position=position_dodge(preserve = "single")) + 
    scale_fill_manual(values=unlist(lapply(levels(data[[a]]), function(y) color_status[[y]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13))
  return(g1) 
  
}

#sample(colours, length(levels(data[[x]])))
barplot_both_func <- function(x,y,data){
  data <- get_data(data)
  color_status <- set_colors(data)
  g1 <- ggplot(data, aes_string(x=x,y=y,fill=x)) + geom_bar(stat="identity") +  
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(m) color_status[[m]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13))
  return(g1) 
  
}

##BOXPLOT##
boxplot_func <- function(x,y,data){
  data <- get_data(data)
  color_status <- set_colors(data)
  ggplot(data, aes_string(x=x,y=y,fill=x)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18",na.rm=TRUE) + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=x, y=y) + geom_jitter(size=1,position = position_jitter(width=0.2)) + 
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(m) color_status[[m]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13))}

##HISTOGRAM##
hist_func <- function(Con,data){
  data <- get_data(data)
  df <- data[[Con]]
  bins = round(sqrt(length(df)))
  #bw <- nclass.Sturges(df)
  #bw <- (2 * IQR(df))/ length(df)^(1/3) #Freedman–Diaconis rule 
  #breaks <- pretty(range(df), n = nclass.FD(df), min.n = 1)
  #bwidth <- breaks[2]-breaks[1]
  ggplot(data=data, aes_string(x=Con)) + geom_histogram(col="#D55E00", fill="#56B4E9",alpha=0.6,bins = bins ,na.rm=TRUE) + labs(x=Con, y="Count") +  
    theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13)) ##0072B2 #xlim: 35,50
}

