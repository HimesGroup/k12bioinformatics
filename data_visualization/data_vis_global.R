
#########################
## Read in data files ##
########################
pheno_QC <- read.table("../gene_expression/databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0) %>% rename(ScanDate=ScanDate_Group)
pheno_QC$ScanDate <- as.factor(gsub("/([^/]*)$", " ", pheno_QC$ScanDate))
pheno_QC$Treatment <- as.factor(gsub("_","-",pheno_QC$Treatment))
pheno_QC$Donor <- as.factor(paste0("D",pheno_QC$Donor))


###############
## FUNCTIONS ##
###############

##BARPLOT##
barplot_func <- function(x,data){
  color_status <- set_colors(data)
  g1 <- ggplot(data, aes_string(x=x,fill=x)) + geom_bar(stat="count") + 
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(y) color_status[[y]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
  return(g1) 
  
}

#sample(colours, length(levels(data[[x]])))
barplot_both_func <- function(x,y,data){
  color_status <- set_colors(data)
  g1 <- ggplot(data, aes_string(x=x,y=y,fill=x)) + geom_bar(stat="identity") +  
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(m) color_status[[m]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
  return(g1) 
  
}

##BOXPLOT##
boxplot_func <- function(x,y,data){
  color_status <- set_colors(data)
  ggplot(data, aes_string(x=x,y=y,fill=x)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18") + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=x, y=y) + geom_jitter() + 
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(m) color_status[[m]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))}

##HISTOGRAM##
hist_func <- function(Con,data){
  df <- data[[Con]]
  bw <- (2 * IQR(df)) / length(df)^(1/3) #
  ggplot(data=data, aes_string(x=Con)) + geom_histogram(col="#D55E00", fill="#56B4E9",alpha=0.6,binwidth = bw) + labs(x=Con, y="Count") +  
    theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14)) ##0072B2 #xlim: 35,50
}

########################
## HELPER FUNCTIONS  ##
########################

get_discrete_var <- function(data){
  disc_var_lst <- list()
  all_vars <- names(data)[sapply(data, class) == "factor"]
  for (i in seq(1,length(all_vars))){
    len = length(levels(data[[all_vars[i]]]))
    if (len >=2 && len <=10){
      disc_var_lst[i] = all_vars[i]}
  }
  return(unlist(disc_var_lst))
}

#set colors
color_status <- colours <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA") #CB and Dark2

set_colors <- function(data){
  choices = get_discrete_var(data)
  color_list = list()
  for (i in choices) { 
    color_list =  append(color_list,levels(data[[i]]))
  }
  names(color_status) <- color_list
  
  return(color_status)
}


