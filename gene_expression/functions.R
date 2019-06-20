

#########################
## Read in data files ##
########################
rma.data <- read_feather("../databases/GSE8823_pheno+rma_counts.feather")
rma.data$Treatment <- gsub("_","-",rma.data$Treatment)
pheno_QC <- read.table("../databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0) %>% rename(ScanDate = ScanDate_Group)
pheno_QC$Treatment <- as.factor(gsub("_","-",pheno_QC$Treatment))
pheno_QC$Donor <- paste0("D",pheno_QC$Donor)

###################################
## Dataframe formatting function ##
###################################

# The datreform_func function reformats the DE table
datreform_func <- function(dt,topnum=200) {
  dt=dt[order(dt$P.Value),]
  dt=dt[1:topnum,]
  round2 <- function(x){round(x,2)}
  dt[,c("logFC","AveExpr","t","B")] <- sapply(dt[,c("logFC","AveExpr","t","B")],round2)
  sciform <- function(x){format(x,scientific=TRUE,digits =2)}
  dt[,c("P.Value","adj.P.Val","pValuesBatch","qValuesBatch")] <- sapply(dt[,c("P.Value","adj.P.Val","pValuesBatch","qValuesBatch")],sciform)
  dt <- dt %>% dplyr::select(ID,logFC,adj.P.Val,SYMBOL) %>% dplyr::rename("Probe ID" = ID, "Fold Change" = logFC, "Adjusted P-value" = adj.P.Val)
  dt
}

#######################
## Top Gene Boxplot ##
######################

# function for top gene boxplot
topgene_boxplot_func <- function(tb) { # comp: comparison status
  #row.names(rma.data.of.interest) <- gsub("_PM","",row.names(rma.data.of.interest))
  probes <- as.vector(tb$ID)
  gene <- unique(tb$SYMBOL)
  df <- rma.data %>% dplyr::filter(Probes %in% probes)
  title=paste0("Smoker vs Non-Smoker: ","probe ", probes[1], " gene ", gene)
  g1 <- ggplot(df,aes(x=Treatment,y=value,fill=Treatment)) +
    geom_boxplot(outlier.colour=NA,color="grey18") + #,fill="#1B9E77"
    stat_boxplot(geom ='errorbar', color="grey18") +
    geom_jitter(size=1,position = position_jitter(width=0.2)) +
    scale_fill_manual(values=colour_status)  + scale_y_continuous("Normalized Read Counts") + 
    theme_bw() +
    theme(legend.position="none",
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 15),
          title = element_text(size=15),
          axis.text=element_text(size=14)) + facet_wrap(. ~ Probes) 
  
  # if (nrow(tb) > 1){ g1 + facet_grid(. ~ Probes) + labs(title=paste0("Smoker vs Non-Smoker: gene ", gene),y="Normalized Read Count") }
  # else{ g1 + labs(title=title,y="Normalized Read Count") }
  g1
  
}

########################
## Helper functions ##
#######################

# The shortname_func function shortens the sample name shown in the plots. To start, define shortname_func <- function(x){x}
shortname_func <- function(x){gsub("^(.*).(cel|CEL).gz","\\1",x)} # remove .cel.gz or .CEL.gz from sample

# assign red to condition 1 and navy to condition 2
colour_status <- c("#0072B2","#D72422")
names(colour_status) <- c("non-smoker","smoker") 
colour_status_list <- unlist(lapply(pheno_QC$Treatment,function(x){colour_status[x]}))
colours = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666") #CB and Dark2


###############
## Heatmap ##
##############

corplot_func <- function(top_probes) {  # m: correlation matrix, colour_status_list: color assigned to each sample; colour_status: colour vector for the legend plot
  tb <- dcast(rma.data %>% dplyr::filter(Probes %in% top_probes) %>% dplyr::select(-Treatment),Probes ~ variable)
  m <- na.omit(as.matrix(tibble::column_to_rownames(tb,var="Probes")))
  array_name <- shortname_func(colnames(m)) # shorten the sample id
  # heatmap plot
  heatmap.2(m, col=viridis(256, option="B"),xlab="Samples",ylab="Probes",
            ColSideColors=colour_status_list, # use predefined colour_status_list, assign colors to status
            labCol=FALSE,labRow = FALSE, # take out gene probe id
            trace="none",
            margins=c(2,2), # (bottom margin, left margin)
            #cexRow=1,cexCol=1.4,
            keysize=1.8,key.title=NA,key.xlab="Gene Expression Values",key.ylab="Counts",
            main=" ")
  legend("topright",legend=names(colour_status),fill=colour_status,cex=1.0) # use predifined colour_status
}

##################################
## Principle Component Analysis ##
##################################

pca_plot <- function(group_var){
  tb <- dcast(rma.data,Probes ~ variable)
  raw.data.pca <- na.omit(as.matrix(tibble::column_to_rownames(tb,var="Probes")))
  sd <- apply(raw.data.pca,1,sd)
  raw.data.pca <- raw.data.pca[!sd==0,]
  # compute pcs
  pca <- prcomp(t(raw.data.pca), retx = TRUE, center = TRUE, scale = TRUE)
  pc <- data.frame(pca$x)
  
  #group
  group=pheno_QC[which(pheno_QC$Filename %in% row.names(pc)),group_var]
  df <- data.frame(
    PC1=pc$PC1,
    PC2=pc$PC2,
    group=group
  )
  
  #colors
  i=length(levels(group))
  group_col <- colours[1:i]
  names(group_col) <- levels(pheno_QC[[group_var]]) # colour to corresponding group for plot
  #Plot
  ggplot(df,aes(PC1,PC2,color=group)) + geom_point(size=5) +
    theme_bw() +
    scale_color_manual(group_var,values=group_col,na.value="grey") +
    theme(legend.text = element_text(size=15),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
}

###Sample Characters
#set colors

#set colors
clrs <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
             "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#8DD3C7","#BEBADA",  #CB and Dark2
             brewer.pal(11, "Spectral"),
             brewer.pal(11, "Set3"),
             brewer.pal(11, "Paired")) 


set_colors <- function(data){
  choices = c("Treatment","Sex","Ancestry","ScanDate")
  color_list = list()
  for (i in choices) { 
    color_list =  append(color_list,levels(data[[i]]))
  }
  col = clrs[1:length(color_list)]
  names(col) <- color_list
  return(col)
}

#Pheno_QC colour scheme
color_status <- set_colors(pheno_QC)

##BARPLOT##
barplot_func <- function(x,data){
  g1 <- ggplot(data, aes_string(x=x,fill=x)) + geom_bar(stat="count") + 
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(y) color_status[[y]]))) + theme_bw() + 
    theme(legend.position="none",
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text.y=element_text(size=13),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  return(g1) 
  
}

barplot_func_dodge <- function(x,a,data){
  g1 <- ggplot(data, aes_string(x=x,fill=a)) + geom_bar(stat="count",position=position_dodge(preserve = "single")) + 
    scale_fill_manual(values=unlist(lapply(levels(data[[a]]), function(y) color_status[[y]]))) + theme_bw() +
    theme(legend.position="none",
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13))
  return(g1) 
  
}

##BARPLOT PERCENTAGE
barplot_pc_func <- function(x,data){
  data$Var = x
  g1 <- ggplot(data, aes_string(x="Var", fill=x)) + geom_bar(position="fill") +
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(y) color_status[[y]]))) + scale_y_continuous(name="Percent") + 
  theme_bw() +
  theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          axis.title.x=element_blank(),
          title = element_text(size=15),
          axis.text.y=element_text(size=13),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  return(g1) 
}

barplot_pc_func_dodge <- function(x,a,data){
  g1 <- ggplot(data, aes_string(x=x,fill=a)) + geom_bar(position="fill") + 
    scale_fill_manual(values=unlist(lapply(levels(data[[a]]), function(y) color_status[[y]]))) + scale_y_continuous(name="Percent") + 
    theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13))
  return(g1) 
}

##HISTOGRAM##
hist_func <- function(Con,data,bins){
  df <- data[[Con]]
  #bins = round(sqrt(length(df)))
  ggplot(data=data, aes_string(x=Con)) + geom_histogram(col="#D55E00", fill="#56B4E9",alpha=0.6,bins = bins ,na.rm=TRUE) + labs(x=Con, y="Count") +  
    theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text=element_text(size=13)) ##0072B2 #xlim: 35,50
}

##BOXPLOT##
boxplot_func <- function(x,y,data){
  ggplot(data, aes_string(x=x,y=y,fill=x)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18",na.rm=TRUE) + 
    stat_boxplot(geom ='errorbar', color="grey18") + 
    labs(x=x, y=y) + geom_jitter(size=1,position = position_jitter(width=0.2)) + 
    scale_fill_manual(values=unlist(lapply(levels(data[[x]]), function(m) color_status[[m]]))) + theme_bw() +
    theme(legend.text = element_text(size=14),
          axis.title=element_text(size=15),
          title = element_text(size=15),
          axis.text.y=element_text(size=13),
          axis.text.x=element_blank(),axis.ticks.x=element_blank())}

