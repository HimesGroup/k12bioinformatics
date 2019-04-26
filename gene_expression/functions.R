

#########################
## Read in data files ##
########################
rma.data <- read_feather("./databases/GSE8823_pheno+rma_counts.feather")
rma.data$Treatment <- gsub("_","-",rma.data$Treatment)
pheno_QC <- read.table("./databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0)
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
    geom_jitter(size=1,position = position_jitter(width=0.3)) +
    scale_fill_manual(values=colour_status) +
    theme_bw() +
    theme(legend.position="none",
          axis.title=element_blank(),
          strip.text.x = element_text(size = 15),
          title = element_text(size=15),
          axis.text=element_text(size=14)) + facet_grid(. ~ Probes)
  
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
  heatmap.2(m, col=viridis(256, option="B"),
            ColSideColors=colour_status_list, # use predefined colour_status_list, assign colors to status
            labCol=array_name,labRow = "", # take out gene probe id
            trace="none",
            margins=c(12,20), # (bottom margin, left margin)
            cexRow=1,cexCol=1.4,
            keysize=1.5,key.title=NA,key.xlab="Gene Expression Values",key.ylab="Counts",
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

