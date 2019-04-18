
rma.data <- read_feather("./databases/GSE8823_pheno+rma_counts.feather")
pheno_QC <- read.table("./databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE, as.is=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0)

# The datreform_func function reformats the DE table
datreform_func <- function(dt,topnum=200) {
  dt=dt[order(dt$P.Value),]
  dt=dt[1:topnum,]
  round2 <- function(x){round(x,2)}
  dt[,c("logFC","AveExpr","t","B")] <- sapply(dt[,c("logFC","AveExpr","t","B")],round2)
  sciform <- function(x){format(x,scientific=TRUE,digits =2)}
  dt[,c("P.Value","adj.P.Val","pValuesBatch","qValuesBatch")] <- sapply(dt[,c("P.Value","adj.P.Val","pValuesBatch","qValuesBatch")],sciform)
  dt
}

# function for top gene boxplot
topgene_boxplot_func <- function(tb) { # comp: comparison status
  #row.names(rma.data.of.interest) <- gsub("_PM","",row.names(rma.data.of.interest))
  probes <- as.vector(tb$ID)
  gene <- unique(tb$SYMBOL)
  df <- rma.data %>% dplyr::filter(Probes %in% probes)
  title=paste0("Smoker vs Non-Smoker: ","probe ", probes[1], " gene ", gene)
  g1 <- ggplot(df,aes(x=Treatment,y=value)) +
    geom_boxplot(outlier.colour=NA,color="grey18",fill="#1B9E77") +
    stat_boxplot(geom ='errorbar', color="grey18") +
    geom_jitter(size=1,position = position_jitter(width=0.3)) +
    theme_bw() +
    theme(legend.position="none",
          axis.title=element_blank(),
          strip.text.x = element_text(size = 15),
          title = element_text(size=15),
          axis.text=element_text(size=14))
  
  if (nrow(tb) > 1){ g1 + facet_grid(. ~ Probes) + labs(title=paste0("Smoker vs Non-Smoker: gene ", gene),y="Normalized Read Count") }
  else{ g1 + labs(title=title,y="Normalized Read Count") }
  
}

# The shortname_func function shortens the sample name shown in the plots. To start, define shortname_func <- function(x){x}
shortname_func <- function(x){gsub("^(.*).(cel|CEL).gz","\\1",x)} # remove .cel.gz or .CEL.gz from sample

# assign red to condition 1 and navy to condition 2
colour_status <- c("navy","red")
names(colour_status) <- c("non_smoker","smoker") 
colour_status_list <- unlist(lapply(pheno_QC$Treatment,function(x){colour_status[x]}))

#Heatmap function
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
            cexRow=1,cexCol=1,
            keysize=1.5,key.title=NA,key.xlab="Gene Expression Values",key.ylab="Counts",
            main="Gene expression heatmap for Smoker vs. Non-Smoker")
  legend("bottomleft",legend=names(colour_status),fill=colour_status,cex=0.8) # use predifined colour_status
}


