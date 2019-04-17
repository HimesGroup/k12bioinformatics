
rma.data <- read_feather("./databases/GSE8823_pheno+rma_counts.feather")

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