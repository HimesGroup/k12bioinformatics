library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
#library(rCharts) #install with devtools (install_github("rCharts", "ramnathv"))

pheno_QC <- read.table("../gene_expression/databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0)
pheno_QC$Donor <- as.factor(paste0("D",pheno_QC$Donor))
#pheno_QC <- pheno_QC %>% mutate_all(as.factor)
colours = c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666") #CB and Dark2



# Define server logic required to draw a histogram
shinyServer(function(input, output){
  #output$barPlot <- renderChart({rPlot(Age ~ Ancestry | Treatment, data = pheno_QC, color = 'Treatment', type = 'bar')})
  
  output$contents <- renderDataTable({
    if(!is.null(input$file1)){
      read.csv(input$file1$datapath,header = TRUE,sep = ",")
    }}, options = list(pageLength=10, searching=FALSE))
  
  output$phenoData <- renderDataTable({pheno_QC %>% dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry,Donor)},options = list(pageLength=10, searching=FALSE))
     
  
  #BARPLOT
  output$barPlot <- renderPlot({
    g1 <- ggplot(pheno_QC, aes_string(x=input$feat,fill=input$feat)) + geom_bar() + scale_fill_manual(values=sample(colours, length(levels(pheno_QC[[input$feat]])))) + theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14))
    g1 
  })
  
  #HISTOGRAM
  output$histPlot <- renderPlot({
    ggplot(data=pheno_QC, aes(Age)) + geom_histogram(breaks=seq(20, 50, by = 2),col="#D55E00", fill="#56B4E9",alpha=0.6) + labs(x="Age", y="Count") + xlim(c(35,50))+ theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14)) ##0072B2
    
  })
  
  #BOXPLOT
  output$boxPlot <- renderPlot({
    ggplot(data=pheno_QC, aes(x=Treatment,y=Age,fill=Treatment)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18") + 
      stat_boxplot(geom ='errorbar', color="grey18") + 
      labs(x="Treatment", y="Age") + geom_jitter() + theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14))
  })
  
})

