library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(feather)
library(viridis)
library(RColorBrewer)
library(gplots)
library(devtools)
library(cowplot)
source("functions.R")

#Load phenotype and results data
pheno <- read.table("databases/GSE8823_Phenotype_withoutQC.txt", sep="\t", header=TRUE, as.is=TRUE)
pheno <- pheno %>% dplyr::rename("ScanDate" = "ScanDate_Group")
pheno$Smoking_status <- as.factor(gsub(", ","-",pheno$Smoking_status))
#pheno$Treatment <- as.factor(gsub("_","-",pheno$Treatment))
de_results <- na.omit(read.csv("databases/GSE8823_alveolar_macrophages_healthy_smoker_vs_non_smoker.csv",header=TRUE))
all_genes <- as.vector(unique(de_results$SYMBOL)[1:500]) #first 500 genes
image1 <- "databases/Raw_Probe_Intensities.tiff"
image2 <- "databases/Normalized_Probe_Intensities.tiff"

shinyServer(function(input, output,session) {
  genes <- reactive({selectizeInput("gene", "Official Gene Symbol:", all_genes, selected="SMAD7", width="185px", options = list(create = TRUE))})
  output$genesAvail <- renderUI({genes()})
  curr_gene <- reactive({gsub(" ", "", toString(input$gene), fixed = TRUE)})
  
  phenotable <- reactive({pheno %>%
      dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry)})

  output$phenoTable <- renderDataTable({
    phenotable()
  }, options = list(pageLength=10, searching=FALSE)
  )
  
  #Download phenotype information
  output$pheno_data_download <- downloadHandler(
    filename= function(){paste0("subject_characteristics.csv")},
    content=function(file){
      write.csv(phenotable(), file,row.names = FALSE, quote = FALSE)})
  
  #Sample characteristics
  #output$phenoData <- renderDataTable({pheno_QC %>% dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry)},options = list(pageLength=10, searching=FALSE))
  
  output$barPlot <- renderPlot({
    plot_grid(barplot_func(input$feat,pheno_QC),NULL,barplot_pc_func(input$feat,pheno_QC),ncol=3,rel_widths = c(1.5,0.1,1),align = "h")
    })

  output$fbarPlot <- renderPlot({
    barplot_func_dodge(input$svar,'Treatment',pheno_QC)
    plot_grid(barplot_func_dodge(input$svar,'Treatment',pheno_QC),NULL,barplot_pc_func_dodge(input$svar,'Treatment',pheno_QC),ncol=3,rel_widths = c(1.5,0.1,1.5),align = "h")
    #barplot_func(input$var,pheno_QC) + facet_grid(.~Treatment) + theme(strip.text = element_text(size=15))
  })

  #output
  output$histPlot <- renderPlot({hist_func("Age",pheno_QC,input$bins)})

  #output
  output$sboxPlot <- renderPlot({boxplot_func(input$comp,"Age",pheno_QC)})
  
  #Affy Image
  output$Affyimage <- renderImage({fi="databases/affymetrix.tiff"
                                  return(list(src = fi,height= 380,width = 550,filetype = "image/tiff",
                                              alt = "Normalize data with RMA"))}, deleteFile = FALSE)
  
  #Outlier plot Image
  output$QCimage <- renderImage({fi="databases/Outlier_barplot.tiff"
  return(list(src = fi,height= 800,width = 550,filetype = "image/tiff",
              alt = "QC Outliers"))}, deleteFile = FALSE)
  
  #Density Curves Image
  output$DCimage <- renderImage({fi="databases/Density_curves.tiff"
  return(list(src = fi,height= 500,width = 610,filetype = "image/tiff",
              alt = "Density Curve Outliers"))}, deleteFile = FALSE)
  
  #Volcano Plot Image
  output$volcanoPlot <- renderImage({fi="databases/Volcano_plot_edited.tiff"
  return(list(src = fi,height= 330,width = 390,filetype = "image/tiff",
              alt = "Volcano Plot"))}, deleteFile = FALSE)                                
  
  #RMA image
  output$RMAimage <- renderImage({ 
    if(input$rma == 0){fi = image1} # don't do anything if action button has been clicked 0 times
    else if (input$rma%%2 == 0) { # %% means "modulus" - i.e. here you're testing if button has been clicked a multiple of 2 times
      fi = image1
      updateActionButton(session, "rma", label="Normalize with RMA") # change action button label based on user input
    } else { # else is 1, 3, 5 etc.
      fi = image2
      updateActionButton(session, "rma", label="See Raw Data")
    }
    return(list(
      src = fi,
      height= 300,
      width = 550,
      filetype = "image/tiff",
      alt = "Normalize data with RMA"))}, deleteFile = FALSE)

  
  #PCA
  output$PCAplot <- renderPlot({pca_plot(input$var)},height=400, width=600)
  
  #DE results table
  output$DEtable <- renderDataTable({
    de_df <- datreform_func(de_results) #%>% dplyr::arrange(adj.P.Val) %>% top_n(50)
    de_df %>% dplyr::rename(`Gene Symbol`= SYMBOL)
  }, options = list(pageLength=10, searching=FALSE))
  
  
  #Genes selected
  gene_de <- reactive({de_results %>% dplyr::filter(SYMBOL == curr_gene())})
  
  #Width for plot
  get_width <- reactive({
    400 + 50*(nrow(gene_de()))}) #800
  
  #Get height for plot
  get_height <- reactive({
    200 + 90*(nrow(gene_de()))}) #800
  
  #Barplot output
  output$boxPlot <- renderPlot({
    validate(need(curr_gene() != "", "Please enter a gene id")) # no gene symbol was input
    #Selected gene
    topgene_boxplot_func(gene_de())
  },height=get_height, width=get_width) #increase width depending on facets

  #Heatmap
  output$heatMap <- renderPlot({
    top_probes <- as.vector(unique(de_results$ID[1:input$probes]))
    corplot_func(top_probes)
  },height=750, width=950)
})





