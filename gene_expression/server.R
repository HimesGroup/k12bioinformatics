library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(feather)
library(viridis)
library(gplots)
source("functions.R")

#Load phenotype and results data
pheno <- read.table("./databases/GSE8823_Phenotype_withoutQC.txt", sep="\t", header=TRUE, as.is=TRUE)
de_results <- na.omit(read.csv("./databases/GSE8823_alveolar_macrophages_healthy_smoker_vs_non_smoker.csv",header=TRUE,as.is=TRUE))
all_genes <- unique(de_results$SYMBOL)

shinyServer(function(input, output) {
  genes <- reactive({selectizeInput("gene", "Official Gene Symbol:", all_genes, selected="SMAD7", width="185px", options = list(create = TRUE))})
  output$genesAvail <- renderUI({genes()})
  curr_gene <- reactive({gsub(" ", "", toupper(toString(input$gene)), fixed = TRUE)})

  output$phenoTable <- renderDataTable({
    pheno %>%
      dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry)
  }, options = list(pageLength=10, searching=FALSE)
  )
   
  #DE results table
  output$DEtable <- renderDataTable({
    datreform_func(de_results) #%>% dplyr::arrange(adj.P.Val) %>% top_n(50)
  }, options = list(pageLength=10, searching=FALSE))
  
  #Volcano plot image
  output$volcanoPlot <- renderImage({ 
    return(list(
      src = "./databases/Volcano_plot.tiff",
      height=400,
      width = 800,
      filetype = "image/tiff",
      alt = "Volcano Plot"))}, deleteFile = FALSE)
  
  #Barplot output
  output$barPlot <- renderPlot({
    validate(need(curr_gene() != "", "Please enter a gene id")) # no gene symbol was input
    #Selected gene
    gene_de <- de_results %>% dplyr::filter(SYMBOL == curr_gene())
    topgene_boxplot_func(gene_de)
  },height=400, width=800) #increase width depending on facets

  #Heatmap
  output$heatMap <- renderPlot({
    top_probes <- as.vector(unique(de_results$ID[1:input$probes]))
    corplot_func(top_probes)
  },height=1100, width=900)
})




