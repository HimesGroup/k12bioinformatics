library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(feather)
library(viridis)
library(gplots)
library(devtools)
source("functions.R")

#Load phenotype and results data
pheno <- read.table("./databases/GSE8823_Phenotype_withoutQC.txt", sep="\t", header=TRUE, as.is=TRUE)
de_results <- na.omit(read.csv("./databases/GSE8823_alveolar_macrophages_healthy_smoker_vs_non_smoker.csv",header=TRUE))
all_genes <- as.vector(unique(de_results$SYMBOL)[1:500]) #first 500 genes
image1 <- "./databases/Raw_probe_intensities_GSE8823.tiff"
image2 <- "./databases/Normalized_intensities_GSE8823.tiff"

shinyServer(function(input, output,session) {
  genes <- reactive({selectizeInput("gene", "Official Gene Symbol:", all_genes, selected="SMAD7", width="185px", options = list(create = TRUE))})
  output$genesAvail <- renderUI({genes()})
  curr_gene <- reactive({gsub(" ", "", toString(input$gene), fixed = TRUE)})

  output$phenoTable <- renderDataTable({
    pheno %>%
      dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry)
  }, options = list(pageLength=10, searching=FALSE)
  )
  
  #RMA image
  rma_image <- reactive({
    if(input$rma == 0){fi = image1} # don't do anything if action button has been clicked 0 times
    else if (input$rma%%2 == 0) { # %% means "modulus" - i.e. here you're testing if button has been clicked a multiple of 2 times
      fi = image1
      updateActionButton(session, "rma", label="Normalize with RMA") # change action button label based on user input
    } else { # else is 1, 3, 5 etc.
      fi = image2
      updateActionButton(session, "rma", label="Go Back")
    }
    fi
  })
   
  output$RMAimage <- renderImage({ 
    if(input$rma == 0){fi = image1} # don't do anything if action button has been clicked 0 times
    else if (input$rma%%2 == 0) { # %% means "modulus" - i.e. here you're testing if button has been clicked a multiple of 2 times
      fi = image1
      updateActionButton(session, "rma", label="Normalize with RMA") # change action button label based on user input
    } else { # else is 1, 3, 5 etc.
      fi = image2
      updateActionButton(session, "rma", label="Go Back")
    }
    return(list(
      src = fi,
      height=400,
      width = 600,
      filetype = "image/tiff",
      alt = "Normalize data with RMA"))}, deleteFile = FALSE)
  
  #Boxplots for outliers
  output$QCimage <- renderImage({
    return(list(
      src = "./databases/RPI_boxplots_GSE8823_QC.tiff",
      height= 800,
      width = 550,
      filetype = "image/tiff",
      alt = "Raw Probe Intensities"))}, deleteFile = FALSE)
  
  #PCA
  output$PCAplot <- renderPlot({pca_plot(input$var)},height=700, width=600)
  
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
  
  #Genes selected
  gene_de <- reactive({de_results %>% dplyr::filter(SYMBOL == curr_gene())})
  
  #Width for plot
  get_width <- reactive({
    800 + 50*(nrow(gene_de()))})
  
  #Barplot output
  output$barPlot <- renderPlot({
    validate(need(curr_gene() != "", "Please enter a gene id")) # no gene symbol was input
    #Selected gene
    topgene_boxplot_func(gene_de())
  },height=400, width=get_width) #increase width depending on facets

  #Heatmap
  output$heatMap <- renderPlot({
    top_probes <- as.vector(unique(de_results$ID[1:input$probes]))
    corplot_func(top_probes)
  },height=1100, width=900)
})




