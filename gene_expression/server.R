library(shiny)
library(ggplot2)
library(dplyr)

#Load phenotype and results data
pheno <- read.table("./databases/GSE8823_Phenotype_withoutQC.txt", sep="\t", header=TRUE, as.is=TRUE)

shinyServer(function(input, output) {

  output$phenoTable <- renderDataTable({
    pheno %>%
      dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry)
  }, options = list(pageLength=10, searching=FALSE)
  )
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
