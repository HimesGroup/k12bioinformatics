library(shiny)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
#library(rCharts) #install with devtools (install_github("rCharts", "ramnathv"))
source("data_vis_global.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output){
  output$phenoData <- renderDataTable({pheno_QC %>% dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry)},options = list(pageLength=10, searching=FALSE))

  #output
  output$barPlot <- renderPlot({barplot_func(input$feat,pheno_QC)})
  
  output$fbarPlot <- renderPlot({
    barplot_func_dodge(input$var,'Treatment',pheno_QC)
    #barplot_func(input$var,pheno_QC) + facet_grid(.~Treatment) + theme(strip.text = element_text(size=15))
    })

  #output
  output$histPlot <- renderPlot({hist_func("Age",pheno_QC)})

  #output
  output$boxPlot <- renderPlot({boxplot_func(input$comp,"Age",pheno_QC)})

  ##Uploaded dataset
  #Get discrete variables from uploaded dataset
  contents <- reactive({if(!is.null(input$file1)){
    df <- read.csv(input$file1$datapath,header = TRUE,sep = ",",na.strings=c("","NA"))
    na.omit(df)}
    })
  output$contents <- renderDataTable({contents()}, options = list(pageLength=10, searching=FALSE))
  
  disc_var <- reactive({
    if(!is.null(contents())){
      get_discrete_var(contents())
    } else { NULL }
})
    
  cont_var <- reactive({
    if(!is.null(contents())){names(contents())[sapply(contents(), class) != "factor"]}else{NULL}})
  
  output$varcData <- renderDataTable({if(!is.null(contents())){data.frame("Continuous variables" = cont_var())}},options = list(pageLength=10, searching=FALSE))
  output$vardData <- renderDataTable({if(!is.null(contents())){data.frame("Discrete variables"= disc_var())}},options = list(pageLength=10, searching=FALSE))
  
  #Discrete variable 
  output$disc = renderUI({if(!is.null(contents())){selectInput('disc', 'Select categorical variable:', choices = disc_var(),width="220px")}else{NULL}})
  #bivariate tab
  output$bdisc = renderUI({if(!is.null(contents())){selectInput('bdisc', 'Select categorical variable:', choices = disc_var(),width="220px")}else{NULL}})
  
  #Continuous variable
  output$cont = renderUI({if(!is.null(contents())){selectInput('cont', 'Select continuous variable:', choices = cont_var(),width="220px")}else{NULL}})
  #bivariate tab
  output$bcont = renderUI({if(!is.null(contents())){selectInput('bcont', 'Select continuous variable:', choices = cont_var(),width="220px")}else{NULL}})
  
  #Plots - Univariate
  observe({ print(input$disc)})
  
  get_width <- reactive({
    if(!is.null(contents())){
      len = length(levels(contents()[[input$disc]]))
      wid = 110*len
      if (wid < 650){wid = 650}
      wid}})
  
  output$barplotUP <- renderPlot({if(!is.null(input$disc)){barplot_func(input$disc,contents())}else{NULL}},width = get_width)
  output$histPlotUP <- renderPlot({if(!is.null(input$cont)){hist_func(input$cont,contents())}else{NULL}}, width = 600)
  
 
  #Plots - Bivariate
  get_width_bi <- reactive({
    if(!(is.null(contents()))){
      len = length(levels(contents()[[input$bdisc]]))
      wid = 110*len
      if (wid < 650){wid = 650}
      wid}})
  
  output$fbarplotUP <- renderPlot({if(!is.null(contents())){barplot_both_func(input$bdisc,input$bcont,contents())}else{NULL}},width=get_width_bi)
  output$boxPlotUP <- renderPlot({if(!is.null(contents())){boxplot_func(input$bdisc,input$bcont,contents())}else{NULL}},width = get_width_bi)
  
  #IRIS data download
  output$iris_data_download <- downloadHandler(
    filename= function(){paste0("iris_dataset.csv")},
    content=function(file){
      write.csv(iris_data, file, row.names = FALSE, quote = FALSE)})

})

