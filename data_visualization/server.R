library(shiny)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
#library(rCharts) #install with devtools (install_github("rCharts", "ramnathv"))
source("data_vis_global.R")

#########################
## Read in data files ##
########################
pheno_QC <- read.table("../databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0) %>% rename(ScanDate=ScanDate_Group)
pheno_QC$ScanDate <- as.factor(gsub("/([^/]*)$", " ", pheno_QC$ScanDate))
pheno_QC$Treatment <- as.factor(gsub("_","-",pheno_QC$Treatment))
pheno_QC$Donor <- as.factor(paste0("D",pheno_QC$Donor))
iris_data <- read.csv("../databases/iris_dataset.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  #output
  ##Uploaded dataset
  #Get discrete variables from uploaded dataset
  contents <- reactive({if(!is.null(input$file1)){
    df <- read.csv(input$file1$datapath, header = TRUE, sep = ",", na.strings=c("","NA"))
    na.omit(df)}
    })
  output$contents <- renderDataTable({contents()}, options = list(pageLength=10, searching=FALSE))
  
  disc_var <- reactive({
    if(!is.null(contents())){
      get_discrete_var(contents())
    } else { NULL }
    })
    
  cont_var <- reactive({
    if(!is.null(contents())){
      get_cont_var(contents())}else{NULL}})
  
  
  output$varcData <- renderDataTable({if(!is.null(contents())){data.frame("Continuous variables" = cont_var())}},options = list(pageLength=10, searching=FALSE))
  output$vardData <- renderDataTable({if(!is.null(contents())){data.frame("Discrete variables"= disc_var())}},options = list(pageLength=10, searching=FALSE))
  
  #Discrete variable 
  output$disc = renderUI({if(!is.null(contents())){selectInput('disc', 'Select categorical variable:', choices = disc_var(),width="220px")}else{NULL}})
  #bivariate tab
  output$bdisc = renderUI({if(!is.null(contents())){selectInput('bdisc', 'Select categorical variable:', choices = disc_var(),width="220px")}else{NULL}})
  
  #Continuous variable
  output$cont = renderUI({if(!is.null(contents())){selectInput('cont', 'Select continuous variable:', choices = cont_var(), width="280px")}else{NULL}})
  #bivariate tab
  output$bcont = renderUI({if(!is.null(contents())){selectInput('bcont', 'Select continuous variable:', choices = cont_var(), width="280px")}else{NULL}})
  
  #Plots - univariate
  get_width <- reactive({
    validate(need(!is.null(input$disc), "No data file found. Please upload csv file to perform analysis."))
    len = length(unique(contents()[[input$disc]]))
    440 + 110*abs(len-5)
  })
  
  output$barplotUP <- renderPlot({barplot_func(input$disc, contents())}, width = get_width)
  output$histPlotUP <- renderPlot({validate(need(!is.null(input$cont), "No data file found. Please upload csv file to perform analysis."))
                                  hist_func(input$cont, contents(), input$bins)}, width = 600)

  output$cont_mean <- renderText({validate(need(!is.null(input$cont), "No continuous variable found."))
                                  get_mean(input$cont, contents())})

  
  #Plots - Bivariate
  get_width_bi <- reactive({
    validate(need(!is.null(input$bdisc), "No data file found. Please upload csv file to perform analysis."))
    len = length(unique(contents()[[input$bdisc]]))
    440 + 110*abs(len-5)
    })
  
  output$fbarplotUP <- renderPlot({barplot_both_func(input$bdisc,input$bcont,contents())},width=get_width_bi)
  output$boxPlotUP <- renderPlot({boxplot_func(input$bdisc,input$bcont,contents())},width = get_width_bi)
  
  #DATE scatterplot
  observe({
  date <- names(contents())[grep(paste("DATE",collapse="|"),names(contents()),ignore.case = TRUE)]
  if(length(date)!=0){
    len = length(unique(contents()[[date]]))
    wid = 110*len
    output$sptitle <- renderText("Scatterplot of average summary across dates/timepoints (if available)")
    output$scatterplotDT <- renderPlot({
      validate(need(!is.null(input$bcont), "No data file found. Please upload csv file to perform analysis."))
      scatplot_func_dt(contents(),input$bcont)},width=wid)
  } else {NULL}})
  
  #IRIS data download
  output$iris_data_download <- downloadHandler(
    filename= function(){paste0("iris_dataset.csv")},
    content=function(file){
      write.csv(iris_data, file, row.names = FALSE, quote = FALSE)})

})

