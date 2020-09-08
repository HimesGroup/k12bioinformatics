library(shiny)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
source("data_vis_global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  #output
  #IRIS data download
  output$iris_data_download <- downloadHandler(
    filename= function(){paste0("iris_dataset.csv")},
    content=function(file){
      write.csv(iris_data, file, row.names = FALSE, quote = FALSE)})
  
  ##Read in uploaded dataset
  contents <- reactive({if(!is.null(input$file1)){
    df <- read.csv(input$file1$datapath, header = TRUE, sep = ",", na.strings=c("","NA"))
    i <- sapply(df, is.character)
    df[i] <- lapply(df[i], as.factor)
    na.omit(df)}
    })
  output$contents <- renderDataTable({contents()}, options = list(pageLength=10, searching=FALSE))
  
  #Get discrete variables from uploaded dataset
  disc_var <- reactive({
    if(!is.null(contents())){
      get_discrete_var(contents())
    } else { NULL }
    })
    
  #Get continuous variables from uploaded dataset
  cont_var <- reactive({
    if(!is.null(contents())){
      get_cont_var(contents())}else{NULL}})
  
  #List the continuous and discrete variables in tables
  output$varcData <- renderDataTable({if(!is.null(contents())){data.frame("Continuous variables" = cont_var())}},options = list(pageLength=10, searching=FALSE))
  output$vardData <- renderDataTable({if(!is.null(contents())){data.frame("Discrete variables"= disc_var())}},options = list(pageLength=10, searching=FALSE))
  
  #Discrete variables for user selection
  #Univariate tab
  output$disc = renderUI({if(!is.null(contents())){selectInput('disc', 'Select categorical variable:', choices = disc_var(),width="220px")}else{NULL}})
  #Bivariate tab
  output$bdisc = renderUI({if(!is.null(contents())){selectInput('bdisc', 'Select categorical variable:', choices = disc_var(),width="220px")}else{NULL}})
  
  #Continuous variable for user selection
  #Univariate tab
  output$cont = renderUI({if(!is.null(contents())){selectInput('cont', 'Select continuous variable:', choices = cont_var(), width="280px")}else{NULL}})
  #bivariate tab
  output$bcont = renderUI({if(!is.null(contents())){selectInput('bcont', 'Select continuous variable:', choices = cont_var(), width="280px")}else{NULL}})
  
  ########################
  ## UNIVARIATE PLOTS  ##
  ########################
  
  #Get width (levels) for plots of selected discrete variable
  get_width <- reactive({
    validate(need(!is.null(input$disc), "No data file found. Please upload csv file to perform analysis."))
    len = length(unique(contents()[[input$disc]]))
    440 + 110*abs(len-5)
  })
  
  ##Barplot for discrete variables
  output$barplotUP <- renderPlot({barplot_func(input$disc, contents())}, width = get_width)
  
  #Histogram for continuous variables
  output$histPlotUP <- renderPlot({validate(need(!is.null(input$cont), "No data file found. Please upload csv file to perform analysis."))
                                  hist_func(input$cont, contents(), input$bins)}, width = 600)
  
  ########################
  ## BIVARIATE PLOTS  ##
  ########################
  
  #Get width (levels) for plots of selected discrete variable
  get_width_bi <- reactive({
    validate(need(!is.null(input$bdisc), "No data file found. Please upload csv file to perform analysis."))
    len = length(unique(contents()[[input$bdisc]]))
    440 + 110*abs(len-5)
    })
  
  ##Barplot with average values 
  output$fbarplotUP <- renderPlot({barplot_both_func(input$bdisc, input$bcont, contents())},width=get_width_bi)
  
  ##Mean output for all variables in the dataset
  output$cont_mean <- renderText({validate(need(!is.null(input$cont), "No continuous variable found."))
    paste("The mean of all measures is:", get_mean(input$bcont, contents()))})
  
  ##Boxplot
  output$boxPlotUP <- renderPlot({boxplot_func(input$bdisc,input$bcont,contents())},width = get_width_bi)
  
  #DATE scatterplot
  #if Date exists, plot average of values across available timepoints
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
  
  
})

