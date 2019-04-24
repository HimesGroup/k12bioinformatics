library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
#library(rCharts) #install with devtools (install_github("rCharts", "ramnathv"))

pheno_QC <- read.table("../gene_expression/databases/GSE8823_Phenotype_withQC.txt", sep="\t", header=TRUE)
pheno_QC <- pheno_QC %>% dplyr::filter(QC_Pass!=0)
pheno_QC$Treatment <- as.factor(gsub("_","-",pheno_QC$Treatment))
pheno_QC$Donor <- as.factor(paste0("D",pheno_QC$Donor))
#pheno_QC <- pheno_QC %>% mutate_all(as.factor)
colours = c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666") #CB and Dark2

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  #output$barPlot <- renderChart({rPlot(Age ~ Ancestry | Treatment, data = pheno_QC, color = 'Treatment', type = 'bar')})

  output$phenoData <- renderDataTable({pheno_QC %>% dplyr::select(GEO_ID, Smoking_status, Sex, Age, Ancestry,Donor)},options = list(pageLength=10, searching=FALSE))
  
  ##BARPLOT##
  #function
  barplot_func <- function(x,data){
    g1 <- ggplot(data, aes_string(x=x)) + geom_bar(stat="count",aes_string(fill=x)) + scale_fill_manual(values=sample(colours, length(levels(data[[x]])))) + theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14))
    return(g1) 
    
  }
  
  barplot_both_func <- function(x,y,data){
    g1 <- ggplot(data, aes_string(x=x,y=y)) + geom_bar(stat="identity",aes_string(fill=x)) + scale_fill_manual(values=sample(colours, length(levels(data[[x]])))) + theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14))
    return(g1) 
    
  }
  
  #output
  output$barPlot <- renderPlot({barplot_func(input$feat,pheno_QC)})
  
  output$fbarPlot <- renderPlot({barplot_func(input$var,pheno_QC) + facet_grid(.~Treatment) + theme(strip.text = element_text(size=15))})
  
  ##HISTOGRAM##
  #function
  hist_func <- function(Con,data){
    df <- data[[Con]]
    ggplot(data=data, aes_string(Con)) + geom_histogram(breaks=seq(min(df), max(df)),col="#D55E00", fill="#56B4E9",alpha=0.6) + labs(x=Con, y="Count") + xlim(c(min(df),max(df))) + 
    theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14)) ##0072B2 #xlim: 35,50
  }
  #output
  output$histPlot <- renderPlot({hist_func("Age",pheno_QC)})
  
  ##BOXPLOT##
  #function
  boxplot_func <- function(var,age,data){ggplot(data, aes_string(x=var,y=age,fill=var)) + geom_boxplot(outlier.colour=NA, lwd=0.2, color="grey18") + 
      stat_boxplot(geom ='errorbar', color="grey18") + 
      labs(x=var, y=age) + geom_jitter() + theme_bw() +
      theme(legend.text = element_text(size=14),
            axis.title=element_text(size=15),
            title = element_text(size=15),
            axis.text=element_text(size=14))}
  #output
  output$boxPlot <- renderPlot({boxplot_func(input$comp,"Age",pheno_QC)})
  
  
  ##Uploaded dataset
  #Get discrete variables from uploaded dataset
  contents <- reactive({if(!is.null(input$file1)){read.csv(input$file1$datapath,header = TRUE,sep = ",")}})
  output$contents <- renderDataTable({contents()}, options = list(pageLength=10, searching=FALSE))
  
  disc_var <- reactive({
    if(!is.null(contents())){
      disc_var_lst <- list()
      all_vars <- names(contents())[sapply(contents(), class) == "factor"]
      for (i in seq(1,length(all_vars))){
        len = length(levels(contents()[[all_vars[i]]]))
        if (len >=2 && len <=10){
          disc_var_lst[i] = all_vars[i]}
      }
      unlist(disc_var_lst)
    } else { NULL }
})
    
  cont_var <- reactive({
    if(!is.null(contents())){names(contents())[sapply(contents(), class) != "factor"]}else{NULL}})
  
  output$varcData <- renderDataTable({if(!is.null(contents())){data.frame("Continuous variables" = cont_var())}},options = list(pageLength=10, searching=FALSE))
  output$vardData <- renderDataTable({if(!is.null(contents())){data.frame("Discrete variables"= disc_var())}},options = list(pageLength=10, searching=FALSE))
  
  output$disc = renderUI({if(!is.null(contents())){selectInput('disc', 'Select categorical variable:', choices = disc_var(),multiple=FALSE,width="220px")}else{NULL}})
  output$cont = renderUI({if(!is.null(contents())){selectInput('cont', 'Select continuous variable:', choices = cont_var(),multiple=FALSE,width="220px")}else{NULL}})
  output$barplotUP <- renderPlot({if(!is.null(contents())){barplot_func(input$disc,contents())}else{NULL}})
  output$fbarplotUP <- renderPlot({if(!is.null(contents())){barplot_both_func(input$disc,input$cont,contents())}else{NULL}})
  output$histPlotUP <- renderPlot({if(!is.null(contents())){hist_func(input$cont,contents())}else{NULL}})
  output$boxPlotUP <- renderPlot({if(!is.null(contents())){boxplot_func(input$disc,input$cont,contents())}else{NULL}})
  
})

