library(shiny)
library(shinythemes)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  
  # Application title
  titlePanel(h1("Exploratory Data Analysis Tool", align="center")),
  hr(),
  #mainPanel(
  tabsetPanel(
    tabPanel("Introduction",
             br(),
             p("This app helps elaborate on basic concepts of data analysis and visualization. 
               You can upload a csv file with data of your choice and carry out basic data analysis. 
               In absence of a user provided file, an example csv file of 'iris' R dataset is available to upload and explore the analysis features."),
             # Input: Select a file ----
             column(12, downloadButton(outputId="iris_data_download",label="Download iris dataset file"), align="left"), 
             br(),br(),hr(),
             column(12,
             fluidRow(fileInput("file1", "Upload CSV File:",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
             br(),
             dataTableOutput("contents")),
             br(),hr()),
    tabPanel("Univariate Analysis",
            br(),
            h3(p("Overview of data")),
            h4(p("The continuous variables in the uploaded dataset are as follows:")),
            dataTableOutput("varcData"),
            br(),
            h4(p("The categorical variables in the uploaded dataset are as follows:")),
            dataTableOutput("vardData"),
            br(), hr(),
            h4(p("Categorical Variables")),
            p("The distribution for the categorical variables with number of unique entries between 2-15 are displayed here."),
            uiOutput('disc'),
            plotOutput("barplotUP",height="400px",width="auto"),
            br(), hr(),
            h4(p("Continuous Variables")),
            p("The distribution for the continuous variables with number of unique entries between 2-15 are displayed here."),
            uiOutput('cont'),
            br(),
            p("Histogram showing distribution of continuous variables"),
            plotOutput("histPlotUP",height="400px",width="auto"),
            br()),
    
    tabPanel("Bivariate Analysis", br(),
            p("Relationship between the categorical and continuous variables in the uploaded data"),
            uiOutput('bdisc'),
            uiOutput('bcont'),
            #plotOutput("fbarplotUP",height="400px",width="auto"),
            p("Boxplot showing summary of variables"),
            plotOutput("boxPlotUP",height="400px",width="auto")))
  
))
