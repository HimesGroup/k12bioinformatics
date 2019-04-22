library(shiny)
library(shinythemes)
library(shinyWidgets)
#library(rCharts)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #theme = shinythemes::shinytheme("lumen"),
  
  # Application title
  titlePanel(h1("Exploratory Data Analysis Tool", align="center")),
  hr(),
  #mainPanel(
  tabsetPanel(
    tabPanel("Introduction",
             br(),
             p("This app helps elaborate on basic concepts of data analysis and visualization. You can upload a csv file with data of your choice and carry out basic data analysis."),
             # Input: Select a file ----
             column(12,
             fluidRow(fileInput("file1", "Upload CSV File:",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
             # Input: Checkbox if file has header ----
             #fluidRow(checkboxInput(input="header", label="Header"))),
             # Input: Select separator ----
             #radioButtons(input="sep", label="Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ",")),
             br(),
             dataTableOutput("contents")),
             br(),hr(),
             h4(p("Example Phenotype Dataset")),
             p("Here, as an example, we have the phenotype information for the Gene Expression Omnibus (GEO) dataset : ", 
               a(href="http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE8823", "GSE8823.", target="_blank"),"This dataset was for an experiment in which
               investigators compared gene expression profiles of bronchoalveolar lavage fluid obtained from 11 non-smokers
               versus 13 smokers (mean smoking use was 36 pack-years). The scientific paper published to report the results of
               this experiment can be found",
               a(href="https://www.ncbi.nlm.nih.gov/pubmed/18587056", "here.", target="_blank")),
             br(),
             dataTableOutput("phenoData")),
            
    tabPanel("Data Visualization",
              br(),
             h4(p("BARPLOTS")),
             p("Here, we can explore the phenotype data attributes. First, lets look at the distribution of all the variables using barplots. A barplot (or barchart) is one of the most common type of plot. It shows the relationship between a numerical variable and a categorical variable."),
             selectInput("feat",label="Select variable:",choices=c("Treatment","Sex","Ancestry","ScanDate_Group"),multiple=FALSE,width="220px",selected = "Treatment"),
             plotOutput("barPlot",height="500px",width="800px"),
             br(),hr(),
             h4(p("HISTOGRAM")),
             p("A histogram shows the distribution of any numerical data using a single variable as input.The variable is cut into multiple bins, where the height of the bin represents the number of observations per bin. Here, we are using bins of size=2."),
             plotOutput("histPlot",height="500px",width="800px"),
             br(),hr(),
             h4(p("BOXPLOT")),
             p("The boxplot gives summary of numerical values. The line in the middle denotes the median while the upper and lower lines denote upper and lower quartiles."),
             plotOutput("boxPlot",height="500px",width="800px")))
))
