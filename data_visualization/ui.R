library(shiny)
library(shinythemes)
library(shinyWidgets)
#library(rCharts)

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
             h4(p("Example Dataset:")),
             p("Here, as an example, we have the phenotype information for the Gene Expression Omnibus (GEO) dataset : ", 
               a(href="http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE8823", "GSE8823.", target="_blank"),"This dataset was for an experiment in which
               investigators compared gene expression profiles of bronchoalveolar lavage fluid obtained from 11 non-smokers
               versus 13 smokers (mean smoking use was 36 pack-years). The scientific paper published to report the results of
               this experiment can be found",
               a(href="https://www.ncbi.nlm.nih.gov/pubmed/18587056", "here.", target="_blank")),
             br(),
             dataTableOutput("phenoData")),
            
    tabPanel("Example Dataset",
              br(),
             h3(p("Univariate Analysis")),
             h4(p("Categorical Variables")),
             p("Here, we can explore the phenotype data attributes. First, lets look at the distribution of all the variables using barplots. A barplot (or barchart) is one of the most common type of plot. It shows the relationship between a numerical variable and a categorical variable."),
             selectInput("feat",label="Select variable:",choices=c("Treatment","Sex","Ancestry","ScanDate_Group"),multiple=FALSE,width="220px",selected = "Treatment"),
             plotOutput("barPlot",height="500px",width="800px"),
             br(),hr(),
             p("We can explore the phenotype data attributes with respect to the two main groups - smoker versus non-smoker."),
             selectInput("var",label="Select feature:",choices=c("Sex","Ancestry","ScanDate_Group"),multiple=FALSE,width="220px",selected = "Sex"),
             plotOutput("fbarPlot",height="500px",width="1100px"),
             hr(),
             h4(p("Continuous Variables")),
             p("A histogram shows the distribution of any numerical data using a single variable as input.The variable is cut into multiple bins, where the height of the bin represents the number of observations per bin. Here, we are using bins of size=2."),
             plotOutput("histPlot",height="500px",width="800px"),
             br(),hr(),
             h3(p("Bivariate Analysis")),
             h4(p("Continuous Variable vs. Categorical Variable")),
             p("The boxplot gives summary of numerical values. The line in the middle denotes the median while the upper and lower lines denote upper and lower quartiles."),
             selectInput("comp",label="Select feature:",choices=c("Sex","Ancestry","Treatment"),multiple=FALSE,width="220px",selected = "Treatment"),
             plotOutput("boxPlot",height="500px",width="800px")),
    
    tabPanel("Data Visualization",
            br(),
            h3(p("Overview of data")),
            h4(p("The continuous variables in the uploaded dataset are as follows:")),
            conditionalPanel(condition = "!is.null(output.contents)", dataTableOutput("varcData"),
            br(),
            h4(p("The categorical variables in the uploaded dataset are as follows:")),
            dataTableOutput("vardData"),
            br(), hr(),
            h3(p("Univariate Analysis")),
            h4(p("Categorical Variables")),
            p("The distribution for the categorical variables with number of levels between 2-10 is displayed here."),
            uiOutput('disc'),
            plotOutput("barplotUP",height="500px",width="800px"),
            br(), hr(),
            h4(p("Continuous Variables")),
            uiOutput('cont'),
            plotOutput("fbarplotUP",height="500px",width="800px"),
            br(),
            p("Histogram showing distribution of continuous variables"),
            plotOutput("histPlotUP",height="500px",width="800px"),
            br(),
            p("Boxplot showing summary of variables"),
            plotOutput("boxPlotUP",height="500px",width="800px"))
            ))
  
))
