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
             p(h4("Data analysis and visualization")),
             p("This app helps convey basic concepts related to data analysis and visualization."),
             p(h4("How to use this app")),
             p("You can upload a file with data of your choice to carry out basic data analysis. First, save a table-oriented data file into CSV (comma-separated values) format, which can be done from programs that focus on data tables like Microsoft Excel.
                After loading a CSV file with the button in the next section, the Univariate Analysis tab can be used to examine characteristics of each continuous and categorical variable. 
                The Bivariate Analysis tab can be used to explore relationships between pairs of variables using bar plots and box plots."),
             p("In absence of a user-provided file, an example csv file of 'iris' R dataset can be downloaded:"),
             column(12, downloadButton(outputId="iris_data_download",label="Download iris dataset file"), align="left"), 
             br(),br(),hr(),
             p(h4("Loaded data")),
             # Input: Select a file ----
             column(12,
             fluidRow(fileInput("file1", "Upload CSV File:", multiple=TRUE, accept=c("text/csv","text/comma-separated-values,text/plain",".csv"))),
             br(),
             dataTableOutput("contents")),
             br(),hr()),

    tabPanel("Univariate Analysis",
            br(),
            h4(p("Continuous variables in the uploaded dataset with more than one unique entry:")),
            dataTableOutput("varcData"),
            br(),
            h4(p("Categorical variables in the uploaded dataset with more than one unique entry:")),
            dataTableOutput("vardData"),
            br(), hr(),
            h4(p("Categorical variable distribution")),
            p("Barplots show the distribution of categorical variables. Displayed next are barplots for those variables with 2-15 levels."),
            uiOutput('disc'),
            plotOutput("barplotUP",height="400px",width="auto"),
            br(), hr(),
            h4(p("Continuous variable distribution")),
            p("Histograms show the distribution of continuous variables by grouping entries in small ranges. Move the slider to change the number of bins used."),
            uiOutput('cont'),
            br(),
            sidebarLayout(sidebarPanel(sliderInput(inputId = "bins",label = "Number of bins:", min = 1, max = 20, value = 5)),
            plotOutput("histPlotUP",height="400px",width="auto")),
            br()),
    
    tabPanel("Bivariate Analysis", br(),
            p("Relationship between pairs of variables in the uploaded dataset."),
            uiOutput('bdisc'),
            uiOutput('bcont'),
            h4(p("Barplots of mean values")),
            p("These barplots have split a categorical variable accross its levels along the x-axis and display the mean measure of the continuous variable for each subset of data.
              In the case of pollution data, the average measurement per site is displayed."),
            plotOutput("fbarplotUP", height="400px", width="auto"),
            br(),
            p("Boxplots show characteristics of the overall distribution of a continous variable as it is split into levels of a categorical variable."),
            plotOutput("boxPlotUP", height="400px", width="auto"),
            br(),
            textOutput("sptitle"),
            plotOutput("scatterplotDT", height="400px", width="auto")))
  
))
