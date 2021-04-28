library(shiny)
library(shinythemes)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  
  # Application title
  tags$head(
    tags$style(
      ".title {margin: auto; width: 600px}"
    )
  ),
  tags$div(class="title", titlePanel("Exploratory Data Analysis and Visualization")),
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
             p(h5("Iris Dataset")),
             p("In the absence of a user-provided file, an example csv file of the 'iris' R dataset can be downloaded from below. Iris is a famous dataset used commonly for pattern recognition analysis.
               The data set contains 3 classes of 50 instances each, where each class refers to a type of iris plant. The other attributes include length and width of sepal and petal."),
             column(12, downloadButton(outputId="iris_data_download", label="Download iris dataset file"), align="left"), 
             br(),br(),hr(),
             p(h4("Load data")),
             p("You will be able to view the data and its contents in a table format below."),
             p("To load the default iris dataset, press the button provided below."),
             actionButton("iris", "Load Iris Data"),
             br(),
             p("To analyse your own data, upload a csv file of your choice."),
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
            p("Displayed below is a barplot showing the distribution for a categorical variable. Using the dropdown menu, select the variable of interest and generate different barplots."), 
            p("Variables with less than 2 or more than 15 unique categories (i.e. levels) are excluded from the list."),
            p("In the barplot, the x-axis corresponds to the category name and the y-axis corresponds to the frequency of each entry in the category."),
            p("\n"),
            textOutput("iris_disc"),
            br(),
            uiOutput('disc'),
            plotOutput("barplotUP", height="400px", width="auto"),
            br(), hr(),
            h4(p("Continuous variable distribution")),
            p("Displayed below is a histogram showing the distribution for a continuous variable by grouping the entries in small ranges. Using the dropdown menu, select the variable of interest and generate different histograms."),
            p("You can move the slider to change the number of bins used to determine the range."),
            p("In the plot, the x-axis corresponds to the measures of the continuous variable selected and the y-axis corresponds to the frequency of each measure of the selected variable within the range set by the bin numbers."),
            p("\n"),
            textOutput("iris_cont"),
            br(),
            uiOutput('cont'),
            br(),
            sidebarLayout(sidebarPanel(sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 20, value = 5)),
            plotOutput("histPlotUP", height="400px", width="auto")),
            br()),
    
    tabPanel("Bivariate Analysis", br(),
            p("Relationship between pairs of selected variables in the uploaded dataset."),
            uiOutput('bdisc'),
            uiOutput('bcont'),
            h4(p("Barplots of mean values")),
            p("Displayed below is a barplot for the selected categorical and continuous variable pair. Using the dropdown menu, select the variables of interest and generate different barplots."),
            p("The x-axis of the plot corresponds to the levels of the selected categorical variable and the y-axis corresponds to the mean of the continuous variable for each subset of data."),
            p("\n"),
            textOutput("iris_bdisc"),
            br(),
            plotOutput("fbarplotUP", height="400px", width="auto"),
            textOutput("cont_mean"),
            br(),
            h4(p("Boxplots of all values")),
            p("Displayed below is a boxplot showing the distribution of the selected continous variable as it is split into levels of the selected categorical variable. Using the 
            dropdown menu, select the variables of interest and generate different boxplots."),
            p("The x-axis corresponds to the levels of the selected categorical variable and the y-axis corresponds to the selected continuous variable."),
            p("Notice that the lines in the middle of the boxplots are the same value as the mean values in the barplot above. These lines represent the 50th percentile or median.
              The box itself represents the interquartile range (IQR). The top line of the box represents 75th percentile while bottom line represents 25th percentile. 
              The top and bottom horizontal lines outside the end of the box, i.e. the whiskers, correspond to the maximum (75th percentile+ 1.5*IQR) and minumum (25th percentile+ 1.5*IQR).
              The points that lie outside this range are called the outliers and they are numerically distant from the data distribution."),
            p("\n"),
            textOutput("iris_bcont"),
            br(),
            plotOutput("boxPlotUP", height="400px", width="auto"),
            br(),
            textOutput("sptitle"),
            plotOutput("scatterplotDT", height="400px", width="auto")))
  
))
