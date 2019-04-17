library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  
  # Application title
  titlePanel(h2("Gene Expression Microarray Tutorial", align="center")),

  tabsetPanel(
    tabPanel("Introduction",
             br(),
             p(h4("Effects of Smoking on Alveolar Macrophages")),
             p("Smoking cigarettes and exposure to cigarette smoke is known to negatively affect a person's health."),
             p("We analyzed a publicly available dataset that is in the Gene Expression Omnibus (GEO)", 
               a(href="http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE8823", "GSE8823", target="_blank"),
               "to learn about expression microarray analysis. This dataset was for an experiment in which
               investigators compared gene expression profiles of bronchoalveolar lavage fluid obtained from 11 non-smokers
               versus 13 smokers (mean smoking use was 36 pack-years). The scientific paper published to report the results of
               this experiment can be found",
               a(href="https://www.ncbi.nlm.nih.gov/pubmed/18587056", "here.", target="_blank")),
             br(),
             p(h4("Subject Characteristics")),
             p("We used the information deposited by study authors into GEO to learn some basics about the experiment's study design, including what tissue was studied, 
               the characteristics of individuals selected for the study."),
             dataTableOutput("phenoTable"),
             br(),
             p(h4("Raw Gene Expression Microarray Data")),
             p("From GEO, we also found out what microarray chip was used to obtain gene expression data (it was one called 
               Affymetrix HG-U133 Plus 2.0). Next, we obtained 24 files, each corresponding to the image intensities 
               captured across each chip for each individual."),
             br(),
             p(h4("To Learn More")),
             p("For this tutorial, we analyzed data and provide it for you to explore gene expression data. 
               If you are interested in analyzing gene expression microarray data on your own, 
               take a look at",
               a(href="https://github.com/HimesGroup/raved", "raved,", target="_blank"),
               "the pipeline we used to analyze the data shown here. You can perform a search in", 
               a(href="https://www.ncbi.nlm.nih.gov/geo/", "GEO", target="_blank"),
               "and choose any microarray dataset that is of interest to you."),
             br(),
             p("This app was created with",
               a("RStudio's Shiny.", href="http://www.rstudio.com/shiny", target="_blank"))
             ),
    
    tabPanel("Quality Control",
             br(),
             p(h4("Normalizing Raw Expression Data")),
             br(),
             p(h4("Identifying Potential Outliers"))
             ),

    tabPanel("Differential Expression Results",
             br(),
             p(h3("Identifying Differentially Expressed Genes")),
             p(h4("Volcano Plots")),
             p("Volcano plot (probes with a q-value <0.05 are present in red)"),
             imageOutput("volcanoPlot"),align="left",
             p(h4("Top 50 Differentially Expressed Genes")),
             p("Show top 50 probes sorted by un-adjusted p-values"),
             dataTableOutput("DEtable"),
             #p("Volcano Plot, Table of Top DE Results"),
             br(),
             p(h4("Checking Results of Top Genes")),
             uiOutput("genesAvail"), tags$head(tags$style(type="text/css", "#curr_gene {width: 190px}")),
             plotOutput("barPlot"),
             # # Sidebar with a slider input for number of bins 
             # sidebarLayout(
             #   sidebarPanel(
             #     sliderInput("bins",
             #                 "Number of bins:",
             #                 min = 1,
             #                 max = 50,
             #                 value = 30)
             #     ),
               mainPanel(
                 # Show a plot of the generated distribution
                 plotOutput("distPlot"))
             ),
             br()
             #p(h4("Heatmap of Top Differentially Expressed Genes"))

    )
  )
)

