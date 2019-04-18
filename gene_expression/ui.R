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
             fluidRow(column(12,p(h4("Normalizing Raw Expression Data")),
             p("Normalize gene expression raw data using robust multi-array average (RMA) method."),br(),
             imageOutput("RMAimage")), 
             div(style="margin-left: 225px", column(2,actionButton("rma","Normalize with RMA",width="150px")))),
             br(),hr(),
             fluidRow(column(12,p(h4("Principal Component Analysis(PCA")),
             p("PCA demonstrates information of the expression dataset in a reduced number of dimensions. 
               Clustering and PCA plots enable to assess to what extent arrays resemble each other, 
               and whether this corresponds to the known resemblances of the samples."),
             selectizeInput("var", strong("Features:"), choices=c("Treatment","Ancestry","ScanDate_Group"),width="200px",selected="Treatment"),
             plotOutput("PCAplot",width="600px",height = "700px"))),br(),hr(),
             fluidRow(column(12,p(h4("Identifying Potential Outliers")),
             p("The log2-transformed/normalized intensity distributions of all samples (arrays) are 
               expected to have the similar scale (i.e. the similar positions and widths of the boxes). 
               Outlier detection is applied by computing a Kolmogorov-Smirnov statistic (Ka) between 
               log-intensity distribution for one array and the pooled array data, 
               where an array with a Ka beyond the upper whisker is designated as an outlier."),               
             imageOutput("QCimage")))),#br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),hr(),
  

    tabPanel("Differential Expression Results",
             br(),
             p(h3("Identifying Differentially Expressed Genes")),
             fluidRow(column(12,p(h4("Volcano Plots")),
             p("Volcano plot (probes with a q-value <0.05 are present in red)"),
             imageOutput("volcanoPlot"))),
             br(),hr(),
             fluidRow(column(12,p(h4("Top 50 Differentially Expressed Genes")),
             p("Show top 50 probes sorted by un-adjusted p-values"),
             dataTableOutput("DEtable"))),
             br(),hr(),
             fluidRow(column(12,p(h4("Checking Results of Top Genes")),
             uiOutput("genesAvail"), tags$head(tags$style(type="text/css", "#curr_gene {width: 190px}")),
             plotOutput("barPlot"))),
             br(),br(),br(),hr(),
             fluidRow(column(12,p(h4("Heatmap of Top Differentially Expressed Genes")), # # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("probes",
                             "Number of top probes/genes:",
                             min = 50,
                             max = 500,
                             value = 250)),
                 
            plotOutput("heatMap")))), 
            br()
    )
  )
))

