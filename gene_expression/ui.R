library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  
  # Application title
  tags$head(
    tags$style(
      ".title {margin: auto; width: 600px}"
    )
  ),
  tags$div(class="title", titlePanel("Gene Expression Microarray Analysis")),
  hr(),
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
             downloadButton(outputId="pheno_data_download", label="Download Phenotype Data"),
             br(),hr(),
             p(h4("Raw Gene Expression Microarray Data")),
             p("From GEO, we also found out what microarray chip was used to obtain gene expression data (it was one called 
               Affymetrix HG-U133 Plus 2.0). Next, we obtained 24 files, each corresponding to the image intensities 
               captured across each chip for each individual."),
             #fluidRow(column(12, align="center",img(src="http://public.himeslab.org/k12_images/affymetrix.tiff",height="380px",width="550px", alt="Affymetrix Chip"))),
             fluidRow(column(12, align="center",imageOutput("Affyimage",height="380px",width="550px"))),
             hr(),
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
    tabPanel("Sample Characteristics",
             br(),
             h3(p("Univariate Analysis")),
             fluidRow(column(12,h4(p("Categorical Variables")),
             p("Here, we can explore the phenotype data attributes. First, lets look at the distribution of all the variables using barplots.
               A barplot (or barchart) is one of the most common type of plot.
               It shows the relationship between a numerical variable and a categorical variable."),
             selectInput("feat",label="Select variable:",choices=c("Treatment","Sex","Ancestry","ScanDate"),multiple=FALSE,width="220px",selected = "Treatment"),
             plotOutput("barPlot",height="400px",width="750px"),
             br(),hr(),
             p("We can explore the phenotype data attributes with respect to the two main groups - smoker versus non-smoker."),
             selectInput("svar",label="Select feature:",choices=c("Sex","Ancestry","ScanDate"),multiple=FALSE,width="220px",selected = "Sex"),
             plotOutput("fbarPlot",height="400px",width="1350px"))),
             hr(),
             fluidRow(column(12,h4(p("Continuous Variables")),
             p("A histogram shows the distribution of any numerical data using a single variable as input.
               The variable is cut into multiple bins, where the height of the bin represents the number of observations per bin.
               Here, we are using bins of size=2."),
             sidebarLayout(sidebarPanel(sliderInput(inputId = "bins",label = "Number of bins:",min = 1,max = 20,value = 5)),
             plotOutput("histPlot",height="400px",width="550px")))),
             br(),br(),br(),br(),br(),br(),br(),hr(),
             fluidRow(column(12,h3(p("Bivariate Analysis")),
             h4(p("Continuous Variable vs. Categorical Variable")),
             p("The boxplot gives summary of numerical values. The line in the middle denotes the median while the upper
               and lower lines denote upper and lower quartiles."),
             selectInput("comp",label="Select feature:",choices=c("Sex","Ancestry","Treatment"),multiple=FALSE,width="220px",selected = "Treatment"),
             plotOutput("sboxPlot",height="400px",width="550px")))),
    
    tabPanel("Quality Control",
             br(),
             fluidRow(column(12,p(h4("Normalizing Raw Expression Data")),
             p("Normalize gene expression raw data using robust multi-array average (RMA) method."),br(),
             imageOutput("RMAimage",height = "300px")), 
             div(style="margin-left: 225px", column(2,actionButton("rma","Normalize with RMA",width="150px")))),
             br(),hr(),
             fluidRow(column(12,p(h4("Principal Component Analysis (PCA)")),
             p("PCA demonstrates information of the expression dataset in a reduced number of dimensions. 
               Clustering and PCA plots enable to assess to what extent arrays resemble each other, 
               and whether this corresponds to the known resemblances of the samples."),
             selectizeInput("var", strong("Features:"), choices=c("Treatment","Ancestry","ScanDate"),width="200px",selected="Treatment"),
             plotOutput("PCAplot",width="600px",height = "400px"))),br(),hr(),
             fluidRow(column(12,p(h4("Identifying Potential Outliers")),
             p("The log2-transformed/normalized intensity distributions of all samples (arrays) are 
               expected to have the similar scale (i.e. the similar positions and widths of the boxes). 
               Outlier detection is applied by computing a Kolmogorov-Smirnov statistic (Ka) between 
               log-intensity distribution for one array and the pooled array data, 
               where an array with a Ka beyond the upper whisker is designated as an outlier."),               
             imageOutput("QCimage",height="800px",width="550px"), br(), hr(),
             #img(src="http://public.himeslab.org/k12_images/Outlier_barplot.tiff",height="800px",width="550px", alt="QC Outliers"), br(), hr(),
             p("The intensity curves of all samples (arrays) are expected to have the similar shapes and ranges. 
               Samples with deviated curves are likely to have problematic experiments. 
               For example, high levels of background will shift an array’s distribution to the right. 
               Lack of signal diminishes its right right tail. A bulge at the upper end of the intensity range often indicates signal saturation."),
             imageOutput("DCimage",height="500px",width="610px"),br(),br()))),
             #img(src="http://public.himeslab.org/k12_images/Density_curves.tiff",height="500px",width="610px", alt="QC Outliers"),br(),br()))),
  

    tabPanel("Differential Expression Results",
             br(),
             p(h3("Identifying Differentially Expressed Genes")),
             fluidRow(column(12,p(h4("Volcano Plots")),
             p("Volcano plot (probes with an adjusted p-value <0.05 are present in red)"),
             imageOutput("volcanoPlot",height="330px",width="390px"))),
             #img(src="http://public.himeslab.org/k12_images/Volcano_plot_edited.tiff",height="330px",width="390px", alt="Volcano Plot"))),
             br(),hr(),
             fluidRow(column(12,p(h4("Top 50 Differentially Expressed Genes")),
             p("Show top 50 probes sorted by p-values"),
             dataTableOutput("DEtable"))),
             br(),hr(),
             fluidRow(column(12,p(h4("Checking Results of Top Genes")),
             uiOutput("genesAvail"), tags$head(tags$style(type="text/css", "#curr_gene {width: 190px}")),
             plotOutput("boxPlot",height="auto",width="auto"))),
             br(),br(),br(),hr(),
             fluidRow(column(12,p(h4("Heatmap of Top Differentially Expressed Genes")), # # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("probes",
                             "Number of top probes/genes:",
                             min = 2,
                             max = 50,
                             value = 50)),
                 
            plotOutput("heatMap")))), 
            br(),br(),br(),br()
    )
  )
))

