library(shiny)
library(shinythemes)
library(leaflet)


shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  # 
  # Application title
  titlePanel(h2("Air Pollution", align="center")),
  
  tabsetPanel(
    tabPanel("Introduction",
             br(),
             h4(p("Overview of Air Pollution Data")), 
             p("This app provides spatial visualization of air quality levels in Philadelphia. The data is available", 
               a(href = "https://docs.google.com/spreadsheets/d/1V5J_TuhfZTFBfPcg1JMavzFrbB2vavd3JMNX1f1oAQw/edit#gid=420394624","here.",target="_blank")),br(),
             dataTableOutput("airqdata")),
    tabPanel("EPA Measures in USA",br(),
             h3(p("PM2.5 measures across particular locations in the USA")),
             p("The locations mapped across the country are Philadelphia, New York, Los Angeles, Miami, Pierre, Billings and Standing Rock."),
             #p("(1). Philadelphia, PA"),
             div(style="display: inline-block;",imageOutput("PhImage",height= "200px")),
             #p("(2). New York, NY"),
             div(style="display: inline-block;",imageOutput("NYImage",height= "200px")),
             #p("(3). Los Angeles, CA"),
             div(style="display: inline-block;",imageOutput("LAImage",height= "200px")),
             #p("(4). Miami, FL"),
             div(style="display: inline-block;",imageOutput("MAImage",height= "200px")),
             #p("(5). Pierre, SD"),
             div(style="display: inline-block;",imageOutput("PRImage",height= "200px")),
             #p("(6). Billings, MO"),
             div(style="display: inline-block;",imageOutput("BLImage",height= "200px")),
             #p("(7). Standing Rock, NM"),
             div(style="display: inline-block;",imageOutput("SRImage",height= "200px")),br(), br(),
             mainPanel(leafletOutput("kmap",height = 700)),
             sidebarPanel(checkboxGroupInput("kcity", "Select Location:",
                                             choices = c("Philadelphia, PA"="PA", "Midtown Manhattan, NY"="NY", "Los Angeles, CA"="CA", "Miami, FL"="FL", 
                                                                                     "Pierre, SD"="SD", "Billings, MO"="MO", "Standing Rock, NM"="NM"), selected=c("PA","MO")), 
             plotOutput("kbarPlot",height="400px",width = "300px"),br(), br(),hr()),br(), br(),br(),hr(),
             h4(p("PM 2.5 values in Philadelphia (2017): ")),
             p("The distribution of EPA values of PM 2.5 for University City, Philadelphia in the year 2017."),
             plotOutput("PhPlot",height="550px",width = "900px"),br(), br()),
    
    tabPanel("Overview of sensor measures",br(),
             h3(p("Characterisitics of Air Pollution Data")),
             h4(p("Univariate Analysis:")),
             p("Choose two variables to plot:"),
             div(style="display: inline-block;",uiOutput("var")),
             div(style="display: inline-block;",uiOutput("pvar")),br(),br(),
             div(style="display: inline-block;",plotOutput("distBoxplot",height="400px",width = "400px")),
             div(style="display: inline-block;",plotOutput("disHist",height="400px",width = "800px")),br(),br(),
             div(style="display: inline-block;",plotOutput("pdistBoxplot",height="400px",width = "400px")),
             div(style="display: inline-block;",plotOutput("pdisHist",height="400px",width = "800px")),br(), hr(),
             h4(p("Bivariate Analysis:")),
             p("Relationship between the two selected variables:"),
             div(style="display: inline-block;",plotOutput("Scatplot",height="500px",width = "700px")),br(), hr(), br()),
    tabPanel("Map of sensor measures",br(),
             mainPanel(
             leafletOutput("mymap",height = 700)),
             sidebarPanel(
             selectInput("type","Select Variable:",choices = c("Temperature","Humidity","DustPM","AirQuality") , selected = "AirQuality"),
             dateRangeInput("dates", label = "Date Range:", start = "2019-04-17", end = Sys.Date()),
             uiOutput("Name"),br())))
             
             
))