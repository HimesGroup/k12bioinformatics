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
    tabPanel("Measurement Characteristics",br(),
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
    tabPanel("Map",br(),
             mainPanel(
             leafletOutput("mymap",height = 700)),
             sidebarPanel(
             selectInput("type","Select Variable:",choices = c("Temperature","Humidity","DustPM","AirQuality") , selected = "AirQuality"),
             dateRangeInput("dates", label = "Date Range:", start = "2019-04-17", end = Sys.Date()),
             uiOutput("Name"),br())))
             
             
))