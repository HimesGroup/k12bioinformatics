library(shiny)
library(shinythemes)
library(leaflet)



shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  
  tags$head(tags$style(HTML("
  .leaflet-top, .leaflet-bottom {
                            z-index: unset !important;
                            }

                            .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
                            z-index: 10000000000 !important;
                            }
                            "))),
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
             h4(p("Characterisitics of Air Pollution Data")),
             uiOutput("var"),br(),
             plotOutput("distBoxplot",width = "400px"),br(),
             plotOutput("disHist",width = "1000px")),
    tabPanel("Map",br(),
             mainPanel(
             leafletOutput("mymap",height = 700)),
             sidebarPanel(
             dateRangeInput("dates", label = "Date Range:", start = "2019-04-17", end = Sys.Date()),
             uiOutput("Name"),br())))
             
             
))