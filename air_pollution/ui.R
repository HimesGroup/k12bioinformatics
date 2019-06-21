library(shiny)
library(shinythemes)
library(leaflet)
library(ggiraph)

cities <- c("Philadelphia, PA"="PA", "Midtown Manhattan, NY"="NY", "Los Angeles, CA"="CA", "Miami, FL"="FL", 
            "Pierre, SD"="SD", "Billings, MO"="MO", "Standing Rock, NM"="NM","Portland, OR"="OR")

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  # 
  # Application title
  titlePanel(h2("Air Pollution", align="center")),
  
  tabsetPanel(
    tabPanel("Introduction",
             br(),
             h4(p("Overview of Air Pollution Data")), 
             p("Exposure to pollutants impacts human health and is associated with multiple respiratory and other chronic diseases. 
               The major pollutant used to infer air quality index of a location is the particulate matter (PM 2.5). These are inhalable particles of diameters 2.5 micrometers or smaller.
              Other pollutants like ozone, SO2, NO2 and CO are hazardous to health, and the United States Environmental Protection Agency (EPA) is in-charge of monitoring their rising levels."), 
              p("Philadelphia is one of the most polluted cities in the Unites States, 
               and effective monitoring of its pollutant levels is crucial to maintain healthy air-quality standards in the city.
               This app provides spatial visualization of air quality levels in the United States, 
                with special focus on Philadelphia. Here, we are using the data acquired by students via pollution monitors at different locations in the USA. 
                The student-acquired data is available", 
               a(href = "https://docs.google.com/spreadsheets/d/1V5J_TuhfZTFBfPcg1JMavzFrbB2vavd3JMNX1f1oAQw/edit#gid=420394624","here.",target="_blank"),
               "Along with this, we are also using publicly available data from the EPA to look at trends of PM 2.5 levels at different locations 
               over multiple time periods."),br(),
             dataTableOutput("airqdata")),
    tabPanel("EPA Measures in USA",br(),
             h3(p("PM2.5 measures across particular locations in the USA")),
             p("The locations mapped across the country are Philadelphia, New York, Los Angeles, Miami, Pierre, Billings, Standing Rock and Portland. 
               The EPA PM 2.5 data was acquired from the R package", a(href="https://github.com/HimesGroup/pargasite","pargasite", target="_blank"),
               "developed by Himes Lab. The measures shown are for the month of September 2017."),
             div(style="display: inline-block;",tags$a(imageOutput("PhImage",height= "200px"),href="https://cdn.vox-cdn.com/thumbor/iz1UYrej5uzUbQZrtlu2tAxkoV4=/0x0:5959x3973/1200x900/filters:focal(2504x1511:3456x2463)/cdn.vox-cdn.com/uploads/chorus_image/image/54302495/shutterstock_618667091.0.jpg")),
             div(style="display: inline-block;",tags$a(imageOutput("NYImage",height= "200px"),href="https://imgs.6sqft.com/wp-content/uploads/2016/06/13172431/Midtown-Skyline-in-2020.jpg")),
             div(style="display: inline-block;",tags$a(imageOutput("LAImage",height= "200px"),href="https://upload.wikimedia.org/wikipedia/commons/3/30/Echo_Park_Lake_with_Downtown_Los_Angeles_Skyline.jpg")),
             div(style="display: inline-block;",tags$a(imageOutput("MAImage",height= "200px"),href="https://img1.coastalliving.timeinc.net/sites/default/files/styles/landscape_3_2/public/image/2017/01/main/miami-florida-luxury-destination-2017-144863422.jpg")),
             div(style="display: inline-block;",tags$a(imageOutput("PRImage",height= "200px"),href="https://www.cityofpierre.org/ImageRepository/Document?documentID=2587")),
             div(style="display: inline-block;",tags$a(imageOutput("BLImage",height= "200px"),href="https://www.visitmt.com/binaries/small/content/gallery/MTOT/responsive/media-carousel-h/cities-towns/billings/cvb-night-cityscape_credit-visit-billings.jpg")),
             div(style="display: inline-block;",tags$a(imageOutput("SRImage",height= "200px"),href="https://imgur.com/wMBC0")),
             div(style="display: inline-block;",tags$a(imageOutput("POImage",height= "200px"),href="https://localadventurer.com/wp-content/uploads/2017/08/things-to-do-in-portland-bucket-list.jpg")),br(), br(),
             mainPanel(leafletOutput("kmap",height = 500)),
             sidebarPanel(checkboxGroupInput("kcity", "Select Location:",
                choices = cities, selected=cities), 
             plotOutput("kbarPlot",height="380px",width = "300px"),br(), br(),hr()),br(), br(),br(),hr()),
    
    tabPanel("Seasonality of measures",br(),
      h3(p("PM 2.5 and CO values in 2007-2017")),
      selectInput("State","Select Location:",choices = c("Philadelphia, PA"="PA", "Midtown Manhattan, NY"="NY", "Los Angeles, CA"="CA", "Miami, FL"="FL", 
                              "Pierre, SD"="SD", "Billings, MO"="MO", "Standing Rock, NM"="NM","Portland, OR"="OR"), selected="PA"),
      selectInput("Year","Select Years:",choices = seq(2007,2017) , selected = "2007",multiple = TRUE),
      h4(p("PM 2.5 levels for the selected year(s).")),
      p("The distribution of EPA values of PM 2.5 from 2007 to 2017."),
      ggiraphOutput("PMPlot",height="500px",width="700px"),br(), 
      h4(p("CO levels for the selected year(s).")),
      p("The distribution of EPA values of CO from 2007 to 2017."),
      ggiraphOutput("COPlot",height="500px",width="700px"),br(), 
      
      br()),
    
    tabPanel("Overview of sensor measures",br(),
             h3(p("Characterisitics of Air Pollution Data")),
             h4(p("Univariate Analysis:")),
             p("Choose two variables to plot:"),
             div(style="display: inline-block;",uiOutput("var")),
             div(style="display: inline-block;",uiOutput("pvar")),br(),br(),
             div(style="display: inline-block;",plotOutput("distBoxplot",height="300px",width = "300px")),
             div(style="display: inline-block;",plotOutput("disHist",height="300px",width = "500px")),br(),br(),
             div(style="display: inline-block;",plotOutput("pdistBoxplot",height="300px",width = "300px")),
             div(style="display: inline-block;",plotOutput("pdisHist",height="300px",width = "500px")),br(), hr(),
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