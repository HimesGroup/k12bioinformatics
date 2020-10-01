library(shiny)
library(shinythemes)
library(leaflet)
library(ggiraph)
library(shinyWidgets)

#Set choices of cities for UI
cities <- c("Los Angeles, CA"="CA", "Miami, FL"="FL", "Billings, MO"="MO", "Standing Rock, NM"="NM",
            "Midtown Manhattan, NY"="NY","Portland, OR"="OR","Philadelphia, PA"="PA",
            "Pierre, SD"="SD")

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),

  # Application title
  tags$head(
    tags$style(
      ".title {margin: auto; width: 600px}"
    )
  ),
  tags$div(class="title", titlePanel("Air Pollution Visualization")),
  hr(),
  
  tabsetPanel(
    tabPanel("Introduction",
             br(),
             h4(p("Air Pollution")), 
             HTML("Air pollution is a mixture of particulate matter (PM) and gaseous chemicals produced by human activity and natural processes. 
                Exposure to pollutants impacts human health and is associated with all-cause mortality and several diseases. For example, people with asthma are more likely to have worsening symptoms or exacerbations when they are exposed to pollutants.
                PM is composed of airborne particles that are classified by size, where PM2.5 refers to particles with diameter <2.5μm.
                Gas-phase pollutants include ozone (O<sub>3</sub>), nitrogen dioxide (NO<sub>2</sub>), carbon monoxide (CO), sulfur dioxide (SO<sub>2</sub>) and volatile organic compounds (VOCs)."), 
             
             h4(p("Environmental Protection Agency (EPA) Regulatory Monitors")), 
             p("The United States EPA monitors levels of major air pollutants to reduce their impact on human health in accordance with the",
               a("Clean Air Act", href="https://www.epa.gov/clean-air-act-overview", target="_blank"),
               "which established National Ambient Air Quality Standards (NAAQS) for six",
               a("criteria pollutants.", href="https://www3.epa.gov/airquality/cleanair.html"),
               "Reference monitors placed across the United States monitor compliance to air quality legislation.
               The second and third tabs of this app show trends of PM2.5 and CO at eight locations in the United States."),
             div(style="display: inline-block;", tags$a(imageOutput("epa_monitor",height="280px",inline = T), href="http://pargasite.org/",target="_blank")),
             #div(style="display: inline-block;", tags$a(img(src="http://public.himeslab.org/k12_images/EPA_Monitors.png",height="280px",alt="EPA Monitors"), href="http://pargasite.org/",target="_blank")),
             
             h4(p("Portable Pollution Sensors")),
             p("Concern for pollution’s effect on health and broad demand for accessible environmental monitoring have led researchers and manufacturers to develop 
              low-cost, portable pollution sensors. Although these sensors are less accurate and reliable than reference monitors, they provide some information
              on locations that may be of concern to citizens. We assembled our own PM and CO sensors using commercially available components for teaching purposes.
              Students are handed a sensor, Android device and charger to take measures:"),
             div(style="display: inline-block;", imageOutput("sensor_setup", height= "200px",inline = T)),
             #div(style="display: inline-block;", tags$a(img(src="http://public.himeslab.org/k12_images/sensor_setup.png",height="200px",alt="Sensor Package"), target="_blank")),
             p("Sensors send measures to a smartphone (paired via Bluetooth), and data from all smartphones is gathered in a Google Spreadsheet:"),
             div(style="display: inline-block;", imageOutput("sensor_working", height= "200px",inline = T)),
             #div(style="display: inline-block;", tags$a(img(src="http://public.himeslab.org/k12_images/sensor_working.png",height="200px",alt="How Sensors Work"), target="_blank")),

             br(),
             h4(p("Crowdsourced Pollution Measures")),
             p("The following table shows currently available sensor data gathered by high school teachers and students."), 
             span(textOutput("Warning"),style="color:red"), br(),
             dataTableOutput("airqdata")),

    tabPanel("EPA Measures Map", br(),
             h3(p("PM2.5 measures in eight U.S. cities")),
             p("Select from among the cities of Philadelphia, New York, Los Angeles, Miami, Pierre, Billings, Standing Rock and Portland to see average PM2.5 data for September 2017. 
               More EPA data can be visualized with", a(href="http://pargasite.org/","pargasite.", target="_blank")),
             sidebarLayout(position="right",
               sidebarPanel(checkboxGroupInput("kcity", "Location:",
                                             choices=cities, selected=cities)),
               mainPanel(leafletOutput("kmap", width=700, height=500))),
             hr(),

             h3(p("Sept 2017 mean PM2.5 measures")),
             plotOutput("kbarPlot", width="700px"), 
             p("Using the button below, you can download a file of daily PM2.5 averages for Sept 2017 for an EPA monitor nearest to each of these sites.", 
               "Use this file to get your own averages per site. How do they compare to the measures in the plot above?"),
             downloadButton(outputId="EPA_data_download", label="Download EPA Data"), br(),br(),
             hr(),
    
             h3(p("Selected Cities")),
             p("What differences in PM2.5 do you expect in these different locations across the U.S.?"),
             div(style="display: inline-block;",tags$a(imageOutput("philadelphia",height="200px",width="250px"), href="https://cdn.vox-cdn.com/thumbor/iz1UYrej5uzUbQZrtlu2tAxkoV4=/0x0:5959x3973/1200x900/filters:focal(2504x1511:3456x2463)/cdn.vox-cdn.com/uploads/chorus_image/image/54302495/shutterstock_618667091.0.jpg",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("newyork",height="200px",width="250px"),href="https://imgs.6sqft.com/wp-content/uploads/2016/06/13172431/Midtown-Skyline-in-2020.jpg",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("los_angeles",height="200px",width="250px"),href="https://upload.wikimedia.org/wikipedia/commons/3/30/Echo_Park_Lake_with_Downtown_Los_Angeles_Skyline.jpg",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("miami",height="200px",width="250px"),href="https://img1.coastalliving.timeinc.net/sites/default/files/styles/landscape_3_2/public/image/2017/01/main/miami-florida-luxury-destination-2017-144863422.jpg",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("pierre",height="200px",width="250px"),href="https://www.cityofpierre.org/ImageRepository/Document?documentID=2587",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("billings",height="200px",width="250px"),href="https://www.visitmt.com/binaries/small/content/gallery/MTOT/responsive/media-carousel-h/cities-towns/billings/cvb-night-cityscape_credit-visit-billings.jpg",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("shiprock",height="200px",width="250px"),href="https://imgur.com/wMBC0",target="_blank")),
             div(style="display: inline-block;",tags$a(imageOutput("portland",height="200px",width="250px"),href="https://localadventurer.com/wp-content/uploads/2017/08/things-to-do-in-portland-bucket-list.jpg",target="_blank")),
             br(), br()),
    
    tabPanel("EPA Measures Plots",br(),
      h3(p("Seasonal variation in mean pollutant levels")),
      p("Levels of pollutants change over time. Well known differences in mean levels are observed at different times of the year. 
        What patterns do you notice across months? Are these changes similar each year? Do they differ by city?"),
      fluidRow(column(4,
                      selectInput("State","Location:", choices = cities, selected="PA")),
               column(4,
                      selectInput("Year","Year(s):",choices = seq(2007,2017) , selected = "2007",multiple = TRUE))
      ),
      hr(),
      h4(p("Mean PM2.5 levels")),
      #p("The distribution of EPA values of PM2.5 from 2007 to 2017."),
      ggiraphOutput("PMPlot",height="500px",width="700px"),br(), 
      h4(p("Mean CO levels")),
      #p("Mean CO levels across each year from 2007 to 2017."),
      ggiraphOutput("COPlot",height="500px",width="700px"),br(), 
      br()),

    tabPanel("Crowdsourced Map",br(),
             mainPanel(
               leafletOutput("mymap", height = 700)),
             sidebarPanel(
               selectInput("type","Select Variable:", choices = c("PM2.5","CO") , selected = "PM2.5"),
               dateRangeInput("dates", label = "Date Range:", start = "2019-04-17", end = Sys.Date()),
               uiOutput("Group"),
               uiOutput("Name"),
               downloadButton(outputId="selected_data_download", label="Download selected data"),br())),
  
    tabPanel("Crowdsourced Summary Plots",br(),
             h3(p("Overall characterisitics of Crowdsourced Pollution Data")),
             h4(p("Univariate Plots - PM2.5")), 
             div(style="display: inline-block;",plotOutput("distBoxplot",height="300px",width = "300px")),
             div(style="display: inline-block;",plotOutput("disHist",height="300px",width = "500px")),br(),br(),hr(),
             h4(p("Univariate Plots - CO")), 
             div(style="display: inline-block;",plotOutput("pdistBoxplot",height="300px",width = "300px")),
             div(style="display: inline-block;",plotOutput("pdisHist",height="300px",width = "500px")),br(), hr(),
             h4(p("Bivariate Plot")),
             div(style="display: inline-block;",plotOutput("Scatplot",height="600px",width = "900px")),br(), br()))
             
))