
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
               The second and third tabs of this app show trends of PM2.5 and CO at eight locations in the United States. To explore pollution trends across the country, please click the map below to access the Pollution-Associated Risk Geospatial Analysis SITE (PARGASITE) application."),
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
             p("Fine particulate matter (PM2.5) is an air pollutant that has an effect on human health at elevated levels. High levels of PM2.5 caused around 4.2 million premature deaths globally in 2015.
               Thus, it is important to enforce PM2.5 ambient air quality standards to reduce the burden of disease and mortality. Optimum level of PM2.5 should be below 12 ug/m3, levels between 
               35-200 ug/m3 are progessively unhealthy, and 250+ ug/m3 are hazardous."),br(),
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
    tabPanel("K12 Map",br(),
             mainPanel(
               p("The map below displays the geospatial distribution of available sensor data gathered by high school teachers and students using portable low-cost pollution sensors."),
               leafletOutput("mymap", height = 700)),
             sidebarPanel(
               selectInput("type","Select Variable:", choices = c("PM2.5","CO") , selected = "PM2.5"),
               dateRangeInput("dates", label = "Date Range:", start = "2019-04-17", end = Sys.Date()),
               uiOutput("Group"),
               uiOutput("Name"),
               downloadButton(outputId="selected_data_download", label="Download selected data"),br())),
    tabPanel("K12 Summary Plots",br(),
             h3(p("Overall characterisitics of K-12 Crowdsourced Pollution Data")),
             h4(p("Univariate Plots - PM2.5")), 
             div(style="display: inline-block;",plotOutput("distBoxplot",height="300px",width = "300px")),
             div(style="display: inline-block;",plotOutput("disHist",height="300px",width = "500px")),br(),br(),hr(),
             h4(p("Univariate Plots - CO")), 
             div(style="display: inline-block;",plotOutput("pdistBoxplot",height="300px",width = "300px")),
             div(style="display: inline-block;",plotOutput("pdisHist",height="300px",width = "500px")),br(), hr(),
             h4(p("Bivariate Plot")),
             div(style="display: inline-block;",plotOutput("Scatplot",height="600px",width = "900px")),br(), br()),
    tabPanel("Crowdsourced Sensor Data",br(),
             h3(p("Sensor-based Analysis of Pollution in the Philadelphia Region with Information on Neighborhoods and the Environment")),
             p("Here, we have an interactive geospatial-analysis tool that allows users to visualize pollution and other 
                  data throughout the Greater Philadelphia Area."),
             p(h4("Data Sources:")),
             p(h5("Sensor Data")),
             p("We acquired data for temperature, humidity and pollutants like PM2.5, PM1, and PM10 measured by portable sensors 
             such as AirBeam from Habitat Map website and low-cost stationary sensors such as Purple Air from their own official website. 
             Additional AirBeam data taken by members of our lab between October 23, 2017, and July 26, 2018 were included as well."),
             p(h5("EPA Data")),
             p("Daily-averaged measurements of PM2.5, PM10, SO2, NO2, O3, and CO concentrations from EPA monitors were 
               downloaded from the EPA Air Data Portal for the greater Philadelphia region."),
             p(h5("Traffic Data")),
             p("Traffic volume line data containing the AADT of all road segments in Pennsylvania was downloaded from the Pennsylvania Department 
               of Transportation Open Data Portal and subsetted to the six GPA counties in Pennsylvania (Berks, Bucks, Chester, Delaware, Montgomery, Philadelphia.
               The traffic data is recorded as Annual Average Daily Traffic (AADT), which is the typical daily traffic volume of a segment of road calculated by dividing the 
               total number of vehicles traveling across a road segment in a year by 365."),
             p(h4("Generate Maps:")),
             p("The panel below provides options to chose the sensors, and range of date, time, latitude and longitude. Adjust the parameters to your desired values, and then click \"Load Map\" to display the corresponding map. 
               Slider bars can be fine-tuned using arrow keys. Depending on your selections, the data will be selected to generate geospatial estimates for visualization."), br(), 
             wellPanel(fluidRow(
               column(4,#Dropdown list for selecting data by sensor
                      pickerInput("sensors",
                                  label = "Low-cost pollution sensors to include",
                                  choices = sensor.names,
                                  multiple = TRUE,
                                  selected = sensor.names,
                                  options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None",
                                                 `selected-Text-Format`= "count"))), #End column 1
                 column(4,#Dual textbox for entering in a date range
                        dateRangeInput("dates", label = "Date range", start = "2015-06-01", end = "2019-12-31", startview = "decade",
                                       min = "2015-06-01", max = "2019-12-31"),
                        #Slider bar for selecting a time-of-day-range
                        sliderTextInput("times",
                                        label = "Time of day",
                                        choices = hours,
                                        selected = c("00:00", "23:59"),
                                        force_edges = TRUE,
                                        width = "50%"
                        )), #End column 2
                 column(4,#Slider for selecting raster boundaries
                        sliderInput("lat.range",
                                    label = "Latitude Range",
                                    min = 38.85,
                                    max = 40.60,
                                    value = c(39.87, 40.17),
                                    step = 0.00001,
                                    sep = ""
                        ),
                        sliderInput("lon.range",
                                    label = "Longitude Range",
                                    min = -76.40,
                                    max = -74.40,
                                    value = c(-75.28, -74.96),
                                    step = 0.00001,
                                    sep = ""
                        )) )), #End column 3
             column(12,fluidRow(actionButton("go", "Load Map",class = "btn-warning")), align="center"), 
             br(), br(),
             tabsetPanel(tabPanel("Philadelphia Region Map", br(),
                                  p("Here, you can visualize the estimates for a selected variable across the Philadelphia region."),
                                  p("Instruction:"), 
                                  p("1. Choose the variable of interest from the options displayed in the upper right corner"),
                                  p("2. You can click and drag, or use the zoom in and out buttons on the left to navigate the map."), 
                                  p("3. You can click at the center of each colored square to open a pop-up that provides the estimate measures for all available variables for that location."),
                                  p("4. The pop-ups display a 'Google Earth URL'. You can click on it to navigate to Google Earth to visualize the geographical area for that specific region."),
                                  p("5. The 'Download Data' button at the bottom allows you to download the data for the selected variable in the map."),br(), 
                                 column(6,uiOutput("download.ras"),align="right"), #hr(),br(),
                                 column(6,uiOutput("co.ras"),align="left"), hr(),br(),
                                 column(1),
                                 column(10, leafletOutput("int.map", height = 700)),
                                 column(1),
                                 br(), br()),
                         tabPanel("Grid Maps", br(),
                                  p("Here, you can visualize multiple selected variables in a grid to compare estimates geospatially."),
                                  p("In this map, you can choose different plot variables and display synchronized maps side-by-side.
                                  For example, you can select PM2.5, Humidity, Traffic, and Int. EPA PM2.5 plot to display four different maps at the same time to compare them."),
                                  p("Instruction:"), 
                                  p("1. Choose multiple variables from the list 'Plot variables'"),
                                  p("2. Click 'Update grid'."), 
                                  p("3. You can click and drag the map to move and change the area displayed."),br(), hr(),
                                  fluidRow(column(6, br(), fluidRow(pickerInput('grid.vars',
                                                      #label = 'Plot variables',
                                                      choices = list(
                                                        "PM\u2082.\u2085" = "pm25", 
                                                        "PM\u2081" = "pm1",
                                                        "PM\u2081\u2080" = "pm10",
                                                        "Temperature" = "temp",
                                                        "Humidity" = "humid",
                                                        # "Crime" = "crime",
                                                        # "Area Deprivation Index" = 'pov',
                                                        "Traffic" = 'tr',
                                                        "EPA CO" = 'co',
                                                        "EPA PM\u2082.\u2085 plot" = 'epa.pm25',
                                                        "EPA PM\u2081\u2080 plot" = 'epa.pm10',
                                                        "EPA SO\u2080\u2082 plot" = 'epa.so2',
                                                        "EPA NO\u2080\u2082 plot" = 'epa.no2',
                                                        "EPA O\u2083 plot" = 'epa.o3'
                                                        # "Int. EPA CO plot" = 'epa.co'
                                                      ),
                                                      multiple = TRUE,
                                                      selected = c('pm25', 'temp', 'humid'),
                                                      options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None",
                                                                     `selected-Text-Format`= "count")), align="right"),style="z-index:10000;"),
                                         column(6,
                                                br(),
                                                actionButton('grid.update', 'Update Grid',class = "btn-primary"), align="left")), br(),
                                  column(1),
                                  column(10, uiOutput("all.maps", height = 700)),
                                  column(1),
                                  br(), br())) #End inner tabPanel and tabsetPanel
     ))
             
))