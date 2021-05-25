

library(shiny)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(navbarPage( title= "COVID-19 K-12 App",
                                 theme=shinytheme("flatly"),
                                  # ----------------------------------
                                  # tab panel 1 - Home
                                  tabPanel("Introduction",icon=icon("info",class=NULL,lib="font-awesome"),
                                        fluidRow(
                                               column(width=2,img(src="virus.png",height=100, width=150)),
                                               column(width=10, 
                                                      mainPanel(
                                                          tags$head(
                                                              tags$style(HTML("
                                                                   #renderprint {
                                                                   color:black;
                                                                   background:white;
                                                                   font-family: 'Times New Roman', Times, serif;
                                                                   font-size: 18px;
                                                                   text-align:left;
                                                                   }"))),
                                           htmlOutput("renderprint")))),br(),
                                           fluidRow(
                                               column(width=2,img(src="Symptoms2.jpg",height=100, width=150)),
                                               column(width=10, 
                                                      mainPanel(
                                                          tags$head(
                                                              tags$style(HTML("
                                                                   #renderprint3 {
                                                                   color:black;
                                                                   background:white;
                                                                   font-family: 'Times New Roman', Times, serif;
                                                                   font-size: 18px;
                                                                   text-align:left;
                                                                   }"))), htmlOutput("renderprint3")))),br(),
                                           fluidRow(
                                               column(width=2,img(src="prevention2.jpg",height=100, width=150)),
                                               column(width=10, 
                                                      mainPanel(
                                                          tags$head(
                                                              tags$style(HTML("
                                                                   #renderprint2 {
                                                                   color:black;
                                                                   background:white;
                                                                   font-family: 'Times New Roman', Times, serif;
                                                                   font-size: 18px;
                                                                   text-align:left;
                                                                   }"))), htmlOutput("renderprint2")))), br(),
                                           fluidRow(
                                               column(width=2,img(src="app.png",height=100, width=150)),
                                               column(width=10, 
                                                      mainPanel(
                                                          tags$head(
                                                              tags$style(HTML("
                                                                   #renderprint4 {
                                                                   color:black;
                                                                   background:white;
                                                                   font-family: 'Times New Roman', Times, serif;
                                                                   font-size: 18px;
                                                                   text-align:left;
                                                                   
                                                                   }"))), htmlOutput("renderprint4"),br(),br(),br(),br(),br(),br()))
                                            )),
                                          tabPanel("Graphs/Plots",icon=icon("chart-line",class=NULL,lib="font-awesome"),
                                                   sidebarLayout(position="left",
                                                                 sidebarPanel(width=3,radioButtons("rd",
                                                                               label="Select distribution plot:",
                                                                               choices=c("Total Cases","Total vaccination doses","Vaccine distribution by company"),
                                                                               selected="Total Cases")),
                                                                 mainPanel(htmlOutput("dist_plot"),br(),htmlOutput("instructions1"),br(),
                                                                           plotlyOutput("distribution_plot",width="600px", height="400px")))),
                                  
                                          tabPanel("Maps", icon=icon("globe-americas",class=NULL,lib="font-awesome"),
                                                   tabsetPanel(
                                                       tabPanel("Total Cases",icon=icon("head-side-cough",class=NULL,lib="font-awesome"),
                                                                br(),
                                                                fluidRow(
                                                                    tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
                                                                    column(6,
                                                                           selectInput(inputId="dat",
                                                                                       label="Type of Data:",
                                                                                       choices=c("Total COVID cases","Total COVID cases per 100k"),
                                                                                       selected="Total COVID cases",
                                                                                       multiple=FALSE))),br(),
                                                                column(12,
                                                                       mainPanel(htmlOutput("map_plot"),br(),
                                                                                 htmlOutput("instructions2"),br(),
                                                                                 htmlOutput("meaning"),br(),
                                                                                 leafletOutput("cases_map",width="1000px"),br(),br(),br(),br(),br(),br()))),
                                                       
                                            tabPanel("Vaccination",icon=icon("syringe",class=NULL,lib="font-awesome"),br(),
                                                        fluidRow(
                                                            tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
                                                            column(6,
                                                                   selectInput(inputId="data",
                                                                               label="Type of Data:",
                                                                               choices=c("Total vaccinations done","% population vaccinated"),
                                                                               selected="Total COVID cases",
                                                                               multiple=FALSE)), br(),
                                                           column(12,
                                                                   mainPanel(
                                                                             leafletOutput("vax_map",width="1000px"),br(),br(),br(),br(),br(),br()) )))))
                                           
                                  
)))
