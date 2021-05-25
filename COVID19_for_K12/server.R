#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distribution_plot<-renderPlotly({if(input$rd=="Total Cases"){(ggplotly(plot1))} else if(input$rd=="Total vaccination doses")(ggplotly(plot2_1)) else(ggplotly(plot2))})
    
    output$cases_map<-renderLeaflet({if(input$dat=="Total COVID cases"){(total_cases_map)} else{(total_cases_100k_map)}})
    
    output$vax_map<-renderLeaflet({if(input$data=="Total vaccinations done"){(vaccine_map_total)} else{(vaccine_map_percent)}})
    
    output$renderprint <- renderPrint({
        HTML(paste0("<b>What is the COVID-19 pandemic?</b><br>
    The COVID-19 pandemic, also known as the coronavirus pandemic, 
    is a global coronavirus disease 2019 (COVID-19) pandemic caused by coronavirus 2 that causes serious acute respiratory syndrome (SARS-CoV-2).
    On 30 January 2020, the World Health Organization declared COVID-19 a Public Health Emergency of International Concern, and on 11 March 2020, it was declared a pandemic."
        ))})
    
    output$renderprint2<-renderPrint({HTML(paste0("<b>Prevention</b><br>
    Preventive measures to reduce the chances of infection include staying at home, wearing a mask in public, avoiding crowded places, keeping distance from others, ventilating indoor spaces, managing potential exposure durations, washing hands with soap and water often and for at least twenty seconds, practising good respiratory hygiene, 
    and avoiding touching the eyes, nose, or mouth with unwashed hands.")
)})
    
    output$renderprint3<-renderPrint({HTML(paste0("<b>Notable Symptoms:</b><br>
    <ul><li>Cough</li><li>Nasal congestion and runny nose</li>
    <li>Sore throat</li>
    <li>Fever</li>
    <li>Loss of smell and taste</li>
    <li>Breathing difficulties</li>
    <li>Diarrhea</li>
    <li>Muscle pain</li></ul> <br>There is a lag between when a person becomes infected and when the first signs occur, as is typical with infections. COVID-19 has a four to five day median delay. Symptoms appear in the majority of symptomatic people two to seven days after exposure and nearly everybody has at least one symptom within 12 days."))})
    
    output$renderprint4<-renderPrint({HTML(paste0("<b> The Application</b><br>
    
    This is an R shiny application visualizing two COVID-19 metrics:<br>
    1. Total Cases in the world <br>
    2. Vaccination trends in the world<br> 
                To study these trends, data relating to total COVID 19 cases has been obtained from <b>John Hopkins University </b> while data 
                relating to vaccinations has been obtained from <b>'Our World in Data'</b>. Both datasets were available on Github
                which is an open source platform. <br>
                COVID cases data spans from March to December 2020 while Vaccination data is from January to March 2021."))})
    
    output$dist_plot<-renderPrint({HTML(paste0("The <b> distribution plot </b> is suitable for comparing range and distribution for groups of numerical data. Data is plotted as value points along an axis."))})
    
    output$instructions1<-renderPrint({HTML(paste0("<b>Instructions:</b><br><ol><li> Take the mouse cursor over the plots</li><li>Hover over the plots to interact with them and analyze the different metrics</li></ol>"))})
    
    output$instructions2<-renderPrint({HTML(paste0("<b>Instructions:</b><br><ol><li> Take the mouse cursor over map to see different metrics associated with different countries</li><li>Zoom in and zoom out to see names of the countries</li></ol>"))})
    
    
    output$map_plot<-renderPrint({HTML(paste0("A <b> chloropleth map </b> is a map that uses differences in shading, coloring, or the placing of symbols within predefined areas to indicate the average values of a property or quantity in those areas."))})
    output$meaning<-renderPrint({if(input$dat=="Total COVID cases per 100k"){HTML(paste0("A <b> rate </b> provides a meaningful way to compare total cases
between population groups of different sizes instead of raw numbers. Hence to control for population of different countries and get a better context for comparison, 
                                its important to compare <b>total cases per 100k of population </b>."))} else{(HTML(paste0(" ")))}})

})
