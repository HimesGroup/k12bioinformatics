library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(devtools)
library(forcats)
library(sf)
library(broom)
library(rgeos)
library(rgdal)
library(Jmisc)
library(RCurl)
library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
library(sf)
library(tidyr)
library(readxl)
library(RColorBrewer)
library(scales)
library(highcharter) 


## Load the updated data file from GITHUB 

#read in the file
x <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases.csv")
y <- read.csv(text = x)

#new cases per day
y <- y %>% dplyr::mutate(new_cases_worldwide= World - dplyr::lag(World)) 
y <- y %>% dplyr::mutate(moving_avg_7= zoo::rollmean(new_cases_worldwide, k = 7, fill = NA))
y$moving_avg_7 <- round(y$moving_avg_7 ,1)

#filter by date
y_filtered <- y[(y$date> "2020-02-29" & y$date < "2021-01-01"),]
y_filtered_T <- reshape2::melt(y_filtered, id.vars=c("date","World"))
y_filtered$date <- as.POSIXct(y_filtered$date,tz=Sys.timezone())

#Change y-axis format i.e. 1000 will be 1K
so_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"),
    x < 1e12 ~ paste0(as.character(x/1e9), "B"),
    TRUE ~ as.character(x)
  )
}

#Finalize labels
labels <- so_formatter(seq(100000,1500000,100000))

#plot1 : Covid-19 Global New Cases
gg_point <- ggplot(data=y_filtered,aes(x=date, y=new_cases_worldwide,group=1,
                                       text=paste("<b>Date:</b>",format(date,"%B %d, %Y"),"<br><b> New Cases:</b>",new_cases_worldwide,"<br><b> 7-day moving average:</b>",moving_avg_7)))+ 
  geom_bar(stat="identity",fill="#deebf7") + geom_line(data=y_filtered, aes(x=date, y=new_cases_worldwide), colour = "#2b8cbe") +
  labs(x= "Date", y= "Total New Daily Cases") + ggtitle("Total new cases from March 2020 to Dec 2020")+
  scale_x_datetime(labels = date_format("%b %Y"),date_breaks  ="1 month") + 
  geom_hline(yintercept=c(0,2813),linetype="dotted") + 
  theme(axis.text.x=element_text(angle=45, hjust=1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),plot.title = element_text(size = 12))+
  scale_y_continuous(breaks=seq(100000,1500000,100000),labels=labels)

#Add interactive elements
plot1<-ggplotly(gg_point, tooltip = "text")%>%plotly::layout(xaxis=list(spikethickness=2, spikecolor="#2b8cbe"))%>%config(displayModeBar = FALSE)%>%onRender("
function(el, x) {
  Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')
}
")

plot1

#Chloropleth maps
shapename <- read_sf('shapefile_99bfd9e7/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp') 

#filter dates
dates_filtered <- y_filtered_T %>% dplyr::filter(date=='2020-12-31')
dates_filtered$variable <- as.character(dates_filtered$variable)
dates_filtered$variable <- sapply(dates_filtered$variable, gsub, pattern = ".", replacement = " ", fixed = TRUE)

#World Population data
world_pop <- read.csv("world_pop.csv")
world_pop <- world_pop %>% 
  dplyr::select(Country.Name,X2015..YR2015.) %>% 
  dplyr::rename("Country"="Country.Name","pop_2015"= "X2015..YR2015.") %>% drop_na()
world_pop$pop_2015 <- as.numeric(world_pop$pop_2015)

#Merge data with shapefile
merged_df <- merge(x =shapename , y = dates_filtered, by.x="CNTRY_NAME", by.y="variable",all=TRUE)

#World co-ordinates
world_lat_long<-read_excel("world_country_long_lat.xlsx")

#Merge data with world co-ordinates
merged_df <- merge(x = merged_df, y = world_lat_long, by.x= "CNTRY_NAME", by.y="Country",all=TRUE)
world_pop$Country<- sapply(world_pop$Country, gsub, pattern = "Russian Federation", replacement = "Russia", fixed = TRUE)

merged_df <- merge(x=merged_df,y=world_pop,by.x="CNTRY_NAME",by.y="Country",all=TRUE)
merged_df$cases_per_capita<-round(merged_df$pop_2015/merged_df$value,0)
merged_df<-merged_df %>% drop_na(pop_2015)


#total cases map
#Set top limit of legend
max_val = max(na.omit(merged_df$value))
max_no = round_any(max_val, 10^(nchar(max_val)-1), f = ceiling)

#Set palette
mybins1 <- rev(c(0,50000,500000,2000000,5000000,7000000,9000000,15000000,20000000,max_no))
mypalette1 <- colorBin(palette="YlOrRd", domain=merged_df$value, na.color="transparent", bins=mybins1)

#Add commas in big numbers for better readability
merged_df$fvalue <- formatC(merged_df$value, format="d", big.mark=",") 
merged_df$fpop_2015 <- formatC(merged_df$pop_2015, format="d", big.mark=",")

#Map1 : COVID-19 Reported Cases
total_cases_map <-leaflet(merged_df,width = "100%") %>% 
  addTiles() %>% 
  setView( lat=10, lng=0 , zoom=2)%>% 
  addPolygons(data=merged_df,fillColor = ~mypalette1(merged_df$value), weight = 1,opacity = 1,color = "white",
              dashArray = "2",fillOpacity = 0.7,highlight = highlightOptions(weight = 2,color = "#666",
                                                                             dashArray = "",fillOpacity = 0.7,bringToFront = TRUE),label = sprintf("<strong>%s</strong><br/>Total Cases: %s</br> Total Population:%s</br>",merged_df$CNTRY_NAME,merged_df$fvalue,merged_df$fpop_2015) %>% 
                lapply(htmltools::HTML),labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))%>%
  addLegend(pal = mypalette1, values = merged_df$value, opacity = 0.5, title = "<small> COVID-19 Reported Cases (2020) </small>",position = "bottomleft")


##Cases per 100k
max_no = round(max(na.omit(merged_df$cases_per_100k))) 

#color palette
mybins2 <- rev(c(0,1,5,10,20,30,40,75,100,200,500))
mypalette2<- colorBin( palette="RdPu", domain=merged_df$cases_per_100k, na.color="transparent", bins=mybins2)

#Add cases per 100k
merged_df$cases_per_100k<-round(merged_df$value/100000,2)

#Map2 : COVID-19 Reported Cases Per 100K
total_cases_100k_map <- leaflet(merged_df,width = "100%") %>% 
  addTiles() %>% 
  setView( lat=10, lng=0 , zoom=2) %>% 
  addPolygons(data=merged_df,fillColor = ~mypalette2(merged_df$cases_per_100k), weight = 1,opacity = 1,color = "white",
              dashArray = "2",fillOpacity = 0.7,highlight = highlightOptions(weight = 2,color = "#666",
                                                                             dashArray = "",fillOpacity = 0.7,bringToFront = TRUE),label = sprintf("<strong>%s</strong><br/>Total Cases per 100k: %s</br> Total Population:%s</br>", merged_df$CNTRY_NAME,merged_df$cases_per_100k,merged_df$fpop_2015) %>% 
                lapply(htmltools::HTML),labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
  addLegend(pal = mypalette2, values = merged_df$cases_per_100k, opacity = 1.0, title = "<small> COVID-19 Reported Cases </br> Per 100K People (2020) </small>",position = "bottomleft")


##Vaccine doses distribution
world_sp <- readOGR(dsn=paste0(getwd(),"/shapefile_World_Countries/") ,layer="World_Countries__Generalized_",verbose=FALSE)

#Read vaccine data from GITHUB
vaccine_data<- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccine_csv <- read.csv(text = vaccine_data)

vaccine_csv_world<-vaccine_csv%>%dplyr::filter(location=="World")

vaccine_csv_world$date <- as.POSIXct(vaccine_csv_world$date,tz=Sys.timezone())


vaccine_csv_world_filtered <- vaccine_csv %>% dplyr::filter(date=="2021-03-31")
vaccine_csv_world_filtered <- merge(vaccine_csv_world_filtered,world_pop,by.x="location",by.y="Country",all=TRUE)
vaccine_csv_world_filtered$location <- sapply(vaccine_csv_world_filtered$location, gsub, pattern = "Russia", 
                                              replacement = "Russian Federation", fixed = TRUE)
world_spdf <- merge(world_sp,vaccine_csv_world_filtered,by.x="COUNTRY",by.y="location",all=TRUE)

#Set top limit of legend
max_val = max(na.omit(world_spdf$people_fully_vaccinated))
max_no = round_any(max_val, 10^(nchar(max_val)-1), f = ceiling)

#Set color palette
mybins_x <- rev(c(0,10000,100000,500000,1000000,5000000,10000000,20000000,50000000,max_no))
mypalette_x <- colorBin( palette="YlGn", domain=world_spdf$people_fully_vaccinated, na.color="transparent", bins=mybins_x)

#Add commas in big numbers for better readability
world_spdf$fpeople_fully_vaccinated <- formatC(world_spdf$people_fully_vaccinated, format="d", big.mark=",") 
world_spdf$fpop_2015 <- formatC(world_spdf$pop_2015, format="d", big.mark=",")

##Map3 : Total People Vaccinated
vaccine_map_total<-leaflet(world_spdf,width = "100%") %>%
  addTiles() %>% 
  setView( lat=10, lng=0 , zoom=2)%>%
  addPolygons(data=world_spdf,fillColor = ~mypalette_x(world_spdf$people_fully_vaccinated), weight = 1,opacity = 1,color = "white",
              dashArray = "2",fillOpacity = 0.7,highlight = highlightOptions(weight = 2,color = "#666",
                                                                             dashArray = "",fillOpacity = 0.7,bringToFront = TRUE),
              label = sprintf("<strong>%s</strong><br/>Total People Vaccinated: %s</br> Total Population:%s</br>",
                              world_spdf$COUNTRY,world_spdf$fpeople_fully_vaccinated,world_spdf$fpop_2015)%>% 
                lapply(htmltools::HTML),labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "15px",direction = "auto")) %>%
  addLegend(pal = mypalette_x, values = world_spdf$people_fully_vaccinated, opacity = 1.0, 
            title = "<small> No. of People Fully </br> Vaccinated till March 2021 </small>",position = "bottomleft")


#Plot2 : Vaccine doses
labels2<- so_formatter(seq(10000000,6000000000,100000000))

options(scipen=999)
gg_point2 <- ggplot(data=vaccine_csv_world,aes(x=date, y=total_vaccinations,
                                                        group=1,
                                                        text=paste("<b>Date:</b>",format(date,"%B %d, %Y"),"<br><b> Total vaccinations:</b>",total_vaccinations)))+ 
  geom_bar(stat="identity",fill="#add8e6")+geom_line(data=vaccine_csv_world, aes(x=date, y=total_vaccinations), colour = "#008080")+
  labs(x= "Date", y= "Total Cumulative Vaccinations")+
  ggtitle("Total Vaccinations from January to March 2021 ")+
  scale_x_datetime(labels = date_format("%b %Y"),date_breaks  ="1 month",)+ 
  geom_hline(yintercept=c(0,10125780),linetype="dotted")+ 
  scale_y_continuous(breaks=seq(10000000,6000000000,100000000), labels=labels2)+ 
  theme(axis.ticks.length =unit(1.5,"mm"),axis.text.x=element_text(angle=45, hjust=1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),plot.title = element_text(size = 12))


plot2_1<-ggplotly(gg_point2, tooltip = "text")%>%plotly::layout(xaxis=list(spikethickness=2, spikecolor="#2b8cbe"))%>%config(displayModeBar = FALSE)%>%onRender("
function(el, x) {
  Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')
}
")

plot2_1

#Set max
max_no = round(max(na.omit(world_spdf$people_fully_vaccinated_per_hundred))) 

#Colorpalette
mybins<- rev(c(0,1,5,10,15,20,25,30,50, max_no))
mypalette <- colorBin(palette="magma", domain=world_spdf$people_fully_vaccinated_per_hundred, na.color="transparent", bins=mybins, reverse = T)

#Readability
world_spdf$ftotal_vaccinations <- formatC(world_spdf$total_vaccinations, format="d", big.mark=",")

mytext <- paste(
  "Country: ", world_spdf$COUNTRY,"<br/>", 
  "Total Vaccinations till March 2021: ",world_spdf$total_vaccinations, "<br/>", 
  "Vaccination %:", round(world_spdf$people_fully_vaccinated_per_hundred, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

#Map4: Percent Population Vaccined
vaccine_map_percent <- leaflet(world_spdf,width = "100%") %>%
  addTiles() %>% 
  setView( lat=10, lng=0 , zoom=2)%>%
  addPolygons(data=world_spdf,fillColor = ~mypalette(world_spdf$people_fully_vaccinated_per_hundred), weight = 1,opacity = 1,
              color = "white",dashArray = "2",fillOpacity = 0.7,highlight = highlightOptions(weight = 2,color = "#666",
                                                                                             dashArray = "",fillOpacity = 0.7,bringToFront = TRUE),label = sprintf("<strong>%s</strong><br/>Total Vaccinations: %s</br>%s Percent Fully Vaccinated </br>",
                                                                                                                                                                   world_spdf$COUNTRY,world_spdf$ftotal_vaccinations,world_spdf$people_fully_vaccinated_per_hundred) %>% 
                lapply(htmltools::HTML),labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "15px",direction = "auto")) %>%
  addLegend(pal = mypalette, values = world_spdf$total_vaccinations, opacity = 1.0, 
            title = "<small> Percent Population Fully </br> Vaccinated till March 2021</small>",position = "bottomleft")


##Get vaccine admin by type data from GITHUB
vaccine_comp <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv")
vaccine_company <- read.csv(text = vaccine_comp)

vaccine_group <- vaccine_company %>% dplyr::group_by(location,vaccine) %>% dplyr::summarize(max_doses=max(total_vaccinations))
vaccine_group <- vaccine_group%>% dplyr::group_by(location) %>% dplyr::mutate(percent = round((max_doses/sum(max_doses))*100,2))

#Plot3 : Vaccine type
vaccine_company_graph <- ggplot(vaccine_group, aes(x = location, y = percent, fill = vaccine, 
                                                   label = percent,text=paste("<b>Country:</b>",location,"<br><b> Company:</b>",
                                                                              vaccine,"<br><b> Percent:</b>",percent))) +geom_bar(stat = "identity",position=position_dodge()) +
  labs(x="Country",y="Percentage of Total Doses Administered")+ggtitle("Percentage of Total Doses by Vaccine Manufacturer Per Country")+
  theme_bw() + 
  theme(axis.text.x=element_text(angle=45, hjust=1), plot.title = element_text(size=10),legend.title = element_blank()) + 
  scale_fill_brewer(name=as.vector(vaccine_group$vaccine), palette = "Dark2")

#Interactive element
plot2<-ggplotly(vaccine_company_graph, tooltip = "text")%>%config(displayModeBar = FALSE)%>%onRender("
function(el, x) {
  Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')
}
")
plot2

#Format
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
hc <- vaccine_group %>% 
  hchart('column', hcaes(x = 'location', y = 'percent', group = 'vaccine'))%>%hc_title(
    text = "Percentage of vaccine doses  administered in each countries per manufacturer",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  )

