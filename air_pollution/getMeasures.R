library(pargasite)
library(dplyr)
library(data.table)


#Get PM 2.5 measures for tab "EPA Measures in USA"
k12 <- read.table("k12_sites.txt",header=TRUE,sep="/t")
long <- k12$Longitude
lat <- k12$Latitude
pm_list <- list()

for (i in seq(1,nrow(k12))){
  pm <- getMonthPollutionEstimate(long[i], lat[i], pollutant = "PM2.5", monthyear="09-2017")
  pm_list[[i]] <- pm
}

k12$PM <- unlist(pm_list)
k12$Location <- paste0(k12$City,",",k12$State)
write.csv(k12,"k12_sites.csv",row.names = FALSE)



#Get PM2.5 and CO measures for all cities from 2007-2017 for tab "Seasonality of measures"

k12 <- read.csv("../databases/k12_sites.csv")
dates <- seq(1,12)
months <- c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sept","Oct","Nov","Dec")
years <- seq(2007,2017)
fdates <- unlist(lapply(years, function(x) paste0(dates,"-",x)))
iter = 1
ph_list <- list()
ch_list <- list()

for (k in k12$State){
  pk12 <- k12 %>% dplyr::filter(State == k) %>% dplyr::select(Longitude,Latitude)
  pm_list <- list()
  co_list <- list()
  y_list <- list()
  count = 1

  for (i in seq(1,length(fdates))){
    pm <- getMonthPollutionEstimate(pk12$Longitude, pk12$Latitude, pollutant="PM2.5", monthyear = fdates[i])
    co <- getMonthPollutionEstimate(pk12$Longitude, pk12$Latitude, pollutant="CO", monthyear = fdates[i])
    pm_list[[i]] <- pm
    co_list[[i]] <- co
    if (i %% 12 == 0){
      y_list[[i]] <- years[count]
      count = count + 1
    } else {
      y_list[[i]] <- years[count]
    }
  }

  ph_df <- data.frame("Dates" = fdates, "PM2.5" = unlist(pm_list),"Year" = unlist(y_list),"Month"= months,"State"=k)
  ph_list[[iter]] <- ph_df
  co_df <- data.frame("Dates" = fdates, "CO" = unlist(co_list),"Year" = unlist(y_list),"Month"= months,"State"=k)
  ch_list[[iter]] <- co_df
  iter = iter + 1
}

ph_df <- rbindlist(ph_list, fill = TRUE)
co_df <- rbindlist(ch_list, fill = TRUE)

write.csv(ph_df,"../databases/all_k12_sites_PM.csv", row.names = FALSE)
write.csv(co_df,"../databases/all_k12_sites_CO.csv", row.names = FALSE)
