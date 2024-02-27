####Web-scrapping code to download Environemtn Canada Weather data
# This code is intended to download Ontario Weather data from climate.weather.gc.ca
## Author: Gillian Holloway. First developed April 2017
## This code was developed by Gillian Holloway, based on original web-scrapping code from Dr. Stephanie Melles & Adam Hussein (14-Dec-14)
## This code is developed to web-scrap and compile ECCC weather data, and format it for import into an ArcGIS geodatabase.
## This code was last revised April 2021 by Gillian Holloway

#########################   STEP1   #######################################
library(dplyr)
colClasses <- c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")

#########################   STEP2   #######################################
#ontario weather stations
stn.id<- read.csv(paste0("data/raw/Ontario_Weather_StationsFin.csv"), header=T, sep=",") ##This is a cleaned and revised file of weather stations with Manitoba and Quebec stations included to remove issues with 
## no weather data beyond the ON border. This file must to present in the working directory.
head(stn.id)
stns <- data.frame()
iter <- 1
ipAddress <- "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID="
numStations <- nrow(stn.id)
for (i in 2020){ #change this value as the need arises
  for (j in 1:numStations){
    outfile <- paste(stn.id[j,1],"_",stn.id[j,2],"_",i,".csv", sep="")
    outdir <- paste("weather/", sep="")
    destfile <- paste(outdir, outfile, sep="")
    fname <- paste(stn.id[j,2], "&Year=", i, "&Month=12&Day=1&timeframe=2&submit=Download+Data", sep="")
    download.file(paste(ipAddress, fname, sep=""), destfile)
    # pull out the metadata for all the files to build a station file that can be easily mapped.
    if(iter == 1){
      metadata <- read.csv(destfile, nrows=6, header=F)
      st_num<- as.numeric(as.character(metadata[6,2]))
      for (p in 1:6){
        stns[j,p] <- as.character(metadata[p,2])
      }
      rm(metadata)
    }
    #import the climate data and check to see if the number of NA's in the max temp field are equal to the number of rows in the data.  if so
    #the data is unlikely to have been collected for that year, so the data file can be removed.  if not, re-export the file without the header info 
    #at the top of the file.
    
  }
  iter <- 0
  names(stns) <- c("Station", "Province", "Latitude", "Longitude", "Elevation", "StationID")
  write.csv(stns, "StationData.csv", row.names = F)
}

#########################  STEP3   ##########################################################################
# This code can be modified to download station data for other provinces or for all provinces.
# Many csv files will lack data for some years as some weather stations do not operate all the time, these files will have a size of <55 kb and can be manually sorted and deleted after download.
# Dr. Stephanie Melles & Adam Hussein: 14-Dec-14

###MANUAL STEP - 
###dELETE all empty weather station files <55kb. Double check these before delete as the exact files size can change when empty, and can vary between downloads.

#########################   STEP4   ######################################################################
#Set the work directory (wd) for the files - 
biglist<-list.files(paste("weather", sep=""))
xxOut<- data.frame()
Out<- data.frame()

##In appending data files together, correct format each column so data goes into ArcGIS correctly
for(i in 1:length(biglist)){
  print(paste(i))
  table1<- read.csv(paste("weather/", biglist[i], sep=""), header=T, stringsAsFactors=F, na.strings="")
  Station_name<-  substr(biglist[i], 0, nchar(biglist[i])-9)
  print(paste(Station_name))
  table2<- data.frame(table1[,"Date.Time"], table1[,"Year"], table1[,"Month"], table1[,"Day"], signif((table1[,"Min.Temp...C."]),digits = 1), signif((table1[,"Mean.Temp...C."]),digits = 1), signif((table1[,"Max.Temp...C."]),digits = 1), 
                      signif((table1[,"Snow.on.Grnd..cm."]),digits = 0), signif((table1[,"Total.Snow..cm."]),digits = 1), signif((table1[,"Total.Precip..mm."]),digits = 1), 
                      signif((table1[,"Total.Rain..mm."]),digits = 1), as.factor(paste0((table1[,"Climate.ID"]))), table1[,"Station.Name"])
  rm(table1)
  names(table2)<- c("date_time", "Year", "Month", "Day","Min_temp", "Mean_temp", "Max_temp", "SnowGrd_cm", "TotalSnow_cm", "Total_Precip_mm", "Total_rain_mm", "climate_id", "StatName")
  xxOut<- rbind(xxOut, table2)  #rm(table2)
}
##Remove records with no data (NAs) for all climate variables - NA for min temp, max temp, mean , snow and rain
xxOut$ID <- seq.int(nrow(xxOut))
sub<-subset(xxOut, is.na(xxOut[,"Mean_temp"]) & is.na(xxOut[,"Total_Precip_mm"]) & is.na(xxOut[,"Total_rain_mm"])) ##subset record with no data for all climate variables
ID_NAs <- data.frame()
ID_NAs <-sub[,c("ID", "StatName")] 
b<-left_join(xxOut,ID_NAs, by="ID") ## join back list of records with complete NAs
Out<-subset(b, is.na(b[,"StatName.y"])) ##select only record not contanied in the list of NAs
Out$date_time <- as.Date(Out$date_time)

weather <- Out %>%
  filter(Month >= 9, Month <= 11) 
saveRDS(weather,"data/processed/ECCweather.rds")
