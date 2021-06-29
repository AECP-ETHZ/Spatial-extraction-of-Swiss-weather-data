# =========================================================================================
# -----------------------------------------------------------------------------------------
#
# Swiss weather grid data extractor: for exact locations in Swiss coordinates
#
# -----------------------------------------------------------------------------------------
# =========================================================================================

# Last Update: 15.04.2021
# Authors: Carol Tanner, Janic Bucheli, Tobias Dalhaus, Vivienne Oggier & Finn Timcke
# Contact: jbucheli@ethz.ch

# Data access point with ETH affiliation: https://hyd.ifu.ethz.ch/research-data-models/meteoswiss.html
# Data access point for others: MeteoSwiss contact center

# More information: https://hyd.ifu.ethz.ch/research-data-models/meteoswiss.html

# Note:if you use BFS municipality centers, use the other script.

# Requirement: 
# gridded datasets from MeteoSwiss (from AECP members, these are in the shares)
# coordinates of location (ideally in GPS format)

# =========================================================================================
# install and load required packages
# =========================================================================================

# Install and load packages (not all are necessary, but all can be useful for further applications)
packages <- c("sp","raster","rasterVis","maptools","mapview","maps","ncdf4","zoo","lubridate","rgdal","rgeos","plyr","dplyr","reshape2","biogeo","geosphere")

package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
)

# =========================================================================================
# Abbreviations and explanation
# =========================================================================================

## variables (daily resolution)

# Rhires = precipitation in [mm]
# Tmax = maximum air temperature in [degree Celsius]
# Tmin = minimum air temperature in [degree Celsius]
# Tabs = mean air temperature in [degree Celsius]
# Srel = sunshine in [%] of daytime

## parameters to be defined below (that first has to be defined by author)

# start     = first year of required data in yyyy format (e.g. 2002)
# end       = last year of required data in yyyy format (e.g. 2005)

# prepare extraction:
# all weather files must be in the following format: .nc (Network Common Data Form)
# all needed weather files (in .nc format) must be in a folder named "all_weatherdata"
# in all_weatherdata, gridded datasets for a specific variable must be in a subfolder (e.g. RhiresD/ for precipitation or TabsD/ for mean air temperature). D represents daily.

# steps to do:
# - type in your information about the time serie below (start and end) 
# - type in your information about the different variables you are interested in (format for ex.: RhiresD, TabsD)
# - set working directory as indicated below
# - load and prepare the coordinates
# - run the rest of the script

# =========================================================================================
# Definition of variables
# =========================================================================================

# set start and end date of extracted time series in format YYYY e.g. 1990
# format YYYY e.g. 1990
start <- 1999 
end <- 2019 

# type in variables of interest
variables_of_interest <- c("RhiresD","TmaxD","TminD") # use the following format for daily values: c("RhiresD", "TmaxD", "TminD", "TabsD", "SrelD")

# set working directory to created folder "all_weatherdata".
# if you work at AECP, use this directory
setwd("Z:/Papers and Data/Weather Data Switzerland/all_weatherdata")

# Define the output directory
# Three outputs will be saved in the output directory
# i) list_weather_data.RData  : a list with structure [[weather variable]][BFS,date]
# ii) all_weather_data.RData  : a data.frame with all weather variables (long form)
# iii) all_weather_data.csv   : a data.frame with all weather variables (long form)
output.dir <- "H:/Working Papers/Wheat Quality Paper"

# =========================================================================================
# Prepare coordinates
# =========================================================================================

# This coordinate preparation might be different for each research
# Load a data.frame with your GPS coordinates.
# Your coordinates will be transformed to the Swiss coordinate system automatically below.
locations_raw <- read.csv("H:/Working Papers/Wheat Quality Paper/Sites_coordinates_mv.csv", sep=";")

# Here coordinates are given in e.g. 46°39'27''N, 6°38'20''E
# We transform these format (degree,minutes,seconds) into the decimal format (e.g. 46.66N, 6.63E)
# If your coordinates are already in the decimal form, you don't have to execute the command below (e.g. skip l. 106/107)
# Note that the data.frame needs the following columns: i)Latitude.deg, ii)Latitude.min, iii)Latitude.sec, iv) Longitude.des, v)Longitude.min, vi)Longitude.sec 

locations_raw$lat <- dms2dd(dd=locations_raw$Latitude.deg, mm=locations_raw$Latitude.min, ss=locations_raw$Latitude.sec, ns=c("N"))
locations_raw$long <- dms2dd(dd=locations_raw$Longitude.deg, mm=locations_raw$Longitude.min, ss=locations_raw$Longitude.sec, ns=c("E"))

# From here on, you need a data.frame with a column "lat" for the latitude and one for "long" for the longitude of your locations
# We transform these into spatial points (make sure your original coordinates are in GPS format)

coordinates_locations <- locations_raw[,c("long","lat")]
coordinates(coordinates_locations) <- c("long" , "lat")
proj4string(coordinates_locations)  <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Optionally, you can check your coordinates on a map (here municipalities from 2021): 

Gemeinden <- readOGR(dsn = "GIS/BOUNDARIES_2021/DATEN/swissBOUNDARIES3D/SHAPEFILE_LV03_LN02/municipality", layer = "swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET") 
coordinates_locations_plot <- spTransform(coordinates_locations,CRSobj = crs(Gemeinden)) 

plot(Gemeinden)
plot(coordinates_locations_plot, col = "red", add = T)

# That is all. From here on, you can just run the script without making anymore changes.
# If you have a different coordinate reference system, just make sure that these are spatial points.

# =========================================================================================
# Extract weather data
# =========================================================================================
  
  # Each element of the list contains the weather data
  list_weather_data <- list() 
  
  for (i in 1:length(variables_of_interest)) {
    
    # Get the weather data (which ones to download)
    if (variables_of_interest[i] == "RhiresD") {
    
      dir <- paste(variables_of_interest[i],"/", sep="")
      listoffiles <- list.files(path=dir ,pattern=".nc")
      
      years.data <- vector(length=length(listoffiles)) 
      for (f in 1:length(listoffiles)){
        years.data[f]<-substr(listoffiles[f],25,28)
      }
      
      files.to.import <- listoffiles[which(years.data==start):which(years.data==end)]
      
      # Download the datasets
      rawdata<- list()
      
      # coordinate reference system is for LV03
      for (j in 1:length(files.to.import)){
        rawdata[[j]]<-brick(paste(variables_of_interest[i],"/",files.to.import[j], sep=""))
        # Add coordinate reference system (LV03)
        crs(rawdata[[j]])<- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs" 
      }
      
      # Bring municipality center into the reference system of weather data
      locations <- spTransform(coordinates_locations,CRSobj = crs(rawdata[[1]])) 
      
      # Get the weather data
      
      weatherdata <- extract(rawdata[[1]], locations)
      if(length(files.to.import) != 1){
        for (k in 2:length(files.to.import)){
          weatherdata.year <- extract(rawdata[[k]], locations)
          weatherdata <- cbind(weatherdata, weatherdata.year)
          rm(weatherdata.year)
        }
      } else {}
      
      years <- seq(start, end)
      days <- as.character(seq(as.Date(paste(years[1],1,1, sep = "-")), as.Date(paste(years[length(years)],12,31, sep = "-")), "day"))
      colnames(weatherdata) <- days
      rm(rawdata, dir, listoffiles, years.data, files.to.import)
      
      
    } else if (variables_of_interest[i] == "TmaxD"){
      dir <- paste(variables_of_interest[i],"/", sep="")
      listoffiles <- list.files(path=dir,pattern=".nc")
      
      years.data <- vector(length=length(listoffiles)) 
      for (f in 1:length(listoffiles)){
        years.data[f]<-substr(listoffiles[f],23,26)
      }
      
      files.to.import <- listoffiles[which(years.data==start):which(years.data==end)]
      
      # Download the datasets
      rawdata<- list()
      
      # coordinate reference system is for LV03
      for (j in 1:length(files.to.import)){
        rawdata[[j]]<-brick(paste(variables_of_interest[i],"/",files.to.import[j], sep=""))
        # Add coordinate reference system (LV03)
        crs(rawdata[[j]])<- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs" 
      }
      
      # Bring municipality center into the reference system of weather data
      locations <- spTransform(coordinates_locations,CRSobj = crs(rawdata[[1]])) 
      
      # Get the weather data
      weatherdata <- extract(rawdata[[1]], locations)
      if(length(files.to.import) != 1){
        for (k in 2:length(files.to.import)){
          weatherdata.year <- extract(rawdata[[k]], locations)
          weatherdata <- cbind(weatherdata, weatherdata.year)
          rm(weatherdata.year)
        }
      } else {}
      
      years <- seq(start, end)
      days <- as.character(seq(as.Date(paste(years[1],1,1, sep = "-")), as.Date(paste(years[length(years)],12,31, sep = "-")), "day"))
      colnames(weatherdata) <- days
      rm(rawdata, dir, listoffiles, years.data, files.to.import)
      
      
    } else if (variables_of_interest[i] == "TminD") {
      dir <- paste(variables_of_interest[i],"/", sep="")
      listoffiles <- list.files(path=dir,pattern=".nc")
      
      years.data <- vector(length=length(listoffiles)) 
      for (f in 1:length(listoffiles)){
        years.data[f]<-substr(listoffiles[f],23,26)
      }
      
      files.to.import <- listoffiles[which(years.data==start):which(years.data==end)]
      
      # Download the datasets
      rawdata<- list()
      
      # coordinate reference system is for LV03
      for (j in 1:length(files.to.import)){
        rawdata[[j]]<-brick(paste(variables_of_interest[i],"/",files.to.import[j], sep=""))
        # Add coordinate reference system (LV03)
        crs(rawdata[[j]])<- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs" 
      }
      
      # Bring municipality center into the reference system of weather data
      locations <- spTransform(coordinates_locations,CRSobj = crs(rawdata[[1]])) 
      
      # Get the weather data
      weatherdata <- extract(rawdata[[1]], locations)
      if(length(files.to.import) != 1){
        for (k in 2:length(files.to.import)){
          weatherdata.year <- extract(rawdata[[k]], locations)
          weatherdata <- cbind(weatherdata, weatherdata.year)
          rm(weatherdata.year)
        }
      } else {}
      
      years <- seq(start, end)
      days <- as.character(seq(as.Date(paste(years[1],1,1, sep = "-")), as.Date(paste(years[length(years)],12,31, sep = "-")), "day"))
      colnames(weatherdata) <- days
      rm(rawdata, dir, listoffiles, years.data, files.to.import)
      
    } else if (variables_of_interest[i] == "TabsD") {
      dir <- paste(variables_of_interest[i],"/", sep="")
      listoffiles <- list.files(path=dir,pattern=".nc")
      
      years.data <- vector(length=length(listoffiles)) 
      for (f in 1:length(listoffiles)){
        years.data[f]<-substr(listoffiles[f],23,26)
      }
      
      files.to.import <- listoffiles[which(years.data==start):which(years.data==end)]
      
      # Download the datasets
      rawdata<- list()
      
      # coordinate reference system is for LV03
      for (j in 1:length(files.to.import)){
        rawdata[[j]]<-brick(paste(variables_of_interest[i],"/",files.to.import[j], sep=""))
        # Add coordinate reference system (LV03)
        crs(rawdata[[j]])<- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs" 
      }
      
      # Bring municipality center into the reference system of weather data
      locations <- spTransform(coordinates_locations,CRSobj = crs(rawdata[[1]])) 
      
      # Get the weather data
      weatherdata <- extract(rawdata[[1]], locations)
      if(length(files.to.import) != 1){
        for (k in 2:length(files.to.import)){
          weatherdata.year <- extract(rawdata[[k]], locations)
          weatherdata <- cbind(weatherdata, weatherdata.year)
          rm(weatherdata.year)
        }
      } else {}
      
      years <- seq(start, end)
      days <- as.character(seq(as.Date(paste(years[1],1,1, sep = "-")), as.Date(paste(years[length(years)],12,31, sep = "-")), "day"))
      colnames(weatherdata) <- days
      rm(rawdata, dir, listoffiles, years.data, files.to.import)
      
      
        # else it must be SrelD
    } else {
      dir <- paste(variables_of_interest[i],"/", sep="")
      listoffiles <- list.files(path=dir,pattern=".nc")
    
    years.data <- vector(length=length(listoffiles)) 
    for (f in 1:length(listoffiles)){
      years.data[f]<-substr(listoffiles[f],23,26)
    }
    
    files.to.import <- listoffiles[which(years.data==start):which(years.data==end)]
    
    # Download the datasets
    rawdata<- list()
    
    # coordinate reference system is for LV03
    for (j in 1:length(files.to.import)){
      rawdata[[j]]<-brick(paste(variables_of_interest[i],"/",files.to.import[j], sep=""))
      # Add coordinate reference system (LV03)
      crs(rawdata[[j]])<- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs" 
    }
    
    # Bring municipality center into the reference system of weather data
    locations <- spTransform(coordinates_locations,CRSobj = crs(rawdata[[1]])) 
    
    # Get the weather data
    weatherdata <- extract(rawdata[[1]], locations)
    if(length(files.to.import) != 1){
      for (k in 2:length(files.to.import)){
        weatherdata.year <- extract(rawdata[[k]], locations)
        weatherdata <- cbind(weatherdata, weatherdata.year)
        rm(weatherdata.year)
      }
    } else {}
    
    years <- seq(start, end)
    days <- as.character(seq(as.Date(paste(years[1],1,1, sep = "-")), as.Date(paste(years[length(years)],12,31, sep = "-")), "day"))
    colnames(weatherdata) <- days
    rm(rawdata, dir, listoffiles, years.data, files.to.import)
    }
    
    
    # Save data from specific weather variable in list
    row.names(weatherdata) <- locations_raw$Site
    list_weather_data[[i]] <- weatherdata
    rm(weatherdata)
    
    print(i / length(variables_of_interest))
  }
  
# =========================================================================================
# Save weather data in the output directory
# =========================================================================================  

# Transform each element of the list into long format (melt)
list_weather_data_melted <- lapply(list_weather_data, melt) 
 for (i in 1:length(variables_of_interest)){
    colnames(list_weather_data_melted[[i]]) <- c("location","date",variables_of_interest[i])
  }  
  
# Merge all weather variables into one frame
weather_data_melted <- list_weather_data_melted[[1]]
  if (length(variables_of_interest) !=1){
    for (i in 2:length(variables_of_interest)){
      weather_data_melted <- join(weather_data_melted,list_weather_data_melted[[i]], by=c("location","date"))
    }
  } else {}
  
# Save the weather data in the output directory  
save(list_weather_data, file=paste(output.dir,"/list_weather_data.RData",sep=""))
save(weather_data_melted, file=paste(output.dir,"/all_weather_data_melted.RData",sep=""))
write.csv(weather_data_melted, file=paste(output.dir,"/all_weather_data_melted.csv",sep=""))
  
print("Done, have fun with the weather data")
  
    