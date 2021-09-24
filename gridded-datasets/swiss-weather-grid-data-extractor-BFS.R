# =========================================================================================
# -----------------------------------------------------------------------------------------
#
# Swiss weather grid data extractor: for BFS number of municipalities (municipality center)
#
# -----------------------------------------------------------------------------------------
# =========================================================================================

# Last Update: 15.04.2021
# Authors: Carol Tanner, Janic Bucheli, Tobias Dalhaus, Vivienne Oggier & Finn Timcke
# Contact: jbucheli@ethz.ch

# Data access point with ETH affiliation: https://hyd.ifu.ethz.ch/research-data-models/meteoswiss.html
# Data access point for others: MeteoSwiss contact center

# More information: https://hyd.ifu.ethz.ch/research-data-models/meteoswiss.html

# Note:if you use exact locations (i.e. not municipalities), use the other script.

# Requirement: 
# gridded datasets from MeteoSwiss (from AECP members, these are in the shares)
# the BFS-number of municipalities of interest

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

# start     = year of beginning in XXXX format (e.g. 2002)
# end       = year of ending in XXXX format (e.g. 2005)

# prepare extraction:
# all weather files must be in the following format: .nc (Network Common Data Form)
# all needed weather files (in .nc format) in a folder named "all_weatherdata"
# in all_weatherdata, gridded datasets for a specific variable must be in a subfolder (e.g. RhiresD/ for precipitation or TabsD/ for mean air temperature). D represents daily.
# know the BFS-number of your municipalities of interest

# steps to do:
# - type in your information about the time serie (start and end) 
# - type in your information about the different variables you are interested in(format for ex.: c(RhiresD, TabsD))
# - set working directory as indicated below
# - load the files "municipalities_bfs.csv" and municipality shapefiles as indicated below
# - download all packages and load them
# - run the rest of the script (and make no changes unless corrections below)

# =========================================================================================
# Definition of variables
# =========================================================================================

# set start and end date of extracted time series in format YYYY e.g. 1990
start <- 2015 # format YYYY e.g. 1990
end <- 2019 # format YYYY e.g. 1990

# type in variables of interest
variables_of_interest <- c("RhiresD","TmaxD","TminD") # use the following format for daily values: c("RhiresD", "TmaxD", "TminD", "TabsD", "SrelD")

# set working directory to created folder "all_weatherdata".
# if you work at AECP, use this directory
setwd("Z:/Papers and Data/Weather Data Switzerland/all_weatherdata")

# give the path for the file "municipalities_bfs.csv". This file contains BFS numbers of all municipalities and shows mergers.
# if you work at AECP, us the directory below. It contains bfs numbers from 2021 and mergers from 1960 to 2021 
bfs.nummern <- read.csv("Z:/Papers and Data/Weather Data Switzerland/all_weatherdata/GIS/municipalities_bfs.csv", sep=";")

# make a subset of the bfs numbers (municipalities) that are of interest 
# maybe you want to do this a bit different than in this example (e.g. upload municipality names or new bfs numbers from your research)
# important: after this step, there must be a column labeled with "bfs_new" (latest bfs number) and "name_new" (latest name of municipality)
# If you use another input file than municipalities_bfs.csv, you might need to change the colnames. 
'IMPORTANTLY, THE SCRIPT REQUIRES THE BFS NUMBER FROM 2021!!! CHECK YOUR DATASET FOR MERGERS AND ADJUST ACCORDINGLY'
bfs.nummern <- bfs.nummern[which(bfs.nummern$name_new %in% c("Cham","Zug","Bern","Luzern","Zürich")),]

# or as an alternative you can filter the bfs numbers
# bfs.nummern <- bfs.nummern[which(bfs.nummern$bfs_new %in% bla$BFS_Nummer),]

# Define the output directory
# Three outputs will be saved in the output directory
# i) list_weather_data.RData  : a list with structure [[weather variable]][BFS,date]
# ii) all_weather_data.RData  : a data.frame with all weather variables (long form)
# iii) all_weather_data.csv   : a data.frame with all weather variables (long form)

output.dir <- ""

# =========================================================================================
# Load latest municipality map (here 2021)
# =========================================================================================

# We will derive the coordinates of municipality centers from this map
Gemeinden <- readOGR(dsn = "GIS/BOUNDARIES_2021/DATEN/swissBOUNDARIES3D/SHAPEFILE_LV03_LN02/municipality", layer = "swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET") 

# =========================================================================================
# run script and do not make any changes from here
# everything has been defined above - no need to adjust code below!
# =========================================================================================
  
  # data.frame with latest BFS numbers
  bfs.nummern <- data.frame(bfs.nummern$bfs_new ,bfs.nummern$name_new)
  colnames(bfs.nummern) <- c("BFS_number", "municipality")
  
  # Visualization of selected municipalities in Switzerland
  plot(Gemeinden)
  plot(Gemeinden[which(Gemeinden@data$BFS_NUMMER %in% bfs.nummern$BFS_number),], col="red", add = T)
  
  # Get coordinates of each municipality center (all municipalities in CH)
  Gemeinden.centroid <- as.data.frame(gCentroid(Gemeinden, byid = T))
  Gemeinden.centroid$ID <- as.integer(row.names(Gemeinden.centroid))
  Gemeinden@data$ID <- as.integer(row.names(Gemeinden@data))
  
  # Join coordinates with Gemeinde data (BFS, name, etc.)
  # Join by the ID created above
  bfs.nummern.coordinates.gemeinden <- join(Gemeinden.centroid,Gemeinden@data) 
  
  # Get the municipalities of interest
  bfs.nummern.coordinates <- bfs.nummern.coordinates.gemeinden[which(bfs.nummern.coordinates.gemeinden$BFS_NUMMER %in% bfs.nummern$BFS_number),]
  
  # No duplicates (exclaves, etc.)
  bfs.nummern.coordinates <- bfs.nummern.coordinates[!duplicated(bfs.nummern.coordinates$NAME),]
  
  # Convert it to spatial points
  bfs_spatial <-SpatialPoints(coords=bfs.nummern.coordinates[,1:2],proj4string=crs(Gemeinden))
  
  # Visual check on map
  plot(bfs_spatial, col = "blue", add = T)
  
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
      locations <- spTransform(bfs_spatial,CRSobj = crs(rawdata[[1]])) 
      
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
      locations <- spTransform(bfs_spatial,CRSobj = crs(rawdata[[1]])) 
      
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
      locations <- spTransform(bfs_spatial,CRSobj = crs(rawdata[[1]])) 
      
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
      locations <- spTransform(bfs_spatial,CRSobj = crs(rawdata[[1]])) 
      
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
    locations <- spTransform(bfs_spatial,CRSobj = crs(rawdata[[1]])) 
    
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
    row.names(weatherdata) <- bfs.nummern.coordinates$BFS_NUMMER
    list_weather_data[[i]] <- weatherdata
    rm(weatherdata)
    
  }
  
# =========================================================================================
# Save weather data in the output directory
# ========================================================================================= 
  
# Transform each element of the list into long format (melt)
list_weather_data_melted <- lapply(list_weather_data, melt) 
  for (i in 1:length(variables_of_interest)){
    colnames(list_weather_data_melted[[i]]) <- c("BFS","date",variables_of_interest[i])
  }  
  
# Merge all weather variables into one frame
weather_data_melted <- list_weather_data_melted[[1]]
  if (length(variables_of_interest) !=1){
    for (i in 2:length(variables_of_interest)){
      weather_data_melted <- join(weather_data_melted,list_weather_data_melted[[i]], by=c("BFS","date"))
    }
  } else {}
  
# Save the weather data in the output directory  
save(list_weather_data, file=paste(output.dir,"/list_weather_data.RData",sep=""))
save(weather_data_melted, file=paste(output.dir,"/all_weather_data_melted.RData",sep=""))
write.csv(weather_data_melted, file=paste(output.dir,"/all_weather_data_melted.csv",sep=""))
  
print("Done, have fun with the weather data")
  
    