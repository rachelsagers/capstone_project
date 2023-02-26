here::i_am("code/load-clean_PM25-data.R")

library(dplyr)

## PM2.5 ##

##read in each file by constructing the path and storing frame in list
read_data <- function(name){
  path <- gsub(" ", "", paste("./raw-data/pm25_raw/", name))
  return(read.csv(path))
}

clean_data <- function(rawAirQual){
  rawAirQual$Date <- as.Date(rawAirQual$Date, format="%m/%d/%Y")
  return(rawAirQual)
}

aggregate_data <- function(airQualWithDateFormated){
  #aggregate means from each monitor into one overall daily mean
  PM25_aggregated <- airQualWithDateFormated %>%
    group_by(Date) %>%
    summarize(daily_mean=mean(Daily.Mean.PM2.5.Concentration))
  return(airQualConverted)
}

#this is my list for storing data frames
pm25_files <- list.files(path="./raw-data/pm25_raw", pattern=NULL, all.files=FALSE, full.names=FALSE)
print(pm25_files)

pm25_raw <- lapply(pm25_files, read_data)

pm25_cleaned <- lapply(pm25_raw, clean_data)

pm25_converted <- lapply(pm25_cleaned, convert_data)




#SLC_PM25_2022_raw <- read.csv("raw-data/PM25_raw/SLC_PM25_2022.csv")
#SLC_PM25_2022_raw$Date <- as.Date(SLC_PM25_2022_raw$Date, format="%m/%d/%Y")

#SLC_PM25_2021_raw <- read.csv("raw-data/PM25_raw/SLC_PM25_2021.csv")

#SLC_PM25_2021_raw_newdate <-



