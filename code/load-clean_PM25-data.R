here::i_am("code/load-clean_PM25-data.R")

library(dplyr)

## PM2.5 ##

##read in each file by constructing the path and storing frame in list
read_data <- function(name){
  path <- gsub(" ", "", paste("./raw-data/pm25_raw/", name))
  return(read.csv(path))
}

create_names <- function(file_name){
  return(gsub(".csv", "", file_name))  
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
  return(PM25_aggregated)
}

#this is my list for storing data frames
pm25_files <- list.files(path="./raw-data/pm25_raw", pattern=NULL, all.files=FALSE, full.names=FALSE)
data_names <- lapply(pm25_files, create_names)

pm25_raw <- lapply(pm25_files, read_data)
test <- setNames(pm25_raw, data_names)
names(pm25_raw) = data_names

pm25_cleaned <- lapply(pm25_raw, clean_data)

pm25_aggregated <- lapply(pm25_cleaned, aggregate_data)



