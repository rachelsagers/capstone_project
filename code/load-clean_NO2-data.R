here::i_am("code/load-clean_NO2-data.R")

library(dplyr)

## NO2 ##

##read in each file by constructing the path and storing frame in list
read_data <- function(name){
  path <- gsub(" ", "", paste("./raw-data/no2_raw/", name))
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
  no2_aggregated <- airQualWithDateFormated %>%
    group_by(Date) %>%
    summarize(daily_mean=mean(Daily.Max.1.hour.NO2.Concentration))
  return(no2_aggregated)
}

#this is my list for storing data frames
no2_files <- list.files(path="./raw-data/no2_raw", pattern=NULL, all.files=FALSE, full.names=FALSE)
data_names <- lapply(no2_files, create_names)

no2_raw <- lapply(no2_files, read_data)
test <- setNames(no2_raw, data_names)
names(no2_raw) = data_names

no2_cleaned <- lapply(no2_raw, clean_data)

no2_aggregated <- lapply(no2_cleaned, aggregate_data)