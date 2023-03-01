here::i_am("code/load-clean_CO-data.R")

library(dplyr)

## CO ##

##read in each file by constructing the path and storing frame in list
read_data <- function(name){
  path <- gsub(" ", "", paste("./raw-data/co_raw/", name))
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
  co_aggregated <- airQualWithDateFormated %>%
    group_by(Date) %>%
    summarize(daily_mean=mean(Daily.Max.8.hour.CO.Concentration))
  return(co_aggregated)
}

#this is my list for storing data frames
co_files <- list.files(path="./raw-data/co_raw", pattern=NULL, all.files=FALSE, full.names=FALSE)
data_names <- lapply(co_files, create_names)

co_raw <- lapply(co_files, read_data)
test <- setNames(co_raw, data_names)
names(co_raw) = data_names

co_cleaned <- lapply(co_raw, clean_data)

co_aggregated <- lapply(co_cleaned, aggregate_data)