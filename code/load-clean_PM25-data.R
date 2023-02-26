here::i_am("code/load-clean_PM25-data.R")

library(dplyr)

## PM2.5 ##
#read in raw csv data as data frame

##read in each file by constructing the path and storing frame in list
read_data <- function(name){
  return(read.csv(name))
  return(read.csv(paste("./raw-data/pm25_raw/", name)))
}

#this is my list for storing data frames
all_air_quality_paths <- list.files(path=".", pattern=NULL, all.files=FALSE, full.names=FALSE)
print(all_air_quality_paths)

SLC_PM25_2022_raw <- read.csv("raw-data/PM25_raw/SLC_PM25_2022.csv")

#convert Excel formatted date to date readable by R
SLC_PM25_2022_raw$Date <- as.Date(SLC_PM25_2022_raw$Date, format="%m/%d/%Y")

#aggregate means from each monitor into one overall daily mean
SLC_PM25_2022_agg <- SLC_PM25_2022_raw %>%
  group_by(Date) %>%
  summarize(daily_mean=mean(Daily.Mean.PM2.5.Concentration))

