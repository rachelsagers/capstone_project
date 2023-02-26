


## NO2 ##
#read in raw csv data as data frame
SLC_NO2_2022_raw <- read.csv("raw-data/NO2_raw/SLC_NO2_2022.csv")

#convert Excel formatted date to date readable by R
SLC_NO2_2022_raw$Date <- as.Date(SLC_NO2_2022_raw$Date, format="%m/%d/%Y")

#aggregate means from each monitor into one overall daily mean
SLC_NO2_2022_agg <- SLC_NO2_2022_raw %>%
  group_by(Date) %>%
  summarize(daily_mean=mean(Daily.Max.1.hour.NO2.Concentration))