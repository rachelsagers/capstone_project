## CO ##
#read in raw csv data as data frame
SLC_CO_2022_raw <- read.csv("raw-data/CO_raw/SLC_CO_2022.csv")

#convert Excel formatted date to date readable by R
SLC_CO_2022_raw$Date <- as.Date(SLC_CO_2022_raw$Date, format="%m/%d/%Y")

#aggregate means from each monitor into one overall daily mean
SLC_CO_2022_agg <- SLC_CO_2022_raw %>%
  group_by(Date) %>%
  summarize(daily_mean=mean(Daily.Max.8.hour.CO.Concentration))