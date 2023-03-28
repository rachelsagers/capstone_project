here::i_am("code/SLC_over_time.R")

library(dplyr)
library(ggplot2)

### PM2.5 ###
# aggregate all years into one dataframe
SLC_PM25_over_time <- rbind(pm25_aggregated[["SLC_PM25_2022"]],
                            pm25_aggregated[["SLC_PM25_2021"]],
                            pm25_aggregated[["SLC_PM25_2020"]],
                            pm25_aggregated[["SLC_PM25_2019"]],
                            pm25_aggregated[["SLC_PM25_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
SLC_PM25_over_time$month <- format(as.Date(SLC_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
SLC_PM25_over_time$year <- format(as.Date(SLC_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
SLC_PM25_over_time <- SLC_PM25_over_time %>% mutate(pollutant = "PM2.5")

SLC_PM25_over_time <- SLC_PM25_over_time %>%
  filter(month == "02") %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean))

SLC_PM25_over_time$year <- factor(SLC_PM25_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### NO2 ###
# aggregate all years into one dataframe
SLC_NO2_over_time <- rbind(no2_aggregated[["SLC_NO2_2022"]],
                           no2_aggregated[["SLC_NO2_2021"]],
                           no2_aggregated[["SLC_NO2_2020"]],
                           no2_aggregated[["SLC_NO2_2019"]],
                           no2_aggregated[["SLC_NO2_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
SLC_NO2_over_time$month <- format(as.Date(SLC_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
SLC_NO2_over_time$year <- format(as.Date(SLC_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
SLC_NO2_over_time <- SLC_NO2_over_time %>% mutate(pollutant = "NO2")

SLC_NO2_over_time <- SLC_NO2_over_time %>%
  filter(month == "02") %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean))

SLC_NO2_over_time$year <- factor(SLC_NO2_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### CO ###
# aggregate all years into one dataframe
SLC_CO_over_time <- rbind(co_aggregated[["SLC_CO_2022"]],
                          co_aggregated[["SLC_CO_2021"]],
                          co_aggregated[["SLC_CO_2020"]],
                          co_aggregated[["SLC_CO_2019"]],
                          co_aggregated[["SLC_CO_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
SLC_CO_over_time$month <- format(as.Date(SLC_CO_over_time$Date, format="%d/%m/%Y"), "%m")
SLC_CO_over_time$year <- format(as.Date(SLC_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
SLC_CO_over_time <- SLC_CO_over_time %>% mutate(pollutant = "CO")

SLC_CO_over_time <- SLC_CO_over_time %>%
  filter(month == "02") %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean))

SLC_CO_over_time$year <- factor(SLC_CO_over_time$year, levels=c("2022","2021","2020","2019","2018"))


SLC_over_time_all <- rbind(SLC_PM25_over_time,
                           SLC_NO2_over_time,
                           SLC_CO_over_time)

SLC_PM25_bar <- ggplot(SLC_PM25_over_time, aes(x=year, y=daily_mean)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5")

SLC_NO2_bar <- ggplot(SLC_NO2_over_time, aes(x=year, y=daily_mean)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ppb)") +
  labs(title="NO2")

SLC_CO_bar <- ggplot(SLC_CO_over_time, aes(x=year, y=daily_mean)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ppm)") +
  labs(title="CO")

library(patchwork)
SLC_PM25_bar + SLC_NO2_bar + SLC_CO_bar + plot_annotation(
  title = "Salt Lake City Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations each February over 5 years"
)

