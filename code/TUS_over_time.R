here::i_am("code/TUS_over_time.R")

library(dplyr)
library(ggplot2)

### PM2.5 ###
# aggregate all years into one dataframe
TUS_PM25_over_time <- rbind(pm25_aggregated[["TUS_PM25_2021"]],
                            pm25_aggregated[["TUS_PM25_2020"]],
                            pm25_aggregated[["TUS_PM25_2019"]],
                            pm25_aggregated[["TUS_PM25_2018"]],
                            pm25_aggregated[["TUS_PM25_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
TUS_PM25_over_time$month <- format(as.Date(TUS_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
TUS_PM25_over_time$year <- format(as.Date(TUS_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
TUS_PM25_over_time <- TUS_PM25_over_time %>% mutate(pollutant = "PM2.5")

TUS_PM25_over_time <- TUS_PM25_over_time %>%
  filter(between(month,03,05)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

TUS_PM25_over_time$year <- factor(TUS_PM25_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### NO2 ###
# aggregate all years into one dataframe
TUS_NO2_over_time <- rbind(no2_aggregated[["TUS_NO2_2021"]],
                           no2_aggregated[["TUS_NO2_2020"]],
                           no2_aggregated[["TUS_NO2_2019"]],
                           no2_aggregated[["TUS_NO2_2018"]],
                           no2_aggregated[["TUS_NO2_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
TUS_NO2_over_time$month <- format(as.Date(TUS_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
TUS_NO2_over_time$year <- format(as.Date(TUS_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
TUS_NO2_over_time <- TUS_NO2_over_time %>% mutate(pollutant = "NO2")

TUS_NO2_over_time <- TUS_NO2_over_time %>%
  filter(between(month,03,05)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

TUS_NO2_over_time$year <- factor(TUS_NO2_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### CO ###
# aggregate all years into one dataframe
TUS_CO_over_time <- rbind(co_aggregated[["TUS_CO_2021"]],
                          co_aggregated[["TUS_CO_2020"]],
                          co_aggregated[["TUS_CO_2019"]],
                          co_aggregated[["TUS_CO_2018"]],
                          co_aggregated[["TUS_CO_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
TUS_CO_over_time$month <- format(as.Date(TUS_CO_over_time$Date, format="%d/%m/%Y"), "%m")
TUS_CO_over_time$year <- format(as.Date(TUS_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
TUS_CO_over_time <- TUS_CO_over_time %>% mutate(pollutant = "CO")

TUS_CO_over_time <- TUS_CO_over_time %>%
  filter(between(month,03,05)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

TUS_CO_over_time$year <- factor(TUS_CO_over_time$year, levels=c("2021","2020","2019","2018","2017"))


TUS_over_time_all <- rbind(TUS_PM25_over_time,
                           TUS_NO2_over_time,
                           TUS_CO_over_time)

TUS_PM25_bar <- ggplot(TUS_PM25_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

TUS_NO2_bar <- ggplot(TUS_NO2_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

TUS_CO_bar <- ggplot(TUS_CO_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  theme(legend.position = "none")

library(patchwork)
TUS_PM25_bar + TUS_NO2_bar + TUS_CO_bar + plot_annotation(
  title = "Tuscon Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations for a 3 month period of March-May over 5 years"
)

