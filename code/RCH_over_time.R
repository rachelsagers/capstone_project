here::i_am("code/RCH_over_time.R")

library(dplyr)
library(ggplot2)

### PM2.5 ###
# aggregate all years into one dataframe
RCH_PM25_over_time <- rbind(pm25_aggregated[["RCH_PM25_2021"]],
                            pm25_aggregated[["RCH_PM25_2020"]],
                            pm25_aggregated[["RCH_PM25_2019"]],
                            pm25_aggregated[["RCH_PM25_2018"]],
                            pm25_aggregated[["RCH_PM25_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
RCH_PM25_over_time$month <- format(as.Date(RCH_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
RCH_PM25_over_time$year <- format(as.Date(RCH_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
RCH_PM25_over_time <- RCH_PM25_over_time %>% mutate(pollutant = "PM2.5")

RCH_PM25_over_time <- RCH_PM25_over_time %>%
  filter(between(month,03,05)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

RCH_PM25_over_time$year <- factor(RCH_PM25_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### NO2 ###
# aggregate all years into one dataframe
RCH_NO2_over_time <- rbind(no2_aggregated[["RCH_NO2_2021"]],
                           no2_aggregated[["RCH_NO2_2020"]],
                           no2_aggregated[["RCH_NO2_2019"]],
                           no2_aggregated[["RCH_NO2_2018"]],
                           no2_aggregated[["RCH_NO2_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
RCH_NO2_over_time$month <- format(as.Date(RCH_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
RCH_NO2_over_time$year <- format(as.Date(RCH_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
RCH_NO2_over_time <- RCH_NO2_over_time %>% mutate(pollutant = "NO2")

RCH_NO2_over_time <- RCH_NO2_over_time %>%
  filter(between(month,03,05)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

RCH_NO2_over_time$year <- factor(RCH_NO2_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### CO ###
# aggregate all years into one dataframe
RCH_CO_over_time <- rbind(co_aggregated[["RCH_CO_2021"]],
                          co_aggregated[["RCH_CO_2020"]],
                          co_aggregated[["RCH_CO_2019"]],
                          co_aggregated[["RCH_CO_2018"]],
                          co_aggregated[["RCH_CO_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
RCH_CO_over_time$month <- format(as.Date(RCH_CO_over_time$Date, format="%d/%m/%Y"), "%m")
RCH_CO_over_time$year <- format(as.Date(RCH_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
RCH_CO_over_time <- RCH_CO_over_time %>% mutate(pollutant = "CO")

RCH_CO_over_time <- RCH_CO_over_time %>%
  filter(between(month,03,05)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

RCH_CO_over_time$year <- factor(RCH_CO_over_time$year, levels=c("2021","2020","2019","2018","2017"))


RCH_over_time_all <- rbind(RCH_PM25_over_time,
                           RCH_NO2_over_time,
                           RCH_CO_over_time)

RCH_PM25_bar <- ggplot(RCH_PM25_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

RCH_NO2_bar <- ggplot(RCH_NO2_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

RCH_CO_bar <- ggplot(RCH_CO_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  theme(legend.position = "none")

library(patchwork)
RCH_PM25_bar + RCH_NO2_bar + RCH_CO_bar + plot_annotation(
  title = "Richmond Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations for a 3 month period of March-May over 5 years"
)

