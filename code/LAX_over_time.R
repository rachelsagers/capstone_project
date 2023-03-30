here::i_am("code/LAX_over_time.R")

library(dplyr)
library(ggplot2)

### PM2.5 ###
# aggregate all years into one dataframe
LAX_PM25_over_time <- rbind(pm25_aggregated[["LAX_PM25_2021"]],
                            pm25_aggregated[["LAX_PM25_2020"]],
                            pm25_aggregated[["LAX_PM25_2019"]],
                            pm25_aggregated[["LAX_PM25_2018"]],
                            pm25_aggregated[["LAX_PM25_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
LAX_PM25_over_time$month <- format(as.Date(LAX_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
LAX_PM25_over_time$year <- format(as.Date(LAX_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
LAX_PM25_over_time <- LAX_PM25_over_time %>% mutate(pollutant = "PM2.5")

LAX_PM25_over_time <- LAX_PM25_over_time %>%
  filter(month == "02") %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

LAX_PM25_over_time$year <- factor(LAX_PM25_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### NO2 ###
# aggregate all years into one dataframe
LAX_NO2_over_time <- rbind(no2_aggregated[["LAX_NO2_2022"]],
                           no2_aggregated[["LAX_NO2_2021"]],
                           no2_aggregated[["LAX_NO2_2020"]],
                           no2_aggregated[["LAX_NO2_2019"]],
                           no2_aggregated[["LAX_NO2_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
LAX_NO2_over_time$month <- format(as.Date(LAX_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
LAX_NO2_over_time$year <- format(as.Date(LAX_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
LAX_NO2_over_time <- LAX_NO2_over_time %>% mutate(pollutant = "NO2")

LAX_NO2_over_time <- LAX_NO2_over_time %>%
  filter(month == "02") %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

LAX_NO2_over_time$year <- factor(LAX_NO2_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### CO ###
# aggregate all years into one dataframe
LAX_CO_over_time <- rbind(co_aggregated[["LAX_CO_2022"]],
                          co_aggregated[["LAX_CO_2021"]],
                          co_aggregated[["LAX_CO_2020"]],
                          co_aggregated[["LAX_CO_2019"]],
                          co_aggregated[["LAX_CO_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
LAX_CO_over_time$month <- format(as.Date(LAX_CO_over_time$Date, format="%d/%m/%Y"), "%m")
LAX_CO_over_time$year <- format(as.Date(LAX_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
LAX_CO_over_time <- LAX_CO_over_time %>% mutate(pollutant = "CO")

LAX_CO_over_time <- LAX_CO_over_time %>%
  filter(month == "02") %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

LAX_CO_over_time$year <- factor(LAX_CO_over_time$year, levels=c("2022","2021","2020","2019","2018"))


LAX_over_time_all <- rbind(LAX_PM25_over_time,
                           LAX_NO2_over_time,
                           LAX_CO_over_time)

LAX_PM25_bar <- ggplot(LAX_PM25_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

LAX_NO2_bar <- ggplot(LAX_NO2_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

LAX_CO_bar <- ggplot(LAX_CO_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ppm)") +
  labs(title="CO") +
  theme(legend.position = "none")

library(patchwork)
LAX_PM25_bar + LAX_NO2_bar + LAX_CO_bar + plot_annotation(
  title = "Salt Lake City Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations each February over 5 years"
)

