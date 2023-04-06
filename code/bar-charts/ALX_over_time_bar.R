here::i_am("code/bar-charts/ALX_over_time_bar.R")

library(dplyr)
library(ggplot2)

### PM2.5 ###
# aggregate all years into one dataframe
ALX_PM25_over_time <- rbind(pm25_aggregated[["ALX_PM25_2022"]],
                            pm25_aggregated[["ALX_PM25_2021"]],
                            pm25_aggregated[["ALX_PM25_2020"]],
                            pm25_aggregated[["ALX_PM25_2019"]],
                            pm25_aggregated[["ALX_PM25_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
ALX_PM25_over_time$month <- format(as.Date(ALX_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
ALX_PM25_over_time$year <- format(as.Date(ALX_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
ALX_PM25_over_time <- ALX_PM25_over_time %>% mutate(pollutant = "PM2.5")

ALX_PM25_over_time <- ALX_PM25_over_time %>%
  filter(between(month,09,11)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2021 ~ "post",
                                 year < 2021 ~ "pre"))

ALX_PM25_over_time$year <- factor(ALX_PM25_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### NO2 ###
# aggregate all years into one dataframe
ALX_NO2_over_time <- rbind(no2_aggregated[["ALX_NO2_2022"]],
                           no2_aggregated[["ALX_NO2_2021"]],
                           no2_aggregated[["ALX_NO2_2020"]],
                           no2_aggregated[["ALX_NO2_2019"]],
                           no2_aggregated[["ALX_NO2_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
ALX_NO2_over_time$month <- format(as.Date(ALX_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
ALX_NO2_over_time$year <- format(as.Date(ALX_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
ALX_NO2_over_time <- ALX_NO2_over_time %>% mutate(pollutant = "NO2")

ALX_NO2_over_time <- ALX_NO2_over_time %>%
  filter(between(month,09,11)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2021 ~ "post",
                                 year < 2021 ~ "pre"))

ALX_NO2_over_time$year <- factor(ALX_NO2_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### CO ###
# aggregate all years into one dataframe
ALX_CO_over_time <- rbind(co_aggregated[["ALX_CO_2022"]],
                          co_aggregated[["ALX_CO_2021"]],
                          co_aggregated[["ALX_CO_2020"]],
                          co_aggregated[["ALX_CO_2019"]],
                          co_aggregated[["ALX_CO_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
ALX_CO_over_time$month <- format(as.Date(ALX_CO_over_time$Date, format="%d/%m/%Y"), "%m")
ALX_CO_over_time$year <- format(as.Date(ALX_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
ALX_CO_over_time <- ALX_CO_over_time %>% mutate(pollutant = "CO")

ALX_CO_over_time <- ALX_CO_over_time %>%
  filter(between(month,09,11)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2021 ~ "post",
                                 year < 2021 ~ "pre"))

ALX_CO_over_time$year <- factor(ALX_CO_over_time$year, levels=c("2022","2021","2020","2019","2018"))


ALX_over_time_all <- rbind(ALX_PM25_over_time,
                           ALX_NO2_over_time,
                           ALX_CO_over_time)

ALX_PM25_bar <- ggplot(ALX_PM25_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

ALX_NO2_bar <- ggplot(ALX_NO2_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

ALX_CO_bar <- ggplot(ALX_CO_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  theme(legend.position = "none")

library(patchwork)
ALX_PM25_bar + ALX_NO2_bar + ALX_CO_bar + plot_annotation(
  title = "Alexandria Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations for a 3 month period of September-November over 5 years"
)

