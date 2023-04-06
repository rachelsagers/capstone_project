here::i_am("code/bar-charts/ALB_over_time_bar.R")

library(dplyr)
library(ggplot2)

### PM2.5 ###
# aggregate all years into one dataframe
ALB_PM25_over_time <- rbind(pm25_aggregated[["ALB_PM25_2022"]],
                            pm25_aggregated[["ALB_PM25_2021"]],
                            pm25_aggregated[["ALB_PM25_2020"]],
                            pm25_aggregated[["ALB_PM25_2019"]],
                            pm25_aggregated[["ALB_PM25_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
ALB_PM25_over_time$month <- format(as.Date(ALB_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
ALB_PM25_over_time$year <- format(as.Date(ALB_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
ALB_PM25_over_time <- ALB_PM25_over_time %>% mutate(pollutant = "PM2.5")

ALB_PM25_over_time <- ALB_PM25_over_time %>%
  filter(between(month,01,03)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2022 ~ "post",
                                 year < 2022 ~ "pre"))

ALB_PM25_over_time$year <- factor(ALB_PM25_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### NO2 ###
# aggregate all years into one dataframe
ALB_NO2_over_time <- rbind(no2_aggregated[["ALB_NO2_2022"]],
                           no2_aggregated[["ALB_NO2_2021"]],
                           no2_aggregated[["ALB_NO2_2020"]],
                           no2_aggregated[["ALB_NO2_2019"]],
                           no2_aggregated[["ALB_NO2_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
ALB_NO2_over_time$month <- format(as.Date(ALB_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
ALB_NO2_over_time$year <- format(as.Date(ALB_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
ALB_NO2_over_time <- ALB_NO2_over_time %>% mutate(pollutant = "NO2")

ALB_NO2_over_time <- ALB_NO2_over_time %>%
  filter(between(month,01,03)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2022 ~ "post",
                                 year < 2022 ~ "pre"))

ALB_NO2_over_time$year <- factor(ALB_NO2_over_time$year, levels=c("2022","2021","2020","2019","2018"))


### CO ###
# aggregate all years into one dataframe
ALB_CO_over_time <- rbind(co_aggregated[["ALB_CO_2022"]],
                          co_aggregated[["ALB_CO_2021"]],
                          co_aggregated[["ALB_CO_2020"]],
                          co_aggregated[["ALB_CO_2019"]],
                          co_aggregated[["ALB_CO_2018"]])

# extract month into new column to filter on and year to group by, add pollutant column
ALB_CO_over_time$month <- format(as.Date(ALB_CO_over_time$Date, format="%d/%m/%Y"), "%m")
ALB_CO_over_time$year <- format(as.Date(ALB_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
ALB_CO_over_time <- ALB_CO_over_time %>% mutate(pollutant = "CO")

ALB_CO_over_time <- ALB_CO_over_time %>%
  filter(between(month,01,03)) %>%
  group_by(year) %>%
  summarise(daily_mean = mean(daily_mean)) %>%
  mutate(pre_or_post = case_when(year >= 2022 ~ "post",
                                 year < 2022 ~ "pre"))

ALB_CO_over_time$year <- factor(ALB_CO_over_time$year, levels=c("2022","2021","2020","2019","2018"))


ALB_over_time_all <- rbind(ALB_PM25_over_time,
                           ALB_NO2_over_time,
                           ALB_CO_over_time)

ALB_PM25_bar <- ggplot(ALB_PM25_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

ALB_NO2_bar <- ggplot(ALB_NO2_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

ALB_CO_bar <- ggplot(ALB_CO_over_time, aes(x=year, y=daily_mean, fill=pre_or_post)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  theme(legend.position = "none")

library(patchwork)
ALB_PM25_bar + ALB_NO2_bar + ALB_CO_bar + plot_annotation(
  title = "Albuquerque Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations for a 3 month period of January-March over 5 years"
)

