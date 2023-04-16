here::i_am("code/SLC_over_time_line.R")

library(dplyr)
library(ggplot2)

### Line Graphs (for use in paper) ###
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
SLC_PM25_over_time <- SLC_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
SLC_PM25_over_time$month_day <- as.factor(SLC_PM25_over_time$month_day)
SLC_PM25_over_time <- SLC_PM25_over_time %>% mutate(pollutant = "PM2.5")

SLC_PM25_over_time <- SLC_PM25_over_time %>%
  filter(month == "02") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

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
SLC_NO2_over_time <- SLC_NO2_over_time %>% mutate(month_day = format(Date, "%m/%d"))
SLC_NO2_over_time$month_day <- as.factor(SLC_NO2_over_time$month_day)
SLC_NO2_over_time <- SLC_NO2_over_time %>% mutate(pollutant = "NO2")

SLC_NO2_over_time <- SLC_NO2_over_time %>%
  filter(month == "02") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

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
SLC_CO_over_time <- SLC_CO_over_time %>% mutate(month_day = format(Date, "%m/%d"))
SLC_CO_over_time$month_day <- as.factor(SLC_CO_over_time$month_day)
SLC_CO_over_time <- SLC_CO_over_time %>% mutate(pollutant = "CO")

SLC_CO_over_time <- SLC_CO_over_time %>%
  filter(month == "02") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

SLC_CO_over_time$year <- factor(SLC_CO_over_time$year, levels=c("2022","2021","2020","2019","2018"))

SLC_PM25_line <- ggplot(SLC_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("02/01","02/04","02/07","02/10","02/13",
                                        "02/16","02/19","02/22","02/25","02/28")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

SLC_NO2_line <- ggplot(SLC_NO2_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("02/01","02/04","02/07","02/10","02/13",
                                        "02/16","02/19","02/22","02/25","02/28")) +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

SLC_CO_line <- ggplot(SLC_CO_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("02/01","02/04","02/07","02/10","02/13",
                                        "02/16","02/19","02/22","02/25","02/28")) +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  labs(color = "Pre or Post Implementation") +
  theme(legend.position = "bottom", legend.justification = c("left","bottom"))

library(patchwork)
SLC_PM25_line / SLC_NO2_line / SLC_CO_line + plot_annotation(
  title = "Salt Lake City Pollutant Concentrations Over Time",
  caption = "These plots display average air pollutant concentrations each February over 5 years"
)

### Save figure as 700 h and 500 w approximately ###