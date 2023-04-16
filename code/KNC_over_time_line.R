here::i_am("code/KNC_over_time_line.R")

library(dplyr)
library(ggplot2)

### Line Graphs (for use in paper) ###
### PM2.5 ###
# aggregate all years into one dataframe
KNC_PM25_over_time <- rbind(pm25_aggregated[["KNC_PM25_2021"]],
                            pm25_aggregated[["KNC_PM25_2020"]],
                            pm25_aggregated[["KNC_PM25_2019"]],
                            pm25_aggregated[["KNC_PM25_2018"]],
                            pm25_aggregated[["KNC_PM25_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
KNC_PM25_over_time$month <- format(as.Date(KNC_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
KNC_PM25_over_time$year <- format(as.Date(KNC_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
KNC_PM25_over_time <- KNC_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
KNC_PM25_over_time$month_day <- as.factor(KNC_PM25_over_time$month_day)
KNC_PM25_over_time <- KNC_PM25_over_time %>% mutate(pollutant = "PM2.5")

KNC_PM25_over_time <- KNC_PM25_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

KNC_PM25_over_time$year <- factor(KNC_PM25_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### NO2 ###
# aggregate all years into one dataframe
KNC_NO2_over_time <- rbind(no2_aggregated[["KNC_NO2_2021"]],
                           no2_aggregated[["KNC_NO2_2020"]],
                           no2_aggregated[["KNC_NO2_2019"]],
                           no2_aggregated[["KNC_NO2_2018"]],
                           no2_aggregated[["KNC_NO2_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
KNC_NO2_over_time$month <- format(as.Date(KNC_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
KNC_NO2_over_time$year <- format(as.Date(KNC_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
KNC_NO2_over_time <- KNC_NO2_over_time %>% mutate(month_day = format(Date, "%m/%d"))
KNC_NO2_over_time$month_day <- as.factor(KNC_NO2_over_time$month_day)
KNC_NO2_over_time <- KNC_NO2_over_time %>% mutate(pollutant = "NO2")

KNC_NO2_over_time <- KNC_NO2_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

KNC_NO2_over_time$year <- factor(KNC_NO2_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### CO ###
# aggregate all years into one dataframe
KNC_CO_over_time <- rbind(co_aggregated[["KNC_CO_2021"]],
                          co_aggregated[["KNC_CO_2020"]],
                          co_aggregated[["KNC_CO_2019"]],
                          co_aggregated[["KNC_CO_2018"]],
                          co_aggregated[["KNC_CO_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
KNC_CO_over_time$month <- format(as.Date(KNC_CO_over_time$Date, format="%d/%m/%Y"), "%m")
KNC_CO_over_time$year <- format(as.Date(KNC_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
KNC_CO_over_time <- KNC_CO_over_time %>% mutate(month_day = format(Date, "%m/%d"))
KNC_CO_over_time$month_day <- as.factor(KNC_CO_over_time$month_day)
KNC_CO_over_time <- KNC_CO_over_time %>% mutate(pollutant = "CO")

KNC_CO_over_time <- KNC_CO_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

KNC_CO_over_time$year <- factor(KNC_CO_over_time$year, levels=c("2021","2020","2019","2018","2017"))

## Building Line Graphs
KNC_PM25_line <- ggplot(KNC_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

KNC_NO2_line <- ggplot(KNC_NO2_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

KNC_CO_line <- ggplot(KNC_CO_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1,aes(color=pre_or_post)) +
  geom_point(size=1.25,aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  labs(color = "Pre or Post Implementation") +
  theme(legend.position = "bottom")
#, #legend.justification = c("left","bottom"))

library(patchwork)
KNC_PM25_line / KNC_NO2_line / KNC_CO_line + plot_annotation(  
  title = "Kansas City Pollutant Concentrations Over Time",
  #caption = "These plots display average air pollutant concentrations each February over 5 years"
)

### Save figure as 800 h and 800 w approximately ###