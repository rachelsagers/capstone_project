here::i_am("code/RCH_over_time_line.R")

library(dplyr)
library(ggplot2)

### Line Graphs (for use in paper) ###
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
RCH_PM25_over_time <- RCH_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
RCH_PM25_over_time$month_day <- as.factor(RCH_PM25_over_time$month_day)
RCH_PM25_over_time <- RCH_PM25_over_time %>% mutate(pollutant = "PM2.5")

RCH_PM25_over_time <- RCH_PM25_over_time %>%
  filter(between(month,03,05)) %>%
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
RCH_NO2_over_time <- RCH_NO2_over_time %>% mutate(month_day = format(Date, "%m/%d"))
RCH_NO2_over_time$month_day <- as.factor(RCH_NO2_over_time$month_day)
RCH_NO2_over_time <- RCH_NO2_over_time %>% mutate(pollutant = "NO2")

RCH_NO2_over_time <- RCH_NO2_over_time %>%
  filter(between(month,03,05)) %>%
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
RCH_CO_over_time <- RCH_CO_over_time %>% mutate(month_day = format(Date, "%m/%d"))
RCH_CO_over_time$month_day <- as.factor(RCH_CO_over_time$month_day)
RCH_CO_over_time <- RCH_CO_over_time %>% mutate(pollutant = "CO")

RCH_CO_over_time <- RCH_CO_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

RCH_CO_over_time$year <- factor(RCH_CO_over_time$year, levels=c("2021","2020","2019","2018","2017"))

## Building Line Graphs
RCH_PM25_line <- ggplot(RCH_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

RCH_NO2_line <- ggplot(RCH_NO2_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

RCH_CO_line <- ggplot(RCH_CO_over_time, aes(x=month_day, y=daily_mean, group=year)) +
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
RCH_PM25_line / RCH_NO2_line / RCH_CO_line + plot_annotation(  
  title = "Richmond Pollutant Concentrations Over Time",
  #caption = "These plots display average air pollutant concentrations each February over 5 years"
)

### Save figure as 800 h and 800 w approximately ###