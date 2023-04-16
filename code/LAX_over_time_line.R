here::i_am("code/LAX_over_time_line.R")

library(dplyr)
library(ggplot2)

### Line Graphs (for use in paper) ###
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
LAX_PM25_over_time <- LAX_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
LAX_PM25_over_time$month_day <- as.factor(LAX_PM25_over_time$month_day)
LAX_PM25_over_time <- LAX_PM25_over_time %>% mutate(pollutant = "PM2.5")

LAX_PM25_over_time <- LAX_PM25_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

LAX_PM25_over_time$year <- factor(LAX_PM25_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### NO2 ###
# aggregate all years into one dataframe
LAX_NO2_over_time <- rbind(no2_aggregated[["LAX_NO2_2021"]],
                           no2_aggregated[["LAX_NO2_2020"]],
                           no2_aggregated[["LAX_NO2_2019"]],
                           no2_aggregated[["LAX_NO2_2018"]],
                           no2_aggregated[["LAX_NO2_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
LAX_NO2_over_time$month <- format(as.Date(LAX_NO2_over_time$Date, format="%d/%m/%Y"), "%m")
LAX_NO2_over_time$year <- format(as.Date(LAX_NO2_over_time$Date, format="%d/%m/%Y"), "%Y")
LAX_NO2_over_time <- LAX_NO2_over_time %>% mutate(month_day = format(Date, "%m/%d"))
LAX_NO2_over_time$month_day <- as.factor(LAX_NO2_over_time$month_day)
LAX_NO2_over_time <- LAX_NO2_over_time %>% mutate(pollutant = "NO2")

LAX_NO2_over_time <- LAX_NO2_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

LAX_NO2_over_time$year <- factor(LAX_NO2_over_time$year, levels=c("2021","2020","2019","2018","2017"))


### CO ###
# aggregate all years into one dataframe
LAX_CO_over_time <- rbind(co_aggregated[["LAX_CO_2021"]],
                          co_aggregated[["LAX_CO_2020"]],
                          co_aggregated[["LAX_CO_2019"]],
                          co_aggregated[["LAX_CO_2018"]],
                          co_aggregated[["LAX_CO_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
LAX_CO_over_time$month <- format(as.Date(LAX_CO_over_time$Date, format="%d/%m/%Y"), "%m")
LAX_CO_over_time$year <- format(as.Date(LAX_CO_over_time$Date, format="%d/%m/%Y"), "%Y")
LAX_CO_over_time <- LAX_CO_over_time %>% mutate(month_day = format(Date, "%m/%d"))
LAX_CO_over_time$month_day <- as.factor(LAX_CO_over_time$month_day)
LAX_CO_over_time <- LAX_CO_over_time %>% mutate(pollutant = "CO")

LAX_CO_over_time <- LAX_CO_over_time %>%
  filter(between(month,03,05)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

LAX_CO_over_time$year <- factor(LAX_CO_over_time$year, levels=c("2021","2020","2019","2018","2017"))

## Building Line Graphs
LAX_PM25_line <- ggplot(LAX_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

LAX_NO2_line <- ggplot(LAX_NO2_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("03/01","03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25","05/02",
                                        "05/09","05/16","05/23","05/30")) +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

LAX_CO_line <- ggplot(LAX_CO_over_time, aes(x=month_day, y=daily_mean, group=year)) +
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
LAX_PM25_line / LAX_NO2_line / LAX_CO_line + plot_annotation(  
  title = "Los Angeles Pollutant Concentrations Over Time",
  #caption = "These plots display average air pollutant concentrations each February over 5 years"
)

### Save figure as 800 h and 800 w approximately ###