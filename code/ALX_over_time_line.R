here::i_am("code/ALX_over_time_line.R")

library(dplyr)
library(ggplot2)

### Line Graphs (for use in paper) ###
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
ALX_PM25_over_time <- ALX_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
ALX_PM25_over_time$month_day <- as.factor(ALX_PM25_over_time$month_day)
ALX_PM25_over_time <- ALX_PM25_over_time %>% mutate(pollutant = "PM2.5")

ALX_PM25_over_time <- ALX_PM25_over_time %>%
  filter(between(month,09,11)) %>%
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
ALX_NO2_over_time <- ALX_NO2_over_time %>% mutate(month_day = format(Date, "%m/%d"))
ALX_NO2_over_time$month_day <- as.factor(ALX_NO2_over_time$month_day)
ALX_NO2_over_time <- ALX_NO2_over_time %>% mutate(pollutant = "NO2")

ALX_NO2_over_time <- ALX_NO2_over_time %>%
  filter(between(month,09,11)) %>%
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
ALX_CO_over_time <- ALX_CO_over_time %>% mutate(month_day = format(Date, "%m/%d"))
ALX_CO_over_time$month_day <- as.factor(ALX_CO_over_time$month_day)
ALX_CO_over_time <- ALX_CO_over_time %>% mutate(pollutant = "CO")

ALX_CO_over_time <- ALX_CO_over_time %>%
  filter(between(month,09,11)) %>%
  mutate(pre_or_post = case_when(year >= 2021 ~ "post",
                                 year < 2021 ~ "pre"))

ALX_CO_over_time$year <- factor(ALX_CO_over_time$year, levels=c("2022","2021","2020","2019","2018"))

## Building Line Graphs
ALX_PM25_line <- ggplot(ALX_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("09/01","09/07","09/14","09/21","09/28",
                                        "10/05","10/12","10/19","10/26","11/02",
                                        "11/09","11/16","11/23","11/30")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="PM2.5") +
  theme(legend.position = "none")

ALX_NO2_line <- ggplot(ALX_NO2_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("09/01","09/07","09/14","09/21","09/28",
                                        "10/05","10/12","10/19","10/26","11/02",
                                        "11/09","11/16","11/23","11/30")) +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(title="NO2") +
  theme(legend.position = "none")

ALX_CO_line <- ggplot(ALX_CO_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1,aes(color=pre_or_post)) +
  geom_point(size=1.25,aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("09/01","09/07","09/14","09/21","09/28",
                                        "10/05","10/12","10/19","10/26","11/02",
                                        "11/09","11/16","11/23","11/30")) +
  ylab("Daily 8-hr Max Conc (ppm)") +
  labs(title="CO") +
  labs(color = "Pre or Post Implementation") +
  theme(legend.position = "bottom")
#, #legend.justification = c("left","bottom"))

library(patchwork)
ALX_PM25_line / ALX_NO2_line / ALX_CO_line + plot_annotation(  
  title = "Alexandria Pollutant Concentrations Over Time",
  #caption = "These plots display average air pollutant concentrations each February over 5 years"
)

### Save figure as 800 h and 800 w approximately ###