here::i_am("code/MIS-COR-COR-OLY_over_time_line.R")

library(dplyr)
library(ggplot2)

### Line Graphs (for use in paper) ###
### Missoula ###
# aggregate all years into one dataframe
MIS_PM25_over_time <- rbind(pm25_aggregated[["MIS_PM25_2016"]],
                            pm25_aggregated[["MIS_PM25_2015"]],
                            pm25_aggregated[["MIS_PM25_2014"]],
                            pm25_aggregated[["MIS_PM25_2013"]],
                            pm25_aggregated[["MIS_PM25_2012"]])

# extract month into new column to filter on and year to group by, add pollutant column
MIS_PM25_over_time$month <- format(as.Date(MIS_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
MIS_PM25_over_time$year <- format(as.Date(MIS_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
MIS_PM25_over_time <- MIS_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
MIS_PM25_over_time$month_day <- as.factor(MIS_PM25_over_time$month_day)
MIS_PM25_over_time <- MIS_PM25_over_time %>% mutate(pollutant = "PM2.5")

MIS_PM25_over_time <- MIS_PM25_over_time %>%
  filter(between(month,01,03)) %>%
  mutate(pre_or_post = case_when(year >= 2015 ~ "post",
                                 year < 2015 ~ "pre"))

MIS_PM25_over_time$year <- factor(MIS_PM25_over_time$year, levels=c("2016","2015","2014","2013","2012"))
MIS_PM25_over_time <- subset(MIS_PM25_over_time, month_day!="02/29")

### Chapel Hill ###
# aggregate all years into one dataframe
CHP_PM25_over_time <- rbind(pm25_aggregated[["CHP_PM25_2003"]],
                            pm25_aggregated[["CHP_PM25_2002"]],
                            pm25_aggregated[["CHP_PM25_2001"]],
                            pm25_aggregated[["CHP_PM25_2000"]],
                            pm25_aggregated[["CHP_PM25_1999"]])

# extract month into new column to filter on and year to group by, add pollutant column
CHP_PM25_over_time$month <- format(as.Date(CHP_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
CHP_PM25_over_time$year <- format(as.Date(CHP_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
CHP_PM25_over_time <- CHP_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
CHP_PM25_over_time$month_day <- as.factor(CHP_PM25_over_time$month_day)
CHP_PM25_over_time <- CHP_PM25_over_time %>% mutate(pollutant = "PM2.5")

CHP_PM25_over_time <- CHP_PM25_over_time %>%
  filter(between(month,01,03)) %>%
  mutate(pre_or_post = case_when(year >= 2002 ~ "post",
                                 year < 2002 ~ "pre"))

CHP_PM25_over_time$year <- factor(CHP_PM25_over_time$year, levels=c("2003","2002","2001","2000","1999"))
CHP_PM25_over_time <- subset(CHP_PM25_over_time, month_day!="02/29")

### Corvallis ###
# aggregate all years into one dataframe
COR_PM25_over_time <- rbind(pm25_aggregated[["COR_PM25_2012"]],
                            pm25_aggregated[["COR_PM25_2011"]],
                            pm25_aggregated[["COR_PM25_2010"]],
                            pm25_aggregated[["COR_PM25_2009"]],
                            pm25_aggregated[["COR_PM25_2008"]])

# extract month into new column to filter on and year to group by, add pollutant column
COR_PM25_over_time$month <- format(as.Date(COR_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
COR_PM25_over_time$year <- format(as.Date(COR_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
COR_PM25_over_time <- COR_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
COR_PM25_over_time$month_day <- as.factor(COR_PM25_over_time$month_day)
COR_PM25_over_time <- COR_PM25_over_time %>% mutate(pollutant = "PM2.5")

COR_PM25_over_time <- COR_PM25_over_time %>%
  filter(between(month,02,04)) %>%
  mutate(pre_or_post = case_when(year >= 2011 ~ "post",
                                 year < 2011 ~ "pre"))

COR_PM25_over_time$year <- factor(COR_PM25_over_time$year, levels=c("2012","2011","2010","2009","2008"))
COR_PM25_over_time <- subset(COR_PM25_over_time, month_day!="02/29")

### Olympia ###
# aggregate all years into one dataframe
OLY_PM25_over_time <- rbind(pm25_aggregated[["OLY_PM25_2021"]],
                            pm25_aggregated[["OLY_PM25_2020"]],
                            pm25_aggregated[["OLY_PM25_2019"]],
                            pm25_aggregated[["OLY_PM25_2018"]],
                            pm25_aggregated[["OLY_PM25_2017"]])

# extract month into new column to filter on and year to group by, add pollutant column
OLY_PM25_over_time$month <- format(as.Date(OLY_PM25_over_time$Date, format="%d/%m/%Y"), "%m")
OLY_PM25_over_time$year <- format(as.Date(OLY_PM25_over_time$Date, format="%d/%m/%Y"), "%Y")
OLY_PM25_over_time <- OLY_PM25_over_time %>% mutate(month_day = format(Date, "%m/%d"))
OLY_PM25_over_time$month_day <- as.factor(OLY_PM25_over_time$month_day)
OLY_PM25_over_time <- OLY_PM25_over_time %>% mutate(pollutant = "PM2.5")

OLY_PM25_over_time <- OLY_PM25_over_time %>%
  filter(between(month,01,03)) %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

OLY_PM25_over_time$year <- factor(OLY_PM25_over_time$year, levels=c("2021","2020","2019","2018","2017"))
OLY_PM25_over_time <- subset(OLY_PM25_over_time, month_day!="02/29")


### Building Line Graphs ### 
## Missoula Line Graph
MIS_PM25_line <- ggplot(MIS_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("01/01","01/07","01/14","01/21","01/28",
                                        "02/04","02/11","02/18","02/25","03/04",
                                        "03/11","03/18","03/25")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="Missoula") +
  theme(legend.position = "none")

## Chapel Hill Line Graph
CHP_PM25_line <- ggplot(CHP_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("01/01","01/07","01/14","01/21","01/28",
                                        "02/04","02/11","02/18","02/25","03/04",
                                        "03/11","03/18","03/25"))+
  ylab("Daily Mean (ug/m3)") +
  labs(title="Chapel Hill") +
  theme(legend.position = "none")

## Corvallis Line Graph
COR_PM25_line <- ggplot(COR_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("02/01","02/07","02/14","02/21","02/28",
                                        "03/07","03/14","03/21","03/28",
                                        "04/04","04/11","04/18","04/25")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="Corvallis") +
  theme(legend.position = "none")

## Olympia Line Graph
OLY_PM25_line <- ggplot(OLY_PM25_over_time, aes(x=month_day, y=daily_mean, group=year)) +
  geom_line(size=1, aes(color=pre_or_post)) +
  geom_point(size=1.25, aes(color=pre_or_post)) +
  scale_x_discrete(name="Date",breaks=c("01/01","01/07","01/14","01/21","01/28",
                                        "02/04","02/11","02/18","02/25","03/04",
                                        "03/11","03/18","03/25")) +
  ylab("Daily Mean (ug/m3)") +
  labs(title="Olympia") +
  labs(color = "Pre or Post Implementation") +
  theme(legend.position = "bottom")

library(patchwork)
MIS_PM25_line / CHP_PM25_line / COR_PM25_line / OLY_PM25_line + plot_annotation(  
  title = "PM2.5 Concentrations Over Time for Missoula, Chapel Hill, Corvallis, and Olympia",
  #caption = "These plots display average air pollutant concentrations each February over 5 years"
)


  