here::i_am("code/PM25_ridership_change.R")

library(dplyr)
library(reshape2)

### PM2.5 change ###

# Los Angeles Mar 2020 (Dec-Feb and Mar-May)
LAX_PM25_change <- rbind(pm25_aggregated[["LAX_PM25_2019"]],
                         pm25_aggregated[["LAX_PM25_2020"]])

# filtering by desired dates and categorizing by pre and post
LAX_PM25_change <- LAX_PM25_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
LAX_PM25_change["city"] = "LAX"

# reshaping data
LAX_PM25_change <- dcast(LAX_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
LAX_PM25_change <- LAX_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))


### Albuquerque Jan 2022 (Oct-Dec and Jan-Mar) ###
ALB_PM25_change <- rbind(pm25_aggregated[["ALB_PM25_2021"]],
                         pm25_aggregated[["ALB_PM25_2022"]])

# filtering by desired dates and categorizing by pre and post
ALB_PM25_change <- ALB_PM25_change %>% 
  filter(between(Date, as.Date('2021-10-01'), as.Date('2022-03-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2022-01-01') ~ "pre", 
                                 Date >= as.Date('2022-01-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
ALB_PM25_change["city"] = "ALB"

# reshaping data
ALB_PM25_change <- dcast(ALB_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
ALB_PM25_change <- ALB_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))


# Tuscon Mar 2020 (Dec-Feb and Mar-May)
TUS_PM25_change <- rbind(pm25_aggregated[["TUS_PM25_2019"]],
                         pm25_aggregated[["TUS_PM25_2020"]])

# filtering by desired dates and categorizing by pre and post
TUS_PM25_change <- TUS_PM25_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
TUS_PM25_change["city"] = "TUS"

# reshaping data
TUS_PM25_change <- dcast(TUS_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
TUS_PM25_change <- TUS_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))


# Kansas City Mar 2020 (Dec-Feb and Mar-May)
KNC_PM25_change <- rbind(pm25_aggregated[["KNC_PM25_2019"]],
                         pm25_aggregated[["KNC_PM25_2020"]])

# filtering by desired dates and categorizing by pre and post
KNC_PM25_change <- KNC_PM25_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
KNC_PM25_change["city"] = "KNC"

# reshaping data
KNC_PM25_change <- dcast(KNC_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
KNC_PM25_change <- KNC_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))


# Richmond Mar 2020 (Dec-Feb and Mar-May)
RCH_PM25_change <- rbind(pm25_aggregated[["RCH_PM25_2019"]],
                         pm25_aggregated[["RCH_PM25_2020"]])

# filtering by desired dates and categorizing by pre and post
RCH_PM25_change <- RCH_PM25_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
RCH_PM25_change["city"] = "RCH"

# reshaping data
RCH_PM25_change <- dcast(RCH_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
RCH_PM25_change <- RCH_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))


# Salt Lake City Feb 2022 (Nov-Jan and Feb-Apr)
SLC_PM25_change <- rbind(pm25_aggregated[["SLC_PM25_2022"]],
                         pm25_aggregated[["SLC_PM25_2021"]])

# filtering by desired dates and categorizing by pre and post
SLC_PM25_change <- SLC_PM25_change %>% 
  filter(between(Date, as.Date('2021-11-01'), as.Date('2022-04-30'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2022-02-01') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
SLC_PM25_change["city"] = "SLC"

# reshaping data
SLC_PM25_change <- dcast(SLC_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
SLC_PM25_change <- SLC_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))


# Alexandria Sept 2021 (Jun-Aug and Sept-Nov)
ALX_PM25_change <- rbind(pm25_aggregated[["ALX_PM25_2021"]])

# filtering by desired dates and categorizing by pre and post
ALX_PM25_change <- ALX_PM25_change %>% 
  filter(between(Date, as.Date('2021-06-01'), as.Date('2021-11-30'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
ALX_PM25_change["city"] = "ALX"

# reshaping data
ALX_PM25_change <- dcast(ALX_PM25_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
ALX_PM25_change <- ALX_PM25_change %>%
  mutate(PM25_change = (post-pre)/(pre))

agg_PM25_change <- rbind(LAX_PM25_change,
                         ALB_PM25_change,
                         TUS_PM25_change,
                         KNC_PM25_change,
                         RCH_PM25_change,
                         SLC_PM25_change,
                         ALX_PM25_change)