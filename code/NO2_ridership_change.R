here::i_am("code/NO2_ridership_change.R")

library(dplyr)
library(reshape2)

### NO2 change ###

# Los Angeles Mar 2020 (Dec-Feb and Mar-May)
LAX_NO2_change <- rbind(no2_aggregated[["LAX_NO2_2019"]],
                        no2_aggregated[["LAX_NO2_2020"]])

# filtering by desired dates and categorizing by pre and post
LAX_NO2_change <- LAX_NO2_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
LAX_NO2_change["city"] = "LAX"

# reshaping data
LAX_NO2_change <- dcast(LAX_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
LAX_NO2_change <- LAX_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))


### Albuquerque Jan 2022 (Oct-Dec and Jan-Mar) ###
ALB_NO2_change <- rbind(no2_aggregated[["ALB_NO2_2021"]],
                         no2_aggregated[["ALB_NO2_2022"]])

# filtering by desired dates and categorizing by pre and post
ALB_NO2_change <- ALB_NO2_change %>% 
  filter(between(Date, as.Date('2021-10-01'), as.Date('2022-03-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2022-01-01') ~ "pre", 
                                 Date >= as.Date('2022-01-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
ALB_NO2_change["city"] = "ALB"

# reshaping data
ALB_NO2_change <- dcast(ALB_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
ALB_NO2_change <- ALB_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))


# Tuscon Mar 2020 (Dec-Feb and Mar-May)
TUS_NO2_change <- rbind(no2_aggregated[["TUS_NO2_2019"]],
                         no2_aggregated[["TUS_NO2_2020"]])

# filtering by desired dates and categorizing by pre and post
TUS_NO2_change <- TUS_NO2_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
TUS_NO2_change["city"] = "TUS"

# reshaping data
TUS_NO2_change <- dcast(TUS_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
TUS_NO2_change <- TUS_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))


# Kansas City Mar 2020 (Dec-Feb and Mar-May)
KNC_NO2_change <- rbind(no2_aggregated[["KNC_NO2_2019"]],
                         no2_aggregated[["KNC_NO2_2020"]])

# filtering by desired dates and categorizing by pre and post
KNC_NO2_change <- KNC_NO2_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
KNC_NO2_change["city"] = "KNC"

# reshaping data
KNC_NO2_change <- dcast(KNC_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
KNC_NO2_change <- KNC_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))


# Richmond Mar 2020 (Dec-Feb and Mar-May)
RCH_NO2_change <- rbind(no2_aggregated[["RCH_NO2_2019"]],
                         no2_aggregated[["RCH_NO2_2020"]])

# filtering by desired dates and categorizing by pre and post
RCH_NO2_change <- RCH_NO2_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
RCH_NO2_change["city"] = "RCH"

# reshaping data
RCH_NO2_change <- dcast(RCH_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
RCH_NO2_change <- RCH_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))


# Salt Lake City Feb 2022 (Nov-Jan and Feb-Apr)
SLC_NO2_change <- rbind(no2_aggregated[["SLC_NO2_2022"]],
                         no2_aggregated[["SLC_NO2_2021"]])

# filtering by desired dates and categorizing by pre and post
SLC_NO2_change <- SLC_NO2_change %>% 
  filter(between(Date, as.Date('2021-11-01'), as.Date('2022-04-30'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2022-02-01') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
SLC_NO2_change["city"] = "SLC"

# reshaping data
SLC_NO2_change <- dcast(SLC_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
SLC_NO2_change <- SLC_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))


# Alexandria Sept 2021 (Jun-Aug and Sept-Nov)
ALX_NO2_change <- rbind(no2_aggregated[["ALX_NO2_2021"]])

# filtering by desired dates and categorizing by pre and post
ALX_NO2_change <- ALX_NO2_change %>% 
  filter(between(Date, as.Date('2021-06-01'), as.Date('2021-11-30'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
ALX_NO2_change["city"] = "ALX"

# reshaping data
ALX_NO2_change <- dcast(ALX_NO2_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
ALX_NO2_change <- ALX_NO2_change %>%
  mutate(NO2_change = (post-pre)/(pre))

agg_NO2_change <- rbind(LAX_NO2_change,
                         ALB_NO2_change,
                         TUS_NO2_change,
                         KNC_NO2_change,
                         RCH_NO2_change,
                         SLC_NO2_change,
                         ALX_NO2_change)