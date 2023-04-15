here::i_am("code/CO_ridership_change.R")

library(dplyr)
library(reshape2)

### CO change ###

# Los Angeles Mar 2020 (Dec-Feb and Mar-May)
LAX_CO_change <- rbind(co_aggregated[["LAX_CO_2019"]],
                       co_aggregated[["LAX_CO_2020"]])

# filtering by desired dates and categorizing by pre and post
LAX_CO_change <- LAX_CO_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
LAX_CO_change["city"] = "LAX"

# reshaping data
LAX_CO_change <- dcast(LAX_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
LAX_CO_change <- LAX_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))


### Albuquerque Jan 2022 (Oct-Dec and Jan-Mar) ###
ALB_CO_change <- rbind(co_aggregated[["ALB_CO_2021"]],
                         co_aggregated[["ALB_CO_2022"]])

# filtering by desired dates and categorizing by pre and post
ALB_CO_change <- ALB_CO_change %>% 
  filter(between(Date, as.Date('2021-10-01'), as.Date('2022-03-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2022-01-01') ~ "pre", 
                                 Date >= as.Date('2022-01-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
ALB_CO_change["city"] = "ALB"

# reshaping data
ALB_CO_change <- dcast(ALB_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
ALB_CO_change <- ALB_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))


# Tuscon Mar 2020 (Dec-Feb and Mar-May)
TUS_CO_change <- rbind(co_aggregated[["TUS_CO_2019"]],
                         co_aggregated[["TUS_CO_2020"]])

# filtering by desired dates and categorizing by pre and post
TUS_CO_change <- TUS_CO_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
TUS_CO_change["city"] = "TUS"

# reshaping data
TUS_CO_change <- dcast(TUS_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
TUS_CO_change <- TUS_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))


# Kansas City Mar 2020 (Dec-Feb and Mar-May)
KNC_CO_change <- rbind(co_aggregated[["KNC_CO_2019"]],
                         co_aggregated[["KNC_CO_2020"]])

# filtering by desired dates and categorizing by pre and post
KNC_CO_change <- KNC_CO_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
KNC_CO_change["city"] = "KNC"

# reshaping data
KNC_CO_change <- dcast(KNC_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
KNC_CO_change <- KNC_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))


# Richmond Mar 2020 (Dec-Feb and Mar-May)
RCH_CO_change <- rbind(co_aggregated[["RCH_CO_2019"]],
                         co_aggregated[["RCH_CO_2020"]])

# filtering by desired dates and categorizing by pre and post
RCH_CO_change <- RCH_CO_change %>% 
  filter(between(Date, as.Date('2019-12-01'), as.Date('2020-05-31'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
RCH_CO_change["city"] = "RCH"

# reshaping data
RCH_CO_change <- dcast(RCH_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
RCH_CO_change <- RCH_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))


# Salt Lake City Feb 2022 (Nov-Jan and Feb-Apr)
SLC_CO_change <- rbind(co_aggregated[["SLC_CO_2022"]],
                         co_aggregated[["SLC_CO_2021"]])

# filtering by desired dates and categorizing by pre and post
SLC_CO_change <- SLC_CO_change %>% 
  filter(between(Date, as.Date('2021-11-01'), as.Date('2022-04-30'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2022-02-01') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
SLC_CO_change["city"] = "SLC"

# reshaping data
SLC_CO_change <- dcast(SLC_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
SLC_CO_change <- SLC_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))


# Alexandria Sept 2021 (Jun-Aug and Sept-Nov)
ALX_CO_change <- rbind(co_aggregated[["ALX_CO_2021"]])

# filtering by desired dates and categorizing by pre and post
ALX_CO_change <- ALX_CO_change %>% 
  filter(between(Date, as.Date('2021-06-01'), as.Date('2021-11-30'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post")) %>%
  group_by(pre_or_post) %>%
  summarize(quarterly_mean = mean(daily_mean))

# adding column with constant city value
ALX_CO_change["city"] = "ALX"

# reshaping data
ALX_CO_change <- dcast(ALX_CO_change, city ~ pre_or_post, value.var="quarterly_mean")

# creating new column to show change in pollutant concentration
ALX_CO_change <- ALX_CO_change %>%
  mutate(CO_change = (post-pre)/(pre))

agg_CO_change <- rbind(LAX_CO_change,
                         ALB_CO_change,
                         TUS_CO_change,
                         KNC_CO_change,
                         RCH_CO_change,
                         SLC_CO_change,
                         ALX_CO_change)