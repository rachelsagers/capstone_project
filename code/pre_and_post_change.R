here::i_am("code/pre_and_post_change.R")
library(dplyr)
library(reshape2)
library(ggplot2)

# Grouping by city and and pre or post time period and taking mean
aggregate_PM25_change <- PM25_preandpost_boxplots %>%
  group_by(city, pre_or_post) %>%
  summarize(daily_mean = mean(daily_mean))

# reshaping data for graphing
aggregate_PM25_change <- dcast(aggregate_PM25_change, city ~ pre_or_post, value.var="daily_mean")

# creating new column to show change in pollutant concentration
aggregate_PM25_change <- aggregate_PM25_change %>%
  mutate(change = post-pre)

# bar chart with concentration change pre and post implementation
ggplot(aggregate_PM25_change, aes(x=city, y=change, fill=factor(sign(change)))) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("green","red"), guide="none") +
  xlab("City") +
  ylab("Change in PM2.5 Concentration") +
  labs(title="Change in PM2.5 Concentration After Program Implementation")

### NO2 ### 
# Grouping by city and and pre or post time period and taking mean
aggregate_NO2_change <- NO2_preandpost_boxplots %>%
  group_by(city, pre_or_post) %>%
  summarize(daily_mean = mean(daily_mean))

# reshaping data for graphing
aggregate_NO2_change <- dcast(aggregate_NO2_change, city ~ pre_or_post, value.var="daily_mean")

# creating new column to show change in pollutant concentration
aggregate_NO2_change <- aggregate_NO2_change %>%
  mutate(change = post-pre)

# bar chart with concentration change pre and post implementation
ggplot(aggregate_NO2_change, aes(x=city, y=change, fill=factor(sign(change)))) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("green","red"), guide="none") +
  xlab("City") +
  ylab("Change in NO2 Concentration") +
  labs(title="Change in NO2 Concentration After Program Implementation")

### CO ###
# Grouping by city and and pre or post time period and taking mean
aggregate_CO_change <- CO_preandpost_boxplots %>%
  group_by(city, pre_or_post) %>%
  summarize(daily_mean = mean(daily_mean))

# reshaping data for graphing
aggregate_CO_change <- dcast(aggregate_CO_change, city ~ pre_or_post, value.var="daily_mean")

# creating new column to show change in pollutant concentration
aggregate_CO_change <- aggregate_CO_change %>%
  mutate(change = post-pre)

# bar chart with concentration change pre and post implementation
ggplot(aggregate_CO_change, aes(x=city, y=change, fill=factor(sign(change)))) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values = c("green","red"), guide="none") +
  xlab("City") +
  ylab("Change in CO Concentration") +
  labs(title="Change in CO Concentration After Program Implementation")
