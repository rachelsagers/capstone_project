here::i_am("code/boxplots.R")

library(ggplot2)
library(dplyr)

#Subsetting SLC data for pre and post implementation of free fares
SLC_PM25_preandpost <- (pm25_aggregated[["SLC_PM25_2022"]])

#filtering by desired dates and categorizing by pre and post
SLC_PM25_preandpost <- SLC_PM25_preandpost %>% 
  filter(between(Date, as.Date('2022-01-01'), as.Date('2022-02-28'))) %>%
  mutate(pre_or_post = case_when(Date <= as.Date('2022-01-31') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post"))

#adding column with constant city value
SLC_PM25_preandpost["city"] = "SLC"


#Subsetting Alexandria (ALX) data for pre and post implementation of free fares
ALX_PM25_preandpost <- rbind(pm25_aggregated[["ALX_PM25_2020"]],pm25_aggregated[["ALX_PM25_2021"]],pm25_aggregated[["ALX_PM25_2022"]])

#filtering by desired dates and categorizing by pre and post
ALX_PM25_preandpost <- ALX_PM25_preandpost %>% 
  filter(between(Date, as.Date('2020-09-01'), as.Date('2022-09-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post"))

#adding column with constant city value
ALX_PM25_preandpost["city"] = "ALX"

PM25_preandpost_boxplots <- rbind(SLC_PM25_preandpost,
                                  ALX_PM25_preandpost)

ggplot(PM25_preandpost_boxplots, aes(x=city, y=daily_mean, fill=pre_or_post)) + 
         geom_boxplot()