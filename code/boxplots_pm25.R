here::i_am("code/boxplots_pm25.R")

library(ggplot2)
library(dplyr)

###### PM2.5 BOXPLOTS ######

### SALT LAKE CITY, UT ###

#Subsetting SLC data for pre and post implementation of free fares
SLC_PM25_preandpost <- (pm25_aggregated[["SLC_PM25_2022"]])

#filtering by desired dates and categorizing by pre and post
SLC_PM25_preandpost <- SLC_PM25_preandpost %>% 
  filter(between(Date, as.Date('2022-01-01'), as.Date('2022-02-28'))) %>%
  mutate(pre_or_post = case_when(Date <= as.Date('2022-01-31') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post"))

#adding column with constant city value
SLC_PM25_preandpost["city"] = "SLC"


### ALEXANDRIA, VA ###

#Subsetting Alexandria (ALX) data for pre and post implementation of free fares
ALX_PM25_preandpost <- rbind(pm25_aggregated[["ALX_PM25_2020"]],pm25_aggregated[["ALX_PM25_2021"]],pm25_aggregated[["ALX_PM25_2022"]])

#filtering by desired dates and categorizing by pre and post
ALX_PM25_preandpost <- ALX_PM25_preandpost %>% 
  filter(between(Date, as.Date('2020-09-01'), as.Date('2022-09-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post"))

#adding column with constant city value
ALX_PM25_preandpost["city"] = "ALX"


### ALBUQUERQUE, NM ###

#Subsetting Albuquerque (ALB)) data for pre and post implementation of free fares
ALB_PM25_preandpost <- rbind(pm25_aggregated[["ALB_PM25_2021"]],pm25_aggregated[["ALB_PM25_2022"]])

#filtering by desired dates and categorizing by pre and post
ALB_PM25_preandpost <- ALB_PM25_preandpost %>% 
  mutate(pre_or_post = case_when(Date < as.Date('2022-01-01') ~ "pre", 
                                 Date >= as.Date('2022-01-01') ~ "post"))

#adding column with constant city value
ALB_PM25_preandpost["city"] = "ALB"


### KANSAS CITY, MO ###

#Subsetting Kansas City (KNC) data for pre and post implementation of free fares
KNC_PM25_preandpost <- rbind(pm25_aggregated[["KNC_PM25_2019"]],pm25_aggregated[["KNC_PM25_2020"]],pm25_aggregated[["KNC_PM25_2021"]])

#filtering by desired dates and categorizing by pre and post
KNC_PM25_preandpost <- KNC_PM25_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
KNC_PM25_preandpost["city"] = "KNC"


### RICHMOND, VA ###

#Subsetting Richmond (RCH) data for pre and post implementation of free fares
RCH_PM25_preandpost <- rbind(pm25_aggregated[["RCH_PM25_2019"]],pm25_aggregated[["RCH_PM25_2020"]],pm25_aggregated[["RCH_PM25_2021"]])

#filtering by desired dates and categorizing by pre and post
RCH_PM25_preandpost <- RCH_PM25_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
RCH_PM25_preandpost["city"] = "RCH"


### LOS ANGELES, CA ###

#Subsetting Los Angeles (LAX) data for pre and post implementation of free fares
LAX_PM25_preandpost <- rbind(pm25_aggregated[["LAX_PM25_2019"]],pm25_aggregated[["LAX_PM25_2020"]],pm25_aggregated[["LAX_PM25_2021"]])

#filtering by desired dates and categorizing by pre and post
LAX_PM25_preandpost <- LAX_PM25_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
LAX_PM25_preandpost["city"] = "LAX"


### TUSCON, AZ ###

#Subsetting Tuscon (TUS) data for pre and post implementation of free fares
TUS_PM25_preandpost <- rbind(pm25_aggregated[["TUS_PM25_2019"]],pm25_aggregated[["TUS_PM25_2020"]],pm25_aggregated[["TUS_PM25_2021"]])

#filtering by desired dates and categorizing by pre and post
TUS_PM25_preandpost <- TUS_PM25_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
TUS_PM25_preandpost["city"] = "TUS"


### BOXPLOT CREATION ###

#binding all cities into one dataframe to graph
PM25_preandpost_boxplots <- rbind(SLC_PM25_preandpost,
                                  ALX_PM25_preandpost,
                                  ALB_PM25_preandpost,
                                  KNC_PM25_preandpost,
                                  RCH_PM25_preandpost,
                                  LAX_PM25_preandpost,
                                  TUS_PM25_preandpost)

#ordering pre and post categories so pre comes before post on graph
PM25_preandpost_boxplots$pre_or_post <- factor(PM25_preandpost_boxplots$pre_or_post, levels=c("pre","post"))

#boxplots with all cities
ggplot(PM25_preandpost_boxplots, aes(x=city, y=daily_mean, fill=pre_or_post)) + 
         geom_boxplot()