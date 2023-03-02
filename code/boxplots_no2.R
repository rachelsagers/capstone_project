here::i_am("code/boxplots_no2.R")

library(ggplot2)
library(dplyr)

###### NO2 BOXPLOTS ######

### SALT LAKE CITY, UT ###

#Subsetting SLC data for pre and post implementation of free fares
SLC_NO2_preandpost <- (no2_aggregated[["SLC_NO2_2022"]])

#filtering by desired dates and categorizing by pre and post
SLC_NO2_preandpost <- SLC_NO2_preandpost %>% 
  filter(between(Date, as.Date('2022-01-01'), as.Date('2022-02-28'))) %>%
  mutate(pre_or_post = case_when(Date <= as.Date('2022-01-31') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post"))

#adding column with constant city value
SLC_NO2_preandpost["city"] = "SLC"

### ALEXANDRIA, VA ###

#Subsetting Alexandria (ALX) data for pre and post implementation of free fares
ALX_NO2_preandpost <- rbind(no2_aggregated[["ALX_NO2_2020"]],no2_aggregated[["ALX_NO2_2021"]],no2_aggregated[["ALX_NO2_2022"]])

#filtering by desired dates and categorizing by pre and post
ALX_NO2_preandpost <- ALX_NO2_preandpost %>% 
  filter(between(Date, as.Date('2020-09-01'), as.Date('2022-09-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post"))

#adding column with constant city value
ALX_NO2_preandpost["city"] = "ALX"

### ALBUQUERQUE, NM ###

#Subsetting Albuquerque (ALB)) data for pre and post implementation of free fares
ALB_NO2_preandpost <- rbind(no2_aggregated[["ALB_NO2_2021"]],no2_aggregated[["ALB_NO2_2022"]])

#filtering by desired dates and categorizing by pre and post
ALB_NO2_preandpost <- ALB_NO2_preandpost %>% 
  mutate(pre_or_post = case_when(Date < as.Date('2022-01-01') ~ "pre", 
                                 Date >= as.Date('2022-01-01') ~ "post"))

#adding column with constant city value
ALB_NO2_preandpost["city"] = "ALB"

### KANSAS CITY, MO ###

#Subsetting Kansas City (KNC) data for pre and post implementation of free fares
KNC_NO2_preandpost <- rbind(no2_aggregated[["KNC_NO2_2019"]],no2_aggregated[["KNC_NO2_2020"]],no2_aggregated[["KNC_NO2_2021"]])

#filtering by desired dates and categorizing by pre and post
KNC_NO2_preandpost <- KNC_NO2_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
KNC_NO2_preandpost["city"] = "KNC"

### BOXPLOT CREATION ###

#binding all cities into one dataframe to graph
NO2_preandpost_boxplots <- rbind(SLC_NO2_preandpost,
                                 ALX_NO2_preandpost,
                                 ALB_NO2_preandpost,
                                 KNC_NO2_preandpost)

#ordering pre and post categories so pre comes before post on graph
NO2_preandpost_boxplots$pre_or_post <- factor(NO2_preandpost_boxplots$pre_or_post, levels=c("pre","post"))

#NO2 boxplots with all cities
ggplot(NO2_preandpost_boxplots, aes(x=city, y=daily_mean, fill=pre_or_post)) + 
  geom_boxplot()