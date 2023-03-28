here::i_am("code/boxplots_co.R")

library(ggplot2)
library(dplyr)

###### CO BOXPLOTS ######

### SALT LAKE CITY, UT ###

#Subsetting SLC data for pre and post implementation of free fares
SLC_CO_preandpost <- (co_aggregated[["SLC_CO_2022"]])

#filtering by desired dates and categorizing by pre and post
SLC_CO_preandpost <- SLC_CO_preandpost %>% 
  filter(between(Date, as.Date('2022-01-01'), as.Date('2022-02-28'))) %>%
  mutate(pre_or_post = case_when(Date <= as.Date('2022-01-31') ~ "pre", 
                                 Date >= as.Date('2022-02-01') ~ "post"))

#adding column with constant city value
SLC_CO_preandpost["city"] = "SLC"


### ALEXANDRIA, VA ####

#Subsetting Alexandria (ALX) data for pre and post implementation of free fares
ALX_CO_preandpost <- rbind(co_aggregated[["ALX_CO_2020"]],co_aggregated[["ALX_CO_2021"]],co_aggregated[["ALX_CO_2022"]])

#filtering by desired dates and categorizing by pre and post
ALX_CO_preandpost <- ALX_CO_preandpost %>% 
  filter(between(Date, as.Date('2020-09-01'), as.Date('2022-09-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2021-09-01') ~ "pre", 
                                 Date >= as.Date('2021-09-01') ~ "post"))

#adding column with constant city value
ALX_CO_preandpost["city"] = "ALX"


### ALBUQUERQUE, NM ###

#Subsetting Albuquerque (ALB)) data for pre and post implementation of free fares
ALB_CO_preandpost <- rbind(co_aggregated[["ALB_CO_2021"]],co_aggregated[["ALB_CO_2022"]])

#filtering by desired dates and categorizing by pre and post
ALB_CO_preandpost <- ALB_CO_preandpost %>% 
  mutate(pre_or_post = case_when(Date < as.Date('2022-01-01') ~ "pre", 
                                 Date >= as.Date('2022-01-01') ~ "post"))

#adding column with constant city value
ALB_CO_preandpost["city"] = "ALB"


### KANSAS CITY, MO ###

#Subsetting Kansas City (KNC) data for pre and post implementation of free fares
KNC_CO_preandpost <- rbind(co_aggregated[["KNC_CO_2019"]],co_aggregated[["KNC_CO_2020"]],co_aggregated[["KNC_CO_2021"]])

#filtering by desired dates and categorizing by pre and post
KNC_CO_preandpost <- KNC_CO_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
KNC_CO_preandpost["city"] = "KNC"


### RICHMOND, VA ###

#Subsetting Richmond (RCH) data for pre and post implementation of free fares
RCH_CO_preandpost <- rbind(co_aggregated[["RCH_CO_2019"]],co_aggregated[["RCH_CO_2020"]],co_aggregated[["RCH_CO_2021"]])

#filtering by desired dates and categorizing by pre and post
RCH_CO_preandpost <- RCH_CO_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
RCH_CO_preandpost["city"] = "RCH"

### LOS ANGELES, CA ###

#Subsetting Los Angeles (LAX) data for pre and post implementation of free fares
LAX_CO_preandpost <- rbind(co_aggregated[["LAX_CO_2019"]],co_aggregated[["LAX_CO_2020"]],co_aggregated[["LAX_CO_2021"]])

#filtering by desired dates and categorizing by pre and post
LAX_CO_preandpost <- LAX_CO_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
LAX_CO_preandpost["city"] = "LAX"


### TUSCON, AZ ###

#Subsetting Tuscon (TUS) data for pre and post implementation of free fares
TUS_CO_preandpost <- rbind(co_aggregated[["TUS_CO_2019"]],co_aggregated[["TUS_CO_2020"]],co_aggregated[["TUS_CO_2021"]])

#filtering by desired dates and categorizing by pre and post
TUS_CO_preandpost <- TUS_CO_preandpost %>% 
  filter(between(Date, as.Date('2019-03-01'), as.Date('2021-03-01'))) %>%
  mutate(pre_or_post = case_when(Date < as.Date('2020-03-01') ~ "pre", 
                                 Date >= as.Date('2020-03-01') ~ "post"))

#adding column with constant city value
TUS_CO_preandpost["city"] = "TUS"

### BOXPLOT CREATION ###

#binding all cities into one dataframe to graph
CO_preandpost_boxplots <- rbind(SLC_CO_preandpost,
                                ALX_CO_preandpost,
                                ALB_CO_preandpost,
                                KNC_CO_preandpost,
                                RCH_CO_preandpost,
                                LAX_CO_preandpost,
                                TUS_CO_preandpost)

#ordering pre and post categories so pre comes before post on graph
CO_preandpost_boxplots$pre_or_post <- factor(CO_preandpost_boxplots$pre_or_post, levels=c("pre","post"))

#CO boxplots with all cities
ggplot(CO_preandpost_boxplots, aes(x=city, y=daily_mean, fill=pre_or_post)) + 
  geom_boxplot() +
  xlab("City") +
  ylab("Mean Concentration") +
  labs(fill = "Pre or Post",
       title = "Mean CO Concentration Pre and Post Free Transit Implementation") +
  theme(
    legend.position = "bottom"
  )
