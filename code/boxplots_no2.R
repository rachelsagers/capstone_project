here::i_am("code/boxplots_no2.R")

library(ggplot2)
library(dplyr)

###### NO2 BOXPLOTS ######

### SALT LAKE CITY, UT ###

#Subsetting SLC data for pre and post implementation of free fares
SLC_NO2_preandpost <- rbind(no2_aggregated[["SLC_NO2_2022"]],
                           no2_aggregated[["SLC_NO2_2021"]],
                           no2_aggregated[["SLC_NO2_2020"]],
                           no2_aggregated[["SLC_NO2_2019"]],
                           no2_aggregated[["SLC_NO2_2018"]],
                           no2_aggregated[["SLC_NO2_2017"]])

# extracting data into separate year and month column
SLC_NO2_preandpost$month <- format(as.Date(SLC_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
SLC_NO2_preandpost$year <- format(as.Date(SLC_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
SLC_NO2_preandpost <- SLC_NO2_preandpost %>% 
  filter(month == "02") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

#adding column with constant city value
SLC_NO2_preandpost["city"] = "SLC"


### ALEXANDRIA, VA ###

#Subsetting Alexandria (ALX) data for pre and post implementation of free fares
ALX_NO2_preandpost <- rbind(no2_aggregated[["ALX_NO2_2022"]],
                           no2_aggregated[["ALX_NO2_2021"]],
                           no2_aggregated[["ALX_NO2_2020"]],
                           no2_aggregated[["ALX_NO2_2019"]],
                           no2_aggregated[["ALX_NO2_2018"]],
                           no2_aggregated[["ALX_NO2_2017"]],
                           no2_aggregated[["ALX_NO2_2016"]])

# extracting data into separate year and month column
ALX_NO2_preandpost$month <- format(as.Date(ALX_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
ALX_NO2_preandpost$year <- format(as.Date(ALX_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
ALX_NO2_preandpost <- ALX_NO2_preandpost %>% 
  filter(month == "09") %>%
  mutate(pre_or_post = case_when(year >= 2021 ~ "post",
                                 year < 2021 ~ "pre"))

#adding column with constant city value
ALX_NO2_preandpost["city"] = "ALX"


### ALBUQUERQUE, NM ###

#Subsetting Albuquerque (ALB)) data for pre and post implementation of free fares
ALB_NO2_preandpost <- rbind(no2_aggregated[["ALB_NO2_2022"]],
                           no2_aggregated[["ALB_NO2_2021"]],
                           no2_aggregated[["ALB_NO2_2020"]],
                           no2_aggregated[["ALB_NO2_2019"]],
                           no2_aggregated[["ALB_NO2_2018"]],
                           no2_aggregated[["ALB_NO2_2017"]])

# extracting data into separate year and month column
ALB_NO2_preandpost$month <- format(as.Date(ALB_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
ALB_NO2_preandpost$year <- format(as.Date(ALB_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
ALB_NO2_preandpost <- ALB_NO2_preandpost %>% 
  filter(month == "01") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

#adding column with constant city value
ALB_NO2_preandpost["city"] = "ALB"


### KANSAS CITY, MO ###

#Subsetting Kansas City (KNC) data for pre and post implementation of free fares
KNC_NO2_preandpost <- rbind(no2_aggregated[["KNC_NO2_2022"]],
                           no2_aggregated[["KNC_NO2_2021"]],
                           no2_aggregated[["KNC_NO2_2020"]],
                           no2_aggregated[["KNC_NO2_2019"]],
                           no2_aggregated[["KNC_NO2_2018"]],
                           no2_aggregated[["KNC_NO2_2017"]],
                           no2_aggregated[["KNC_NO2_2016"]],
                           no2_aggregated[["KNC_NO2_2015"]])

# extracting data into separate year and month column
KNC_NO2_preandpost$month <- format(as.Date(KNC_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
KNC_NO2_preandpost$year <- format(as.Date(KNC_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
KNC_NO2_preandpost <- KNC_NO2_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
KNC_NO2_preandpost["city"] = "KNC"


### RICHMOND, VA ###

#Subsetting Richmond (RCH) data for pre and post implementation of free fares
RCH_NO2_preandpost <- rbind(no2_aggregated[["RCH_NO2_2022"]],
                           no2_aggregated[["RCH_NO2_2021"]],
                           no2_aggregated[["RCH_NO2_2020"]],
                           no2_aggregated[["RCH_NO2_2019"]],
                           no2_aggregated[["RCH_NO2_2018"]],
                           no2_aggregated[["RCH_NO2_2017"]],
                           no2_aggregated[["RCH_NO2_2016"]],
                           no2_aggregated[["RCH_NO2_2015"]])

# extracting data into separate year and month column
RCH_NO2_preandpost$month <- format(as.Date(RCH_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
RCH_NO2_preandpost$year <- format(as.Date(RCH_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
RCH_NO2_preandpost <- RCH_NO2_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
RCH_NO2_preandpost["city"] = "RCH"


### LOS ANGELES, CA ###

#Subsetting Los Angeles (LAX) data for pre and post implementation of free fares
LAX_NO2_preandpost <- rbind(no2_aggregated[["LAX_NO2_2021"]],
                           no2_aggregated[["LAX_NO2_2020"]],
                           no2_aggregated[["LAX_NO2_2019"]],
                           no2_aggregated[["LAX_NO2_2018"]],
                           no2_aggregated[["LAX_NO2_2017"]],
                           no2_aggregated[["LAX_NO2_2016"]],
                           no2_aggregated[["LAX_NO2_2015"]])

# extracting data into separate year and month column
LAX_NO2_preandpost$month <- format(as.Date(LAX_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
LAX_NO2_preandpost$year <- format(as.Date(LAX_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
LAX_NO2_preandpost <- LAX_NO2_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
LAX_NO2_preandpost["city"] = "LAX"


### TUSNO2N, AZ ###

#Subsetting Tusno2n (TUS) data for pre and post implementation of free fares
TUS_NO2_preandpost <- rbind(no2_aggregated[["TUS_NO2_2022"]],
                           no2_aggregated[["TUS_NO2_2021"]],
                           no2_aggregated[["TUS_NO2_2020"]],
                           no2_aggregated[["TUS_NO2_2019"]],
                           no2_aggregated[["TUS_NO2_2018"]],
                           no2_aggregated[["TUS_NO2_2017"]],
                           no2_aggregated[["TUS_NO2_2016"]],
                           no2_aggregated[["TUS_NO2_2015"]])

# extracting data into separate year and month column
TUS_NO2_preandpost$month <- format(as.Date(TUS_NO2_preandpost$Date, format="%d/%m/%Y"), "%m")
TUS_NO2_preandpost$year <- format(as.Date(TUS_NO2_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
TUS_NO2_preandpost <- TUS_NO2_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
TUS_NO2_preandpost["city"] = "TUS"


### BOXPLOT CREATION ###

#binding all cities into one dataframe to graph
NO2_preandpost_boxplots <- rbind(SLC_NO2_preandpost,
                                ALX_NO2_preandpost,
                                ALB_NO2_preandpost,
                                KNC_NO2_preandpost,
                                RCH_NO2_preandpost,
                                LAX_NO2_preandpost,
                                TUS_NO2_preandpost)

#ordering pre and post categories so pre no2mes before post on graph
NO2_preandpost_boxplots$pre_or_post <- factor(NO2_preandpost_boxplots$pre_or_post, levels=c("pre","post"))

#boxplots with all cities
ggplot(NO2_preandpost_boxplots, aes(x=city, y=daily_mean, fill=pre_or_post)) +
  geom_boxplot() +
  xlab("City") +
  ylab("Daily 1-hr Max Conc (ppb)") +
  labs(fill = "Pre or Post",
       title = "NO2 Concentration Pre and Post Free Transit Implementation") +
  theme(
    legend.position = "bottom"
  )

## Save as 800 w and 500 h