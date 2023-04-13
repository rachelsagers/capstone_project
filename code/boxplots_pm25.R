here::i_am("code/boxplots_pm25.R")

library(ggplot2)
library(dplyr)

###### PM2.5 BOXPLOTS ######

### SALT LAKE CITY, UT ###

#Subsetting SLC data for pre and post implementation of free fares
SLC_PM25_preandpost <- rbind(pm25_aggregated[["SLC_PM25_2022"]],
                             pm25_aggregated[["SLC_PM25_2021"]],
                             pm25_aggregated[["SLC_PM25_2020"]],
                             pm25_aggregated[["SLC_PM25_2019"]],
                             pm25_aggregated[["SLC_PM25_2018"]],
                             pm25_aggregated[["SLC_PM25_2017"]])

# extracting data into separate year and month column
SLC_PM25_preandpost$month <- format(as.Date(SLC_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
SLC_PM25_preandpost$year <- format(as.Date(SLC_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
SLC_PM25_preandpost <- SLC_PM25_preandpost %>% 
  filter(month == "02") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

#SLC_PM25_over_time$year <- factor(SLC_PM25_over_time$year, levels=c("2022","2021","2020","2019","2018","2017"))

#adding column with constant city value
SLC_PM25_preandpost["city"] = "SLC"


### ALEXANDRIA, VA ###

#Subsetting Alexandria (ALX) data for pre and post implementation of free fares
ALX_PM25_preandpost <- rbind(pm25_aggregated[["ALX_PM25_2022"]],
                             pm25_aggregated[["ALX_PM25_2021"]],
                             pm25_aggregated[["ALX_PM25_2020"]],
                             pm25_aggregated[["ALX_PM25_2019"]],
                             pm25_aggregated[["ALX_PM25_2018"]],
                             pm25_aggregated[["ALX_PM25_2017"]],
                             pm25_aggregated[["ALX_PM25_2016"]])

# extracting data into separate year and month column
ALX_PM25_preandpost$month <- format(as.Date(ALX_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
ALX_PM25_preandpost$year <- format(as.Date(ALX_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
ALX_PM25_preandpost <- ALX_PM25_preandpost %>% 
  filter(month == "09") %>%
  mutate(pre_or_post = case_when(year >= 2021 ~ "post",
                                 year < 2021 ~ "pre"))

#adding column with constant city value
ALX_PM25_preandpost["city"] = "ALX"


### ALBUQUERQUE, NM ###

#Subsetting Albuquerque (ALB)) data for pre and post implementation of free fares
ALB_PM25_preandpost <- rbind(pm25_aggregated[["ALB_PM25_2022"]],
                             pm25_aggregated[["ALB_PM25_2021"]],
                             pm25_aggregated[["ALB_PM25_2020"]],
                             pm25_aggregated[["ALB_PM25_2019"]],
                             pm25_aggregated[["ALB_PM25_2018"]],
                             pm25_aggregated[["ALB_PM25_2017"]])

# extracting data into separate year and month column
ALB_PM25_preandpost$month <- format(as.Date(ALB_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
ALB_PM25_preandpost$year <- format(as.Date(ALB_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
ALB_PM25_preandpost <- ALB_PM25_preandpost %>% 
  filter(month == "01") %>%
  mutate(pre_or_post = case_when(year == 2022 ~ "post",
                                 year < 2022 ~ "pre"))

#adding column with constant city value
ALB_PM25_preandpost["city"] = "ALB"


### KANSAS CITY, MO ###

#Subsetting Kansas City (KNC) data for pre and post implementation of free fares
KNC_PM25_preandpost <- rbind(pm25_aggregated[["KNC_PM25_2022"]],
                             pm25_aggregated[["KNC_PM25_2021"]],
                             pm25_aggregated[["KNC_PM25_2020"]],
                             pm25_aggregated[["KNC_PM25_2019"]],
                             pm25_aggregated[["KNC_PM25_2018"]],
                             pm25_aggregated[["KNC_PM25_2017"]],
                             pm25_aggregated[["KNC_PM25_2016"]],
                             pm25_aggregated[["KNC_PM25_2015"]])

# extracting data into separate year and month column
KNC_PM25_preandpost$month <- format(as.Date(KNC_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
KNC_PM25_preandpost$year <- format(as.Date(KNC_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
KNC_PM25_preandpost <- KNC_PM25_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
KNC_PM25_preandpost["city"] = "KNC"


### RICHMOND, VA ###

#Subsetting Richmond (RCH) data for pre and post implementation of free fares
RCH_PM25_preandpost <- rbind(pm25_aggregated[["RCH_PM25_2022"]],
                             pm25_aggregated[["RCH_PM25_2021"]],
                             pm25_aggregated[["RCH_PM25_2020"]],
                             pm25_aggregated[["RCH_PM25_2019"]],
                             pm25_aggregated[["RCH_PM25_2018"]],
                             pm25_aggregated[["RCH_PM25_2017"]],
                             pm25_aggregated[["RCH_PM25_2016"]],
                             pm25_aggregated[["RCH_PM25_2015"]])

# extracting data into separate year and month column
RCH_PM25_preandpost$month <- format(as.Date(RCH_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
RCH_PM25_preandpost$year <- format(as.Date(RCH_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
RCH_PM25_preandpost <- RCH_PM25_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
RCH_PM25_preandpost["city"] = "RCH"


### LOS ANGELES, CA ###

#Subsetting Los Angeles (LAX) data for pre and post implementation of free fares
LAX_PM25_preandpost <- rbind(pm25_aggregated[["LAX_PM25_2021"]],
                             pm25_aggregated[["LAX_PM25_2020"]],
                             pm25_aggregated[["LAX_PM25_2019"]],
                             pm25_aggregated[["LAX_PM25_2018"]],
                             pm25_aggregated[["LAX_PM25_2017"]],
                             pm25_aggregated[["LAX_PM25_2016"]],
                             pm25_aggregated[["LAX_PM25_2015"]])

# extracting data into separate year and month column
LAX_PM25_preandpost$month <- format(as.Date(LAX_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
LAX_PM25_preandpost$year <- format(as.Date(LAX_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
LAX_PM25_preandpost <- LAX_PM25_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
LAX_PM25_preandpost["city"] = "LAX"


### TUSCON, AZ ###

#Subsetting Tuscon (TUS) data for pre and post implementation of free fares
TUS_PM25_preandpost <- rbind(pm25_aggregated[["TUS_PM25_2022"]],
                             pm25_aggregated[["TUS_PM25_2021"]],
                             pm25_aggregated[["TUS_PM25_2020"]],
                             pm25_aggregated[["TUS_PM25_2019"]],
                             pm25_aggregated[["TUS_PM25_2018"]],
                             pm25_aggregated[["TUS_PM25_2017"]],
                             pm25_aggregated[["TUS_PM25_2016"]],
                             pm25_aggregated[["TUS_PM25_2015"]])

# extracting data into separate year and month column
TUS_PM25_preandpost$month <- format(as.Date(TUS_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
TUS_PM25_preandpost$year <- format(as.Date(TUS_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
TUS_PM25_preandpost <- TUS_PM25_preandpost %>% 
  filter(month == "03") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
TUS_PM25_preandpost["city"] = "TUS"


### MISSOULA, MT ###

#Subsetting Missoula (MIS) data for pre and post implementation of free fares
MIS_PM25_preandpost <- rbind(pm25_aggregated[["MIS_PM25_2017"]],
                             pm25_aggregated[["MIS_PM25_2016"]],
                             pm25_aggregated[["MIS_PM25_2015"]],
                             pm25_aggregated[["MIS_PM25_2014"]],
                             pm25_aggregated[["MIS_PM25_2013"]],
                             pm25_aggregated[["MIS_PM25_2012"]],
                             pm25_aggregated[["MIS_PM25_2011"]],
                             pm25_aggregated[["MIS_PM25_2010"]])


# extracting data into separate year and month column
MIS_PM25_preandpost$month <- format(as.Date(MIS_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
MIS_PM25_preandpost$year <- format(as.Date(MIS_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
MIS_PM25_preandpost <- MIS_PM25_preandpost %>% 
  filter(month == "01") %>%
  mutate(pre_or_post = case_when(year >= 2015 ~ "post",
                                 year < 2015 ~ "pre"))

#adding column with constant city value
MIS_PM25_preandpost["city"] = "MIS"


### OLYMPIA, WA ###

#Subsetting Olympia (OLY) data for pre and post implementation of free fares
OLY_PM25_preandpost <- rbind(pm25_aggregated[["OLY_PM25_2022"]],
                             pm25_aggregated[["OLY_PM25_2021"]],
                             pm25_aggregated[["OLY_PM25_2020"]],
                             pm25_aggregated[["OLY_PM25_2019"]],
                             pm25_aggregated[["OLY_PM25_2018"]],
                             pm25_aggregated[["OLY_PM25_2017"]],
                             pm25_aggregated[["OLY_PM25_2016"]],
                             pm25_aggregated[["OLY_PM25_2015"]])

# extracting data into separate year and month column
OLY_PM25_preandpost$month <- format(as.Date(OLY_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
OLY_PM25_preandpost$year <- format(as.Date(OLY_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
OLY_PM25_preandpost <- OLY_PM25_preandpost %>% 
  filter(month == "01") %>%
  mutate(pre_or_post = case_when(year >= 2020 ~ "post",
                                 year < 2020 ~ "pre"))

#adding column with constant city value
OLY_PM25_preandpost["city"] = "OLY"


### CORVALLIS, OR ###

#Subsetting Corvallis (COR) data for pre and post implementation of free fares
COR_PM25_preandpost <- rbind(pm25_aggregated[["COR_PM25_2013"]],
                             pm25_aggregated[["COR_PM25_2012"]],
                             pm25_aggregated[["COR_PM25_2011"]],
                             pm25_aggregated[["COR_PM25_2010"]],
                             pm25_aggregated[["COR_PM25_2009"]],
                             pm25_aggregated[["COR_PM25_2008"]],
                             pm25_aggregated[["COR_PM25_2007"]],
                             pm25_aggregated[["COR_PM25_2006"]])

# extracting data into separate year and month column
COR_PM25_preandpost$month <- format(as.Date(COR_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
COR_PM25_preandpost$year <- format(as.Date(COR_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
COR_PM25_preandpost <- COR_PM25_preandpost %>% 
  filter(month == "02") %>%
  mutate(pre_or_post = case_when(year >= 2011 ~ "post",
                                 year < 2011 ~ "pre"))

#adding column with constant city value
COR_PM25_preandpost["city"] = "COR"


### CHAPEL HILL, NC ###

#Subsetting Chapel Hill (CHP) data for pre and post implementation of free fares
CHP_PM25_preandpost <- rbind(pm25_aggregated[["CHP_PM25_2004"]],
                             pm25_aggregated[["CHP_PM25_2003"]],
                             pm25_aggregated[["CHP_PM25_2002"]],
                             pm25_aggregated[["CHP_PM25_2001"]],
                             pm25_aggregated[["CHP_PM25_2000"]],
                             pm25_aggregated[["CHP_PM25_1999"]])

# extracting data into separate year and month column
CHP_PM25_preandpost$month <- format(as.Date(CHP_PM25_preandpost$Date, format="%d/%m/%Y"), "%m")
CHP_PM25_preandpost$year <- format(as.Date(CHP_PM25_preandpost$Date, format="%d/%m/%Y"), "%Y")

#filtering by desired dates and categorizing by pre and post
CHP_PM25_preandpost <- CHP_PM25_preandpost %>% 
  filter(month == "01") %>%
  mutate(pre_or_post = case_when(year >= 2002 ~ "post",
                                 year < 2002 ~ "pre"))

#adding column with constant city value
CHP_PM25_preandpost["city"] = "CHP"


### BOXPLOT CREATION ###

#binding all cities into one dataframe to graph
PM25_preandpost_boxplots <- rbind(SLC_PM25_preandpost,
                                  ALX_PM25_preandpost,
                                  ALB_PM25_preandpost,
                                  KNC_PM25_preandpost,
                                  RCH_PM25_preandpost,
                                  LAX_PM25_preandpost,
                                  TUS_PM25_preandpost,
                                  MIS_PM25_preandpost,
                                  OLY_PM25_preandpost,
                                  COR_PM25_preandpost,
                                  CHP_PM25_preandpost)

#ordering pre and post categories so pre comes before post on graph
PM25_preandpost_boxplots$pre_or_post <- factor(PM25_preandpost_boxplots$pre_or_post, levels=c("pre","post"))

#boxplots with all cities
ggplot(PM25_preandpost_boxplots, aes(x=city, y=daily_mean, fill=pre_or_post)) +
  geom_boxplot() +
  xlab("City") +
  ylab("Mean Concentration (ug/m3") +
  labs(fill = "Pre or Post",
       title = "Mean PM2.5 Concentration Pre and Post Free Transit Implementation") +
  theme(
    legend.position = "bottom"
  )
          
## Save as 800 w and 500 h