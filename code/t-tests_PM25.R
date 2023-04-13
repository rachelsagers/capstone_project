here::i_am("code/t-tests_pm25.R")
library(devtools)
library(broom)
library(dplyr)

# paired t test sample code
t.test(x, y, paired = TRUE, alternative = "two.sided")
# choices for alternative are two.sided, greater, or less - which one?? 
# LESS (difference should be negative between t0 and t1, 2, etc if there was improvement)

#tidy data for t-test
tt <- t.test(....etc)
tidy(tt)

### Los Angeles ###

#splitting data into separate data frames for t-tests
LAX_t0_PM25 <- LAX_PM25_preandpost %>% filter(year == "2020")
LAX_t1_PM25 <- LAX_PM25_preandpost %>% filter(year == "2019")
LAX_t2_PM25 <- LAX_PM25_preandpost %>% filter(year == "2018")
LAX_t3_PM25 <- LAX_PM25_preandpost %>% filter(year == "2017")
LAX_t4_PM25 <- LAX_PM25_preandpost %>% filter(year == "2016")
LAX_t5_PM25 <- LAX_PM25_preandpost %>% filter(year == "2015")

mean(LAX_t2_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(LAX_t0_PM25$daily_mean, LAX_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
#t.test(LAX_t0_PM25$daily_mean, LAX_t1_PM25$daily_mean, paired = TRUE, alternative = "two.sided")
t.test(LAX_t0_PM25$daily_mean, LAX_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(LAX_t0_PM25$daily_mean, LAX_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(LAX_t0_PM25$daily_mean, LAX_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(LAX_t0_PM25$daily_mean, LAX_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Albuquerque ###

#splitting data into separate data frames for t-tests
ALB_t0_PM25 <- ALB_PM25_preandpost %>% filter(year == "2022")
ALB_t1_PM25 <- ALB_PM25_preandpost %>% filter(year == "2021")
ALB_t2_PM25 <- ALB_PM25_preandpost %>% filter(year == "2020")
ALB_t3_PM25 <- ALB_PM25_preandpost %>% filter(year == "2019")
ALB_t4_PM25 <- ALB_PM25_preandpost %>% filter(year == "2018")
ALB_t5_PM25 <- ALB_PM25_preandpost %>% filter(year == "2017")

mean(ALB_t2_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(ALB_t0_PM25$daily_mean, ALB_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALB_t0_PM25$daily_mean, ALB_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALB_t0_PM25$daily_mean, ALB_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALB_t0_PM25$daily_mean, ALB_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALB_t0_PM25$daily_mean, ALB_t5_PM25$daily_mean, paired = TRUE, alternative = "less")

### Tuscon ###

#splitting data into separate data frames for t-tests
TUS_t0_PM25 <- TUS_PM25_preandpost %>% filter(year == "2020")
TUS_t1_PM25 <- TUS_PM25_preandpost %>% filter(year == "2019")
TUS_t2_PM25 <- TUS_PM25_preandpost %>% filter(year == "2018")
TUS_t3_PM25 <- TUS_PM25_preandpost %>% filter(year == "2017")
TUS_t4_PM25 <- TUS_PM25_preandpost %>% filter(year == "2016")
TUS_t5_PM25 <- TUS_PM25_preandpost %>% filter(year == "2015")

mean(TUS_t0_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(TUS_t0_PM25$daily_mean, TUS_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(TUS_t0_PM25$daily_mean, TUS_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(TUS_t0_PM25$daily_mean, TUS_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(TUS_t0_PM25$daily_mean, TUS_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(TUS_t0_PM25$daily_mean, TUS_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Kansas City ###

#splitting data into separate data frames for t-tests
KNC_t0_PM25 <- KNC_PM25_preandpost %>% filter(year == "2020")
KNC_t1_PM25 <- KNC_PM25_preandpost %>% filter(year == "2019")
KNC_t2_PM25 <- KNC_PM25_preandpost %>% filter(year == "2018")
KNC_t3_PM25 <- KNC_PM25_preandpost %>% filter(year == "2017")
KNC_t4_PM25 <- KNC_PM25_preandpost %>% filter(year == "2016")
KNC_t5_PM25 <- KNC_PM25_preandpost %>% filter(year == "2015")

mean(KNC_t0_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(KNC_t0_PM25$daily_mean, KNC_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(KNC_t0_PM25$daily_mean, KNC_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(KNC_t0_PM25$daily_mean, KNC_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(KNC_t0_PM25$daily_mean, KNC_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(KNC_t0_PM25$daily_mean, KNC_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Richmond ###

#splitting data into separate data frames for t-tests
RCH_t0_PM25 <- RCH_PM25_preandpost %>% filter(year == "2020")
RCH_t1_PM25 <- RCH_PM25_preandpost %>% filter(year == "2019")
RCH_t2_PM25 <- RCH_PM25_preandpost %>% filter(year == "2018")
RCH_t3_PM25 <- RCH_PM25_preandpost %>% filter(year == "2017")
RCH_t4_PM25 <- RCH_PM25_preandpost %>% filter(year == "2016")
RCH_t5_PM25 <- RCH_PM25_preandpost %>% filter(year == "2015")

mean(RCH_t0_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(RCH_t0_PM25$daily_mean, RCH_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(RCH_t0_PM25$daily_mean, RCH_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(RCH_t0_PM25$daily_mean, RCH_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(RCH_t0_PM25$daily_mean, RCH_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(RCH_t0_PM25$daily_mean, RCH_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Salt Lake City ###

#splitting data into separate data frames for t-tests
SLC_t0_PM25 <- SLC_PM25_preandpost %>% filter(year == "2022")
SLC_t1_PM25 <- SLC_PM25_preandpost %>% filter(year == "2021")
SLC_t2_PM25 <- SLC_PM25_preandpost %>% filter(year == "2020")
SLC_t2_PM25 <- SLC_t2_PM25[1:28,] #select first 28 observations (exclude leap day)
SLC_t3_PM25 <- SLC_PM25_preandpost %>% filter(year == "2019")
SLC_t4_PM25 <- SLC_PM25_preandpost %>% filter(year == "2018")
SLC_t5_PM25 <- SLC_PM25_preandpost %>% filter(year == "2017")

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(SLC_t0_PM25$daily_mean, SLC_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(SLC_t0_PM25$daily_mean, SLC_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(SLC_t0_PM25$daily_mean, SLC_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(SLC_t0_PM25$daily_mean, SLC_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(SLC_t0_PM25$daily_mean, SLC_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Alexandria ###

#splitting data into separate data frames for t-tests
ALX_t0_PM25 <- ALX_PM25_preandpost %>% filter(year == "2021")
ALX_t1_PM25 <- ALX_PM25_preandpost %>% filter(year == "2020")
ALX_t2_PM25 <- ALX_PM25_preandpost %>% filter(year == "2019")
ALX_t3_PM25 <- ALX_PM25_preandpost %>% filter(year == "2018")
ALX_t4_PM25 <- ALX_PM25_preandpost %>% filter(year == "2017")
ALX_t5_PM25 <- ALX_PM25_preandpost %>% filter(year == "2016")

mean(ALX_t5_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(ALX_t0_PM25$daily_mean, ALX_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALX_t0_PM25$daily_mean, ALX_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALX_t0_PM25$daily_mean, ALX_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALX_t0_PM25$daily_mean, ALX_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(ALX_t0_PM25$daily_mean, ALX_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Missoula ###

#splitting data into separate data frames for t-tests
MIS_t0_PM25 <- MIS_PM25_preandpost %>% filter(year == "2015")
MIS_t1_PM25 <- MIS_PM25_preandpost %>% filter(year == "2014")
MIS_t2_PM25 <- MIS_PM25_preandpost %>% filter(year == "2013")
MIS_t3_PM25 <- MIS_PM25_preandpost %>% filter(year == "2012")
MIS_t4_PM25 <- MIS_PM25_preandpost %>% filter(year == "2011")
MIS_t5_PM25 <- MIS_PM25_preandpost %>% filter(year == "2010")

mean(MIS_t2_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(MIS_t0_PM25$daily_mean, MIS_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(MIS_t0_PM25$daily_mean, MIS_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(MIS_t0_PM25$daily_mean, MIS_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(MIS_t0_PM25$daily_mean, MIS_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(MIS_t0_PM25$daily_mean, MIS_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Chapel Hill ###

#splitting data into separate data frames for t-tests
CHP_t0_PM25 <- CHP_PM25_preandpost %>% filter(year == "2002")
chp_new_row <- list(Date="2002-01-31",daily_mean=8.5,month="01",year=2002,pre_or_post="pre",city="CHP")
CHP_t0_PM25 <- rbind(CHP_t0_PM25,chp_new_row)
CHP_t1_PM25 <- CHP_PM25_preandpost %>% filter(year == "2001")
CHP_t2_PM25 <- CHP_PM25_preandpost %>% filter(year == "2000")
CHP_t3_PM25 <- CHP_PM25_preandpost %>% filter(year == "1999")

mean(CHP_t2_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(CHP_t0_PM25$daily_mean, CHP_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(CHP_t0_PM25$daily_mean, CHP_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
### Not enought data points from 1999 to do a paired t-test
#t.test(CHP_t0_PM25$daily_mean, CHP_t3_PM25$daily_mean, paired = TRUE, alternative = "less")


### Corvallis ###

#splitting data into separate data frames for t-tests
COR_t0_PM25 <- COR_PM25_preandpost %>% filter(year == "2011")
COR_t1_PM25 <- COR_PM25_preandpost %>% filter(year == "2010")
COR_t2_PM25 <- COR_PM25_preandpost %>% filter(year == "2009")
COR_t3_PM25 <- COR_PM25_preandpost %>% filter(year == "2008")
COR_t3_PM25 <- COR_t3_PM25[1:28,] #select first 28 observations (exclude leap day)
COR_t4_PM25 <- COR_PM25_preandpost %>% filter(year == "2007")
COR_t5_PM25 <- COR_PM25_preandpost %>% filter(year == "2006")

mean(COR_t2_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(COR_t0_PM25$daily_mean, COR_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(COR_t0_PM25$daily_mean, COR_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(COR_t0_PM25$daily_mean, COR_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(COR_t0_PM25$daily_mean, COR_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(COR_t0_PM25$daily_mean, COR_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


### Olympia ###

#splitting data into separate data frames for t-tests
OLY_t0_PM25 <- OLY_PM25_preandpost %>% filter(year == "2020")
OLY_t1_PM25 <- OLY_PM25_preandpost %>% filter(year == "2019")
OLY_t2_PM25 <- OLY_PM25_preandpost %>% filter(year == "2018")
OLY_t3_PM25 <- OLY_PM25_preandpost %>% filter(year == "2017")
OLY_t4_PM25 <- OLY_PM25_preandpost %>% filter(year == "2016")
OLY_t5_PM25 <- OLY_PM25_preandpost %>% filter(year == "2015")

mean(OLY_t2_PM25$daily_mean)

#t-test between daily mean of t0 vs t1, t2, etc time periods
t.test(OLY_t0_PM25$daily_mean, OLY_t1_PM25$daily_mean, paired = TRUE, alternative = "less")
## Substantial amount of missing data, nothing from Jan 1-22
#t.test(OLY_t0_PM25$daily_mean, OLY_t2_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(OLY_t0_PM25$daily_mean, OLY_t3_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(OLY_t0_PM25$daily_mean, OLY_t4_PM25$daily_mean, paired = TRUE, alternative = "less")
t.test(OLY_t0_PM25$daily_mean, OLY_t5_PM25$daily_mean, paired = TRUE, alternative = "less")


