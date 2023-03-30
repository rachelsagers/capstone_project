here::i_am("code/t-tests_pm25.R")


### Los Angeles ###

#splitting data into two data frames for pre and post to compare
LAX_pre_PM25 <- LAX_PM25_preandpost %>%
  filter(pre_or_post == "pre")

LAX_post_PM25 <- LAX_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(LAX_pre_PM25$daily_mean, LAX_post_PM25$daily_mean)


### Albuquerque ###

#splitting data into two data frames for pre and post to compare
ALB_pre_PM25 <- ALB_PM25_preandpost %>%
  filter(pre_or_post == "pre")

ALB_post_PM25 <- ALB_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(ALB_pre_PM25$daily_mean, ALB_post_PM25$daily_mean)

### Tuscon ###

#splitting data into two data frames for pre and post to compare
TUS_pre_PM25 <- TUS_PM25_preandpost %>%
  filter(pre_or_post == "pre")

TUS_post_PM25 <- TUS_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(TUS_pre_PM25$daily_mean, TUS_post_PM25$daily_mean)


### Kansas City ###

#splitting data into two data frames for pre and post to compare
KNC_pre_PM25 <- KNC_PM25_preandpost %>%
  filter(pre_or_post == "pre")

KNC_post_PM25 <- KNC_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(KNC_pre_PM25$daily_mean, KNC_post_PM25$daily_mean)


### Richmond ###

#splitting data into two data frames for pre and post to compare
RCH_pre_PM25 <- RCH_PM25_preandpost %>%
  filter(pre_or_post == "pre")

RCH_post_PM25 <- RCH_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(RCH_pre_PM25$daily_mean, RCH_post_PM25$daily_mean)


### Salt Lake City ###

#splitting data into two data frames for pre and post to compare
SLC_pre_PM25 <- SLC_PM25_preandpost %>%
  filter(pre_or_post == "pre")

SLC_post_PM25 <- SLC_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(SLC_pre_PM25$daily_mean, SLC_post_PM25$daily_mean)


### Alexandria ###

#splitting data into two data frames for pre and post to compare
ALX_pre_PM25 <- ALX_PM25_preandpost %>%
  filter(pre_or_post == "pre")

ALX_post_PM25 <- ALX_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(ALX_pre_PM25$daily_mean, ALX_post_PM25$daily_mean)


### Missoula ###

#splitting data into two data frames for pre and post to compare
MIS_pre_PM25 <- MIS_PM25_preandpost %>%
  filter(pre_or_post == "pre")

MIS_post_PM25 <- MIS_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(MIS_pre_PM25$daily_mean, MIS_post_PM25$daily_mean)


### Chapel Hill ###

#splitting data into two data frames for pre and post to compare
CHP_pre_PM25 <- CHP_PM25_preandpost %>%
  filter(pre_or_post == "pre")

CHP_post_PM25 <- CHP_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(CHP_pre_PM25$daily_mean, CHP_post_PM25$daily_mean)


### Corvallis ###

#splitting data into two data frames for pre and post to compare
COR_pre_PM25 <- COR_PM25_preandpost %>%
  filter(pre_or_post == "pre")

COR_post_PM25 <- COR_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(COR_pre_PM25$daily_mean, COR_post_PM25$daily_mean)


### Olympia ###

#splitting data into two data frames for pre and post to compare
OLY_pre_PM25 <- OLY_PM25_preandpost %>%
  filter(pre_or_post == "pre")

OLY_post_PM25 <- OLY_PM25_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(OLY_pre_PM25$daily_mean, OLY_post_PM25$daily_mean)


