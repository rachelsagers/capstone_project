here::i_am("code/t-tests_NO2.R")


### Los Angeles ###

#splitting data into two data frames for pre and post to compare
LAX_pre_NO2 <- LAX_NO2_preandpost %>%
  filter(pre_or_post == "pre")

LAX_post_NO2 <- LAX_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(LAX_pre_NO2$daily_mean, LAX_post_NO2$daily_mean)


### Albuquerque ###

#splitting data into two data frames for pre and post to compare
ALB_pre_NO2 <- ALB_NO2_preandpost %>%
  filter(pre_or_post == "pre")

ALB_post_NO2 <- ALB_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(ALB_pre_NO2$daily_mean, ALB_post_NO2$daily_mean)

### Tuscon ###

#splitting data into two data frames for pre and post to compare
TUS_pre_NO2 <- TUS_NO2_preandpost %>%
  filter(pre_or_post == "pre")

TUS_post_NO2 <- TUS_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(TUS_pre_NO2$daily_mean, TUS_post_NO2$daily_mean)


### Kansas City ###

#splitting data into two data frames for pre and post to compare
KNC_pre_NO2 <- KNC_NO2_preandpost %>%
  filter(pre_or_post == "pre")

KNC_post_NO2 <- KNC_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(KNC_pre_NO2$daily_mean, KNC_post_NO2$daily_mean)


### Richmond ###

#splitting data into two data frames for pre and post to compare
RCH_pre_NO2 <- RCH_NO2_preandpost %>%
  filter(pre_or_post == "pre")

RCH_post_NO2 <- RCH_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(RCH_pre_NO2$daily_mean, RCH_post_NO2$daily_mean)


### Salt Lake City ###

#splitting data into two data frames for pre and post to compare
SLC_pre_NO2 <- SLC_NO2_preandpost %>%
  filter(pre_or_post == "pre")

SLC_post_NO2 <- SLC_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(SLC_pre_NO2$daily_mean, SLC_post_NO2$daily_mean)


### Alexandria ###

#splitting data into two data frames for pre and post to compare
ALX_pre_NO2 <- ALX_NO2_preandpost %>%
  filter(pre_or_post == "pre")

ALX_post_NO2 <- ALX_NO2_preandpost %>%
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods
t.test(ALX_pre_NO2$daily_mean, ALX_post_NO2$daily_mean)
