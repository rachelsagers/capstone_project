here::i_am("code/t-tests_CO.R") 

### Los Angeles ### 

#splitting data into two data frames for pre and post to compare 
LAX_pre_CO <- LAX_CO_preandpost %>% 
  filter(pre_or_post == "pre") 

LAX_post_CO <- LAX_CO_preandpost %>% 
  filter(pre_or_post == "post")

#t-test between daily mean of pre and post time periods 
t.test(LAX_pre_CO$daily_mean, LAX_post_CO$daily_mean) 


### Albuquerque ###

#splitting data into two data frames for pre and post to compare 
ALB_pre_CO <- ALB_CO_preandpost %>% 
  filter(pre_or_post == "pre")

ALB_post_CO <- ALB_CO_preandpost %>% 
  filter(pre_or_post == "post") 

#t-test between daily mean of pre and post time periods 
t.test(ALB_pre_CO$daily_mean, ALB_post_CO$daily_mean) 


### Tuscon ### 

#splitting data into two data frames for pre and post to compare 
TUS_pre_CO <- TUS_CO_preandpost %>% 
  filter(pre_or_post == "pre") 

TUS_post_CO <- TUS_CO_preandpost %>% 
  filter(pre_or_post == "post") 

#t-test between daily mean of pre and post time periods 
t.test(TUS_pre_CO$daily_mean, TUS_post_CO$daily_mean) 


### Kansas City ### 

#splitting data into two data frames for pre and post to compare 
KNC_pre_CO <- KNC_CO_preandpost %>% 
  filter(pre_or_post == "pre") 

KNC_post_CO <- KNC_CO_preandpost %>% 
  filter(pre_or_post == "post") 

#t-test between daily mean of pre and post time periods 
t.test(KNC_pre_CO$daily_mean, KNC_post_CO$daily_mean)


### Richmond ### 

#splitting data into two data frames for pre and post to compare 
RCH_pre_CO <- RCH_CO_preandpost %>% 
  filter(pre_or_post == "pre") 

RCH_post_CO <- RCH_CO_preandpost %>% 
  filter(pre_or_post == "post") 

#t-test between daily mean of pre and post time periods 
t.test(RCH_pre_CO$daily_mean, RCH_post_CO$daily_mean) 


### Salt Lake City ### 

#splitting data into two data frames for pre and post to compare 
SLC_pre_CO <- SLC_CO_preandpost %>% 
  filter(pre_or_post == "pre") 

SLC_post_CO <- SLC_CO_preandpost %>% 
  filter(pre_or_post == "post") 

#t-test between daily mean of pre and post time periods 
t.test(SLC_pre_CO$daily_mean, SLC_post_CO$daily_mean) 


### Alexandria ### 

#splitting data into two data frames for pre and post to compare 
ALX_pre_CO <- ALX_CO_preandpost %>% 
  filter(pre_or_post == "pre")

ALX_post_CO <- ALX_CO_preandpost %>% 
  filter(pre_or_post == "post") 

#t-test between daily mean of pre and post time periods 
t.test(ALX_pre_CO$daily_mean, ALX_post_CO$daily_mean) 