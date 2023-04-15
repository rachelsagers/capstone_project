here::i_am("code/heatmap_pvalues.R")

library(ggplot2)
library(dplyr)
library(reshape)
library(RColorBrewer)

PM25_pvalues <- read.csv("raw-data/PM25_ttest_pvalues.csv")
colnames(PM25_pvalues) <- c("city","t-1","t-2","t-3","t-4","t-5")

#PM25_pvalues$city <- factor(PM25_pvalues$city, levels=c("LAX","ALB","TUS","KNC","RCH",
#                                                "SLC","ALX","MIS","CHP","COR","OLY"))

PM25_melt_pvalues <- melt(PM25_pvalues)

PM25_melt_pvalues <- PM25_melt_pvalues %>%
  mutate(p_value = case_when(value >= 0.1 ~ "> 0.1",
                              value >= 0.05 & value < 0.1 ~ "0.05-0.1",
                              value >= 0.001 & value < 0.05 ~ "0.001-0.05",
                              value < 0.001 ~ "< 0.001"))

#PM25_melt_pvalues$p_value <- factor(PM25_melt_pvalues$p_value, levels=c("> 0.1","0.05-0.1",
#                                                                        "0.001-0.05","<0.001"))

#default heatmap with default scaling
ggplot(PM25_melt_pvalues, aes(variable, city)) +
  geom_tile(aes(fill = value)) +
  scale_x_discrete(position="top",name="Time") + # put x axis on top
  scale_y_discrete(limits=rev, name="City") + #reverse order of labels on y
  labs(fill = "P-Value",
       title = "Improvement in PM2.5 Concentration")
  #scale_fill_gradient2(low=("blue"), mid="white", high=("red"), midpoint=0.1, na.value="grey", guide="colorbar")


#heatmap with custom scaling
ggplot(PM25_melt_pvalues, aes(variable, city)) +
  geom_tile(aes(fill = p_value)) +
  scale_fill_manual(breaks=c("> 0.1","0.05-0.1","0.001-0.05","< 0.001",NA), values=c("#e8f1f2","#91d3f5","#006494","#13293d")) +
  scale_x_discrete(position="top",name="Time") + # put x axis on top
  scale_y_discrete(limits=rev, name="City") + #reverse order of labels on y
  labs(fill = "P-Value",
       title = "Significance of Improvement in PM2.5 Concentration")