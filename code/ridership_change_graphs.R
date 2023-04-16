here::i_am("code/ridership_change_graphs.R")

library(ggplot2)
library(dplyr)

ridership_change <- read.csv("raw-data/ridership_change.csv")
ridership_change <- ridership_change %>%
  rename_at('city', ~'city_code')

agg_pollutant_change <- cbind(agg_PM25_change,
                              agg_CO_change,
                              agg_NO2_change)

ridership_pollutant_change <- cbind(agg_pollutant_change,
                                    ridership_change)

ridership_pollutant_change <- ridership_pollutant_change %>%
  select(city_code,PM25_change,CO_change,NO2_change,prop_change)

## All 3 pollutants on a graph ##
ggplot(ridership_pollutant_change) +
  geom_point(aes(y=PM25_change,x=prop_change,color="PM25")) +
  geom_point(aes(y=CO_change,x=prop_change,color="CO")) +
  geom_point(aes(y=NO2_change,x=prop_change,color="NO2")) +
  xlab("Change in Ridership") +
  ylab("Change in Pollutant Concentration") +
  labs(title="Change in Ridership vs Change in Pollutant Concentration") +
  scale_color_manual(name="Pollutant", values=c(PM25="blue",CO="red",NO2="green"))

## Pollutants on separate graphs with labeled cities ##
ggplot(ridership_pollutant_change, aes(x=prop_change,y=PM25_change,label=city_code)) +
  geom_point() +
  geom_text(hjust=0,vjust=0)