here::i_am("code/ridership_change_graphs.R")

library(ggplot2)

ridership_change <- read.csv("raw-data/ridership_change.csv")
ridership_change <- ridership_change %>%
  rename_at('city', ~'city_code')

agg_pollutant_change <- cbind(agg_PM25_change,
                              agg_CO_change,
                              agg_NO2_change)

ridership_pollutant_change <- cbind(agg_pollutant_change,
                                    ridership_change)

ridership_pollutant_change <- ridership_pollutant_change %>%
  select(city,PM25_change,CO_change,NO2_change,prop_change)

ggplot(ridership_pollutant_change) +
  geom_point(aes(x=PM25_change,y=prop_change))