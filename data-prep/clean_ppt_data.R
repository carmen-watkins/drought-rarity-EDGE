## Clean up northern edge precipitation data 
library(tidyverse)
precip <- read.csv("data/growingseason_precip_totals_allyears.csv")

colnames(precip)

ggplot(precip, aes(x=ambient_precip)) +
  geom_histogram()


ggplot(precip, aes(x=Date, y=ambient_precip)) +
  geom_point() +
  facet_wrap(~Site)

unique(precip$Month)

growing.season.tot <- precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) %>%
  mutate(site = Site, 
         year = Year)

ggplot(growing.season.tot, aes(x=Year, y=tot.precip, color = Site))+
  geom_point() +
  geom_line() 
  
## should also make a previous year precip column
