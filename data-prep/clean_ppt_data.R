## Clean up northern edge precipitation data 
library(tidyverse)
precip <- read.csv("data/growingseason_precip_totals_allyears.csv")

sev_ppt <- read.csv("data/sev298_NPP_edge_biomass.csv")

## graph SEV data to see what it looks like
ggplot(sev_ppt, aes(x=year, y=season.precip, color = season)) +
  geom_point() +
  geom_line() +
  ylab("SEV Season Precip") +
  facet_wrap(~site)

ggsave("preliminary_figs/SEV_raw_season_precip.png", width = 7, height = 3)

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

ggsave("preliminary_figs/North_sites_raw_ppt.png", width = 5, height = 3)

## should also make a previous year precip column
