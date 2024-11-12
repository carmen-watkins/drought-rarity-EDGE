## Clean up northern edge precipitation data 
library(tidyverse)
library(ggpubr)
source("analyses/color_palettes.R")

pal <- wes_palette("Royal3")



precip <- read.csv("data/growingseason_precip_totals_allyears.csv")

sev_ppt <- read.csv("data/sev298_NPP_edge_biomass.csv")

## graph SEV data to see what it looks like
ggplot(sev_ppt, aes(x=year, y=season.precip, color = season)) +
  geom_point() +
  geom_line() +
  ylab("SEV Season Precip") +
  facet_wrap(~site)

#ggsave("preliminary_figs/SEV_raw_season_precip.png", width = 7, height = 3)

sev_ppt2 = sev_ppt %>%
  select(site, year, season, season.precip) %>%
  distinct() %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL"))

sev_ppt2$site <- factor(sev_ppt2$site, levels = c("SBL", "SBK"))

names(sev_ppt2) = c("Site", "Year", "Season", "tot.precip")

colnames(precip)

ggplot(precip, aes(x=ambient_precip)) +
  geom_histogram()


ggplot(precip, aes(x=Date, y=ambient_precip)) +
  geom_point() +
  facet_wrap(~Site)

unique(precip$Month)

north_ppt <- precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) #%>%
 # mutate(Site = fct_relevel(Site, "KNZ", "HYS", "CHY", "SGS"))

north_ppt$Site <- factor(north_ppt$Site, levels = c("KNZ", "HYS", "CHY", "SGS"))


ggplot(north_ppt, aes(x=Year, y=tot.precip, color = Site))+
  geom_point() +
  geom_line() 

#ggsave("preliminary_figs/North_sites_raw_ppt.png", width = 5, height = 3)

## Figure S1 ####
sev <- ggplot(sev_ppt2, aes(x=Year, y=tot.precip, linetype = Season, color = Site)) +
  geom_line() +
  geom_point(size = 2) +
  ylab(NULL) +
  scale_color_manual(values = pal[5:6]) +
  coord_cartesian(ylim = c(10, 915)) +
  theme(legend.position = "none")

north <- ggplot(north_ppt, aes(x=Year, y=tot.precip, color = Site))+
  geom_line() +
  geom_point(size = 2) +
  ylab("Seasonal Precipitation (XX?)") +
  scale_color_manual(values = pal[1:4]) +
  coord_cartesian(ylim = c(10, 915), xlim = c(2012, 2021)) +
  theme(legend.position = "none")

ggarrange(north, sev, labels = "AUTO")

ggsave("preliminary_figs/june_2024/figureS1_siteppt.tiff", width = 7, height = 3)

## should also make a previous year precip column

## make legend
north_ppt2 = north_ppt %>%
  mutate(season = NA)

both = rbind(north_ppt, sev_ppt2)

ggplot(both, aes(x=Year, y=tot.precip, color = Site, linetype = Season)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_manual(values = pal)

ggsave("preliminary_figs/june_2024/figureS1_legend.tiff", width = 5, height = 3)



