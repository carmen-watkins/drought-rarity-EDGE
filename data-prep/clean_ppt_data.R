# Header #### 
## Script name: Clean Precipitation Data
##
## Purpose of script: Clean precip data and calculate the mean over the time series to use as a predictor variable in models
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
## load packages
library(tidyverse)
library(ggpubr)

## read in precip data
precip <- read.csv("data/growingseason_precip_totals_allyears.csv")
sev_ppt <- read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")

## set up for visualization 
source("analyses/color_palettes.R")
pal <- wes_palette("Royal3")
theme_set(theme_classic())

# Clean ####
sev_ppt2 = sev_ppt %>%
  select(site, year, season, season.precip) %>%
  distinct() %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL")) %>%
  filter(season == "fall") %>%
  group_by(site) %>%
  summarise(mean_ppt = mean(season.precip),
            se_ppt = calcSE(season.precip))

north_ppt <- precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) %>%
  group_by(Site) %>%
  summarise(mean_ppt = mean(tot.precip),
            se_ppt = calcSE(tot.precip)) %>%
  mutate(site = Site) %>%
  select(-Site)

## merge together
site_ppt = rbind(north_ppt, sev_ppt2)

# Figure S1 ####
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

## make legend
north_ppt2 = north_ppt %>%
  mutate(season = NA)

both = rbind(north_ppt, sev_ppt2)

ggplot(both, aes(x=Year, y=tot.precip, color = Site, linetype = Season)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_manual(values = pal)

#ggsave("preliminary_figs/june_2024/figureS1_legend.tiff", width = 5, height = 3)

# Clean up ####
rm(precip, sev_ppt)