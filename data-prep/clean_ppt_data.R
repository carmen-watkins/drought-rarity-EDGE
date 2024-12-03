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

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in precip data
precip <- read.csv("data/growingseason_precip_totals_allyears.csv")
sev_ppt <- read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")

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

# Clean up ####
rm(precip, sev_ppt, sev_ppt2, north_ppt)
