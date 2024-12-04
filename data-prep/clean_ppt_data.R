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
## get overall means across timeseries
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
site_ppt_avg = rbind(north_ppt, sev_ppt2)

# Separate precipitation by response ratio time periods
sev_ppt %>%
  group_by(site, year, season) %>%
  summarise(ppt = median(season.precip)) %>%
ggplot(aes(x=year, y=ppt, color = season)) +
  geom_point() +
  facet_wrap(~site) +
  geom_line()

sev_DRR4 = sev_ppt %>%
  filter(season == "fall", year %in% c(2013:2016)) %>%
  group_by(site) %>%
  summarise(pptDRR4 = mean(season.precip))

sev_DRR7 = sev_ppt %>%
  filter(season == "fall", year %in% c(2013:2019)) %>%
  group_by(site) %>%
  summarise(pptDRR7 = mean(season.precip))

sev_PDRRfull = sev_ppt %>%
  filter(season == "fall", year %in% c(2020:2023)) %>%
  group_by(site) %>%
  summarise(pptPDRRfull = mean(season.precip))

sev_PDRRfirst = sev_ppt %>%
  filter(season == "fall", year %in% c(2020:2021)) %>%
  group_by(site) %>%
  summarise(pptPDRRfirst = mean(season.precip))

sev_PDRRfinal = sev_ppt %>%
  filter(season == "fall", year %in% c(2022:2023)) %>%
  group_by(site) %>%
  summarise(pptPDRRfinal = mean(season.precip))

st1 = left_join(sev_DRR4, sev_DRR7, by = c("site"))
st2 = left_join(sev_PDRRfinal, sev_PDRRfirst, by = c("site"))
st3 = left_join(st1, sev_PDRRfull, by = c("site"))
sev_all = left_join(st3, st2, by = c("site"))

north_DRR4 = precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) %>%
  filter(Year %in% c(2014:2017)) %>%
  group_by(Site) %>%
  summarise(pptDRR4 = mean(tot.precip))

north_PDRRfull = precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) %>%
  filter(Year %in% c(2018:2021)) %>%
  group_by(Site) %>%
  summarise(pptPDRRfull = mean(tot.precip))

north_PDRRfirst = precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) %>%
  filter(Year %in% c(2018:2019)) %>%
  group_by(Site) %>%
  summarise(pptPDRRfirst = mean(tot.precip))

north_PDRRfinal = precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip)) %>%
  filter(Year %in% c(2020:2021)) %>%
  group_by(Site) %>%
  summarise(pptPDRRfinal = mean(tot.precip))

t1 = left_join(north_DRR4, north_PDRRfull, by = c("Site"))
t2 = left_join(north_PDRRfirst, north_PDRRfinal, by = c("Site"))
north_all = left_join(t1, t2, by = c("Site")) %>%
  mutate(site = Site,
         pptDRR7 = pptDRR4) %>%
  select(-Site)

ppt_intervals = rbind(sev_all, north_all) %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK",
                       ifelse(site == "EDGE_blue", "SBL", site)))

site_ppt = left_join(ppt_intervals, site_ppt_avg, by = "site")

# Clean up ####
rm(precip, sev_ppt, sev_ppt2, north_ppt, north_DRR4, north_PDRRfinal, north_PDRRfirst, north_PDRRfull, north_all, sev_all, sev_DRR4, sev_DRR7, sev_PDRRfinal, sev_PDRRfull, sev_PDRRfirst, st1, st2, st3, t1, t2, ppt_intervals, site_ppt_avg)
