# Header ####
## Script name: Filter 1's
##
## Purpose of script: Filter species with response ratio values of 1 and -1 from
## data that were not present in pre-treatment year, for sensitivity analysis 
## to test whether these highly transient species were not driving patterns 
## in the data
##
## Author: Carmen Watkins

# Set Up ####
## load response ratio data, so can verify which are 1 or -1
source("analyses/calc_response_ratio.R") 

## load data with years attached so can filter to 'pre-treatment' species
source("data-prep/clean_cover_dat_fill_zeros.R")

# Prep Data ####
## filter to pre-treatment years
pre_trt = edge_all %>%
  filter(year < 2014) 

## filter gained and/or lost species
## select all species with a 1 or -1 value in drought or post-drought
checksp1 = edge_RR %>%
  filter(resp.ratio.site_D4 %in% c(1, -1, NaN) & 
           resp.ratio.site_PDfull %in% c(1, -1, NaN)) %>%
  select(site, species, resp.ratio.site_D4, resp.ratio.site_PDfull,
         spatial_rarity, temporal_rarity)

# Filter Sites ####
## KNZ ####
## examine KNZ gained or lost species
sort(unique(checksp1[checksp1$site == "KNZ",]$species))

## in pre-treatment data
knz_PT = pre_trt %>%
  ## filter to correct site
  filter(site == "KNZ") %>%
  ## filter gained or lost species
  filter(species %in% c(unique(checksp1[checksp1$site == "KNZ",]$species))) %>%
  group_by(species) %>%
  ## sum presence / absence across plots
  summarise(PA = sum(pres.abs))

## create a list of species to drop
## based on not appearing in pre-treatment data
Kdrop = knz_PT %>%
  filter(PA < 1)

## HYS ####
## examine HYS gained or lost species
sort(unique(checksp1[checksp1$site == "HYS",]$species))

## in pre-treatment data
hys_PT = pre_trt %>%
  ## filter to correct site
  filter(site == "HYS") %>%
  ## filter gained or lost species
  filter(species %in% c(unique(checksp1[checksp1$site == "HYS",]$species))) %>%
  group_by(species) %>%
  ## sum presence / absence across plots
  summarise(PA = sum(pres.abs))

## create a list of species to drop
## based on not appearing in pre-treatment data
Hdrop = hys_PT %>%
  filter(PA < 1)

## CHY ####
## examine CHY gained or lost species
sort(unique(checksp1[checksp1$site == "CHY",]$species))

## in pre-treatment data
chy_PT = pre_trt %>%
  ## filter to correct site
  filter(site == "CHY") %>%
  ## filter gained or lost species
  filter(species %in% c(unique(checksp1[checksp1$site == "CHY",]$species))) %>%
  group_by(species) %>%
  ## sum presence / absence across plots
  summarise(PA = sum(pres.abs))

## create a list of species to drop
## based on not appearing in pre-treatment data
Cdrop = chy_PT %>%
  filter(PA < 1)

## SGS ####
## examine SGS gained or lost species
sort(unique(checksp1[checksp1$site == "SGS",]$species))

## in pre-treatment data
sgs_PT = pre_trt %>%
  ## filter to correct site
  filter(site == "SGS") %>%
  ## filter gained or lost species
  filter(species %in% c(unique(checksp1[checksp1$site == "SGS",]$species))) %>%
  group_by(species) %>%
  ## sum presence / absence across plots
  summarise(PA = sum(pres.abs))

## create a list of species to drop
## based on not appearing in pre-treatment data
SGdrop = sgs_PT %>%
  filter(PA < 1)

## SBL ####
## examine SBL gained or lost species
sort(unique(checksp1[checksp1$site == "SBL",]$species))

## in pre-treatment data
sbl_PT = pre_trt %>%
  ## filter to correct site and year as 2012 is pre-treatment at SEV sites
  filter(site == "SBL", year < 2013) %>%
  ## filter gained or lost species
  filter(species %in% c(unique(checksp1[checksp1$site == "SBL",]$species))) %>%
  group_by(species) %>%
  ## sum presence / absence across plots
  summarise(PA = sum(pres.abs))

## create a list of species to drop
## based on not appearing in pre-treatment data
SLdrop = sbl_PT %>%
  filter(PA < 1)

## SBK ####
## examine SBK gained or lost species
sort(unique(checksp1[checksp1$site == "SBK",]$species))

## in pre-treatment data
sbk_PT = pre_trt %>%
  ## filter to correct site and year as 2012 is pre-treatment at SEV sites
  filter(site == "SBK", year < 2013) %>%
  ## filter gained or lost species
  filter(species %in% c(unique(checksp1[checksp1$site == "SBK",]$species))) %>%
  group_by(species) %>%
  ## sum presence / absence across plots
  summarise(PA = sum(pres.abs))

## create a list of species to drop
## based on not appearing in pre-treatment data
SKdrop = sbk_PT %>%
  filter(PA < 1)

# Clean Up ####
rm(knz_PT, hys_PT, chy_PT, sgs_PT, sbl_PT, sbk_PT)

