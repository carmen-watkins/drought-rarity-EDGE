# Header ####
## Script name: Filter 1's
##
## Purpose of script: Filter species with response ratio values of 1 and -1 from
## data that were not present in pre-treatment year, for sensitivity analysis 
## to test whether these highly transient species were not driving patterns 
## in the data
##
## Author: Carmen Watkins

## load response ratio data, so can verify which are 1 or -1
source("analyses/calc_response_ratio.R") 

## load data with years attached so can filter to 'pre-treatment' species
source("data-prep/clean_cover_dat_fill_zeros.R")

## filter to pre-treatment years
pre_trt = edge_all %>%
  filter(year < 2014) 

## Check 1's and -1's ####
## select all species with a 1 or -1 value in drought or post-drought
checksp1 = edge_RR %>%
  filter(resp.ratio.site_D4 %in% c(1, -1, NaN) & 
           resp.ratio.site_PDfull %in% c(1, -1, NaN)) %>%
  select(site, species, resp.ratio.site_D4, resp.ratio.site_PDfull,
         spatial_rarity, temporal_rarity)


## KNZ
knz_check = checksp1 %>%
  filter(site == "KNZ")

sort(unique(knz_check$species))

## sum number of times each species appeared in the pre-treatment data
knz_PT = pre_trt %>%
  filter(site == "KNZ") %>%
  group_by(species, treatment)%>%
  summarise(pres.abs.sum = sum(pres.abs)) 

## filter pre-treatment data by the species that need to be checked as they
## have a 1 or -1 value RR
Ktmp = knz_PT %>%
  filter(species %in% c(unique(knz_check$species))) %>%
  group_by(species) %>%
  summarise(PA = sum(pres.abs.sum))
  
Kkeep = Ktmp %>%
  filter(PA >= 1)

Kdrop = Ktmp %>%
  filter(PA < 1)

## HYS ####
hys_check = checksp1 %>%
  filter(site == "HYS")

sort(unique(hys_check$species))

hys_PT = pre_trt %>%
  filter(site == "HYS") %>%
  group_by(species, treatment)%>%
  summarise(pres.abs.sum = sum(pres.abs)) 

Htmp = hys_PT %>%
  filter(species %in% c(unique(hys_check$species))) %>%
  group_by(species) %>%
  summarise(PA = sum(pres.abs.sum))

Hkeep = Htmp %>%
  filter(PA >= 1)

Hdrop = Htmp %>%
  filter(PA < 1)

## CHY ####
chy_check = checksp1 %>%
  filter(site == "CHY")

sort(unique(chy_check$species))

chy_PT = pre_trt %>%
  filter(site == "CHY") %>%
  group_by(species, treatment)%>%
  summarise(pres.abs.sum = sum(pres.abs)) 

Ctmp = chy_PT %>%
  filter(species %in% c(unique(chy_check$species))) %>%
  group_by(species) %>%
  summarise(PA = sum(pres.abs.sum))

Ckeep = Ctmp %>%
  filter(PA >= 1)

Cdrop = Ctmp %>%
  filter(PA < 1)

## SGS ####
sgs_check = checksp1 %>%
  filter(site == "SGS")

sort(unique(sgs_check$species))

sgs_PT = pre_trt %>%
  filter(site == "SGS") %>%
  group_by(species, treatment)%>%
  summarise(pres.abs.sum = sum(pres.abs)) 

SGtmp = sgs_PT %>%
  filter(species %in% c(unique(sgs_check$species))) %>%
  group_by(species) %>%
  summarise(PA = sum(pres.abs.sum))

SGkeep = SGtmp %>%
  filter(PA >= 1)

SGdrop = SGtmp %>%
  filter(PA < 1)

## SBL ####
sbl_check = checksp1 %>%
  filter(site == "SBL")

sort(unique(sbl_check$species))

sbl_PT = pre_trt %>%
  filter(site == "SBL", 
         year < 2013) %>%
  group_by(species, treatment)%>%
  summarise(pres.abs.sum = sum(pres.abs)) 

SLtmp = sbl_PT %>%
  filter(species %in% c(unique(sbl_check$species))) %>%
  group_by(species) %>%
  summarise(PA = sum(pres.abs.sum))

SLkeep = SLtmp %>%
  filter(PA >= 1)

SLdrop = SLtmp %>%
  filter(PA < 1)

## SBK ####
sbk_check = checksp1 %>%
  filter(site == "SBK")

sort(unique(sbk_check$species))

sbk_PT = pre_trt %>%
  filter(site == "SBK", 
         year < 2013) %>%
  group_by(species, treatment)%>%
  summarise(pres.abs.sum = sum(pres.abs)) 

SKtmp = sbk_PT %>%
  filter(species %in% c(unique(sbk_check$species))) %>%
  group_by(species) %>%
  summarise(PA = sum(pres.abs.sum))

SKkeep = SKtmp %>%
  filter(PA >= 1)

SKdrop = SKtmp %>%
  filter(PA < 1)

