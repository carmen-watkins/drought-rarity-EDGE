# Header #### 
## Script name: Clean SPEI data
##
## Purpose of script: Clean & explore SPEI data for each site to prep for use 
## in analyses. Specifically need to filter to the correct years and to 
## growing season months.
##
## Author: Carmen Watkins
##

## SPEI data from:SPEI database https://spei.csic.es/database.html
## version v2.11:1
## used spei06 (6 month version)

library(tidyverse)
library(lubridate)

KNZ = read.csv("data/raw_data/site_and_env_data/spei/-96.750000_39.250000.csv")
HYS = read.csv("data/raw_data/site_and_env_data/spei/-99.250000_38.750000.csv")
CHY = read.csv("data/raw_data/site_and_env_data/spei/-104.750000_41.250000.csv")
SGS = read.csv("data/raw_data/site_and_env_data/spei/-104.750000_40.750000.csv")
SEV = read.csv("data/raw_data/site_and_env_data/spei/-106.750000_34.250000.csv")

theme_set(theme_classic())

clean_spei = function(dat, site) {
  datc = dat %>%
    mutate(dat = strsplit(dates.spei06, split = ";"),
           time = map_vec(dat, first),
           spei = map_vec(dat, last),
           site = site,
           lat = 39.25, 
           long = -96.75) %>%
    select(site, lat, long, time, spei)
  return(datc)
}

KNZc = clean_spei(KNZ, "KNZ")
HYSc = clean_spei(HYS, "HYS")
CHYc = clean_spei(CHY, "CHY")
SGSc = clean_spei(SGS, "SGS")
SEVc = clean_spei(SEV, "SEV")

spei_all = rbind(KNZc, HYSc, CHYc, SGSc, SEVc) %>%
  mutate(month = month(time)) %>%
  filter(ifelse(site == "SEV", month == 10, month == 9)) %>%
  ## filter to growing season spei; spei values should be the 6 months 
  ## preceding the month value
  ## selected 10 and 9 based on the growing season lengths SEV (Apr - Oct), Northern sites (Apr - Sept); it would be nice to use spei preceding data collection dates, but we don't have specific dates for Northern sites
  mutate(site = as.factor(site),
         site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV"),
         spei = as.numeric(spei))

spei_exp = spei_all %>%
  mutate(year = year(time),
         exp.year = ifelse(year %in% c(2013:2019) & site %in% c("SEV"), 
                           "Drought",
                           ifelse(year %in% c(2014:2017) & site %in% 
                                    c("KNZ", "HYS", "CHY", "SGS"), 
                                  "Drought", 
                                  ifelse(year %in% c(2020:2023) & site %in% c("SEV"), 
                                         "Post-Drought",
                                         ifelse(year %in% c(2018:2021) & site 
                                                %in% c("KNZ", "HYS", "CHY", "SGS"), 
                                                "Post-Drought", 
                                                "Historical"))))) %>%
  filter(exp.year != "Historical")

## clean env

rm(KNZ, HYS, CHY, SGS, SEV, KNZc, HYSc, CHYc, SGSc, SEVc)
