# Header #### 
## Script name: Clean SPEI data
##
## Purpose of script: Clean & explore SPEI data for each site to prep for use in analyses. Specifically need to filter to the correct years and to growing season months.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
library(tidyverse)
theme_set(theme_bw())

## read in spei data
spei.dat <- read.csv("data/SPEI_all_EDGE.csv") %>%
  mutate(experiment.years = ifelse(year %in% c(2012:2021), "Y", "N"), 
         site = ifelse(Site == "HAYS", "HYS", ## change site names to match cover data
                       ifelse(Site == "SEV_BLACK", "SEV_black", 
                              ifelse(Site == "SEV_BLUE", "SEV_blue", Site)))) %>%
  select(-Site) ## get rid of capitalized column, keep uncapitalized to match cover data

# Clean ####
## classify SPEI years 
spei.filt <- spei.dat %>%
  filter(ifelse(site == "SEV_black" | site == "SEV_blue", month == 10, month == 9)) %>% ## filter to growing season spei; spei values should be the 6 months preceding the month value
  ## selected 10 and 9 based on the growing season lengths SEV (Apr - Oct), Northern sites (Apr - Sept); it would be nice to use spei preceding data collection dates, but we don't have specific dates for Northern sites
  group_by(site) %>%
  mutate(spei.class = ifelse(spei <= quantile(spei.dat$spei, probs = 0.25, na.rm = T), 1, 
                             ifelse(spei > quantile(spei.dat$spei, probs = 0.25, na.rm = T)
                                    & spei <= quantile(spei.dat$spei, probs = 0.5, na.rm = T), 2,
                                    ifelse(spei > quantile(spei.dat$spei, probs = 0.5, na.rm = T) & spei <= quantile(spei.dat$spei, probs = 0.75, na.rm = T), 3,
                                           ifelse(spei > quantile(spei.dat$spei, probs = 0.75, na.rm = T) & spei <= quantile(spei.dat$spei, probs = 1, na.rm = T), 4, "other"))))) ## classify spei by quantiles

## reorder sites to match ppt gradient
spei.filt$site <- as.factor(spei.filt$site)
spei.filt <- spei.filt %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

## create spei df for experiment years only
spei.exp <- spei.filt %>%
  filter(experiment.years == "Y")

## reorder sites to match ppt gradient
spei.exp$site <- as.factor(spei.exp$site)
spei.exp <- spei.exp %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

# Visualize ####
## check whether spei classification was successful
ggplot(spei.filt, aes(x=spei, y=spei.class, color = experiment.years)) +
  geom_point() +
  facet_wrap(~site) +
  scale_color_manual(values = c("lightgrey", "black"))

#ggsave("preliminary_figs/spei_class_by_site.png", height = 4, width = 6)

## Plot SPEI by year for each site
ggplot(spei.exp, aes(x=year, y=spei)) +
  geom_point() +
  facet_wrap(~site) +
  ylab("6-mo SPEI")

#ggsave("preliminary_figs/spei_exp_years_by_site.png", height = 4, width = 6)

## explore distrib of SPEI over all years
ggplot(spei.filt, aes(x=spei)) +
  geom_density() +
  facet_wrap(~site) +
  geom_point(aes(x=spei, y=0.1, color = experiment.years)) +
  scale_color_manual(values = c("lightgrey", "black")) +
  xlab("monthly spei") +
  ylab("frequency")

#ggsave("preliminary_figs/spei_historical.png", height = 4, width = 6)

## clean up environment
rm(spei.dat, spei.filt)
