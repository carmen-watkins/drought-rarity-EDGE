# Set up env 
library(tidyverse)
theme_set(theme_bw())

source("data-prep/clean_edge_data.R")

## read in data
spei <- read.csv("data/SPEI_all_EDGE.csv") %>%
  mutate(experiment.years = ifelse(year %in% c(2012:2021), "Y", "N"), 
         Site = ifelse(Site == "HAYS", "HYS",
                       ifelse(Site == "SEV_BLACK", "SEV_black", 
                              ifelse(Site == "SEV_BLUE", "SEV_blue", Site))))

## filter to growing season spei
spei.filt <- spei %>%
  filter(ifelse(Site == "SEV_black" | Site == "SEV_blue", month == 10, month == 9))

## reorder sites to match ppt gradient
spei.filt$Site <- as.factor(spei.filt$Site)
spei.filt <- spei.filt %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))


## classify SPEI years ##
spei.filt.record <- spei.filt %>%
  group_by(Site) %>%
  mutate(spei.class = ifelse(spei <= quantile(spei.filt$spei, probs = 0.25, na.rm = T), 1, 
                             ifelse(spei > quantile(spei.filt$spei, probs = 0.25, na.rm = T)
                                    & spei <= quantile(spei.filt$spei, probs = 0.5, na.rm = T), 2,
                                    ifelse(spei > quantile(spei.filt$spei, probs = 0.5, na.rm = T) & spei <= quantile(spei.filt$spei, probs = 0.75, na.rm = T), 3,
                                           ifelse(spei > quantile(spei.filt$spei, probs = 0.75, na.rm = T) & spei <= quantile(spei.filt$spei, probs = 1, na.rm = T), 4, "other")))))


ggplot(spei.filt.record, aes(x=spei, y=spei.class)) +
  geom_point() +
  facet_wrap(~Site)

## create spei df for experiment years only
spei.exp <- spei.filt.record %>%
  filter(experiment.years == "Y") %>%
  #filter(ifelse(Site %in% c("CHY", "HYS", "KNZ", "SGS"), month == 9, month == 10)) %>%
  mutate(site = Site) 

spei.exp$site <- as.factor(spei.exp$site)
spei.exp <- spei.exp %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

## merge spei with edge data
edge_w_spei <- left_join(edge_all, spei.exp, by = c("site", "year")) %>%
  filter(treatment == "C")

## Plot SPEI by year for each site
ggplot(spei.exp, aes(x=year, y=spei)) +
  geom_point() +
  facet_wrap(~site) +
  ylab("6-mo SPEI")

#ggsave("preliminary_figs/spei_exp_years_by_site.png", height = 4, width = 6)

## explore distrib of SPEI over all years
ggplot(spei.filt, aes(x=spei)) +
  geom_density() +
  facet_wrap(~Site) +
  geom_point(aes(x=spei, y=0.1, color = experiment.years)) +
  scale_color_manual(values = c("lightgrey", "black")) +
  xlab("monthly spei") +
  ylab("frequency")

#ggsave("preliminary_figs/spei_historical.png", height = 4, width = 6)
