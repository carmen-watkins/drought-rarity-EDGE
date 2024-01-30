# Header #### 
## Script name: Calculate Response Ratio
##
## Purpose of script: Calculate the response ratio between drought and control plots 1. across years during two time periods (drought & recovery) and 2. for each year. Add in functional group data as well.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned data
source("data-prep/classify_rank_persistence.R")
source("data-prep/clean_edge_data.R")

FG <- read.csv("data/edge_species_info.csv")

# Resp Ratio Across Years ####
## During Drought ####
## (drought - control)/control + drought

### Block Level ##
## NOT using block level anymore
#resp.ratio.block <- edge_all %>%
 # filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  #group_by(site, block, treatment, species) %>%
#  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by block across years
 # pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  #ungroup() %>%
#  group_by(site, block, species) %>%
 # mutate(resp.ratio.block = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
#edge_w_predictors.block <- left_join(resp.ratio.block, rank_persist, by = c("site", "species"))

## change site to an ordered factor
#edge_w_predictors.block$site <- as.factor(edge_w_predictors.block$site)
#edge_w_predictors.block <- edge_w_predictors.block %>%
 # mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### Site Level ####
resp.ratio.site <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
edge_w_predictors.site <- left_join(resp.ratio.site, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site$site <- as.factor(edge_w_predictors.site$site)
edge_w_predictors.site <- edge_w_predictors.site %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## During Recovery ####
### Site Level ####
resp.ratio.site.recov <- edge_all %>%
  filter(treatment.year == "recovery") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank and persistence values for each species
edge_w_predictors.site.recov <- left_join(resp.ratio.site.recov, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.recov$site <- as.factor(edge_w_predictors.site.recov$site)
edge_w_predictors.site.recov <- edge_w_predictors.site.recov %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## Resp Ratio Yearly ####
resp.ratio.yearly <- edge_all %>%
  ungroup() %>%
  select(-spcode, -kartez, -plot, -block, -year) %>% ## remove extraneous cols that will mess up pivoting
  group_by(site, treatment, species, experiment.year, treatment.year) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% 
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species, experiment.year, treatment.year) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank & persistence vals
edge_yearly_w_predictors <- left_join(resp.ratio.yearly, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_yearly_w_predictors$site <- as.factor(edge_yearly_w_predictors$site)
edge_yearly_w_predictors <- edge_yearly_w_predictors %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## create a key of years
## will be used to match up spei data to particular treatment years
year.key <- edge_all %>%
  group_by(site, experiment.year, treatment.year, year) %>%
  summarise(year2 = unique(year)) 

# Merge DR & RR Data ####
drought.RR <- edge_w_predictors.site %>%
  mutate(treatment.period = "drought.RR") %>%
  select(site, treatment.period, species, resp.ratio.site, persistence.site, percrank)

recov.RR <- edge_w_predictors.site.recov %>%
  mutate(treatment.period = "recovery.RR") %>%
  select(site, treatment.period, species, resp.ratio.site, persistence.site, percrank)

## merge drought & recov dataframes
response.ratio.tog <- rbind(drought.RR, recov.RR) %>%
  mutate(precip.bin = ifelse(site %in% c("KNZ", "HYS"), "high",
                             ifelse(site %in% c("CHY", "SGS"), "med", "low"))) %>%
  pivot_wider(names_from = treatment.period, values_from = resp.ratio.site) %>%
  mutate(drought.RR = ifelse(is.na(drought.RR), 0, drought.RR),
         recovery.RR = ifelse(is.na(recovery.RR), 0, recovery.RR))

# Merge FG Data ####
edge_FG <- left_join(response.ratio.tog, FG, by = "species") %>%
  filter(FunctionalGroup != "tree", !is.na(FunctionalGroup)) 

edge_FG$precip.bin <- as.factor(edge_FG$precip.bin)

edge_FG <- edge_FG %>%
  mutate(precip.bin = fct_relevel(precip.bin, "high", "med", "low"))

# Clean up ####
rm(edge_all, edge_w_zeros, rank_persist, resp.ratio.site, resp.ratio.site.recov, resp.ratio.yearly, response.ratio.tog, drought.RR, recov.RR, edge_w_predictors.site, edge_w_predictors.site.recov)
