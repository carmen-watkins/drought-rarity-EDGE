# Set up env ####
## read in cleaned data
source("data-prep/classify_rank_persistence.R")
source("data-prep/clean_edge_data.R")

# Calcs ####
## Resp Ratio Across Years ####
### During Drought ####
## (drought - control)/control + drought
#### Block Level ####
resp.ratio.block <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, block, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by block across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, block, species) %>%
  mutate(resp.ratio.block = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
edge_w_predictors.block <- left_join(resp.ratio.block, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.block$site <- as.factor(edge_w_predictors.block$site)
edge_w_predictors.block <- edge_w_predictors.block %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

#### Site Level ####
resp.ratio.site <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
edge_w_predictors.site <- left_join(resp.ratio.site, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site$site <- as.factor(edge_w_predictors.site$site)
edge_w_predictors.site <- edge_w_predictors.site %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### During Recovery ####
#### Site Level ####
resp.ratio.site.recov <- edge_all %>%
  filter(treatment.year == "recovery") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
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
  select(-spcode, -kartez, -plot, -block, -year) %>%
  group_by(site, treatment, species, experiment.year, treatment.year) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% 
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species, experiment.year, treatment.year) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

edge_yearly_w_predictors <- left_join(resp.ratio.yearly, rank_persist, by = c("site", "species"))

edge_yearly_w_predictors$site <- as.factor(edge_yearly_w_predictors$site)
edge_yearly_w_predictors <- edge_yearly_w_predictors %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

year.key <- edge_all %>%
  group_by(site, experiment.year, treatment.year, year) %>%
  summarise(year2 = unique(year)) 



