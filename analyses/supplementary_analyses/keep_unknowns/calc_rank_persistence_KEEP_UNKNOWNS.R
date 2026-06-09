# Header #### 
## Script name: Sensitivity 
##
## Purpose of script: Calculate rank and persistence values for each species
## UNKNOWN VALUES KEPT IN
##
## Author: Carmen Watkins
##

source("data-prep/keep_unknowns/clean_cover_dat_fill_zeros_KEEP_UNKNOWNS_sensA.R")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## filter data to include control plots only
## use edge data with zeros for accurate calculations
controls <- edge_all %>%
  filter(treatment == "C")

# Rank ####
## take the rank of the mean (NOT the mean of the rank)
## keep the 0's
rank_mean <- controls %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  ## this averages over all the subplots, including 0-filled subs
  summarise(mean.ctrl.cov = mean(max.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(percrank = percent_rank(mean.ctrl.cov), ## take the percent rank
         absrank = rank(mean.ctrl.cov)) 

# Persistence ####
persist_site <- controls %>%
  group_by(site, species, year) %>%
  summarise(pres.abs.site = ifelse(sum(pres.abs)>0, 1,0)) %>% ## present at site?
  ungroup() %>%
  group_by(site, species) %>%
  summarise(persistence.site = sum(pres.abs.site)/n())

# Merge Rank & Persist ####
rank_persist <- left_join(persist_site, rank_mean, by = c("site", "species")) %>%
  mutate(spatial_rarity = 1 - percrank,
         temporal_rarity = 1 - persistence.site)

# Clean Env ####
rm(controls, persist_site, rank_mean)

# Calc Resp Ratio ####
## Drought ####
drought.SE.RII <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  
  ungroup() %>%
  
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), ## calc response ratio
         
         ## calc error of RII
         ## rho
         rho = (((sd.cover.sp_D^2)/num.obs_D) - ((sd.cover.sp_C^2)/num.obs_C)) / ((sd.cover.sp_D^2/num.obs_D) + (sd.cover.sp_C^2/num.obs_C)), ## calc rho as part of standard error calc
         
         ## term outside of parentheses
         outpar = ((sd.cover.sp_D^2)/num.obs_D + (sd.cover.sp_C^2)/num.obs_C) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 1 inside parentheses
         term1 = ((mean.cover.sp_D - mean.cover.sp_C)^2) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 2 inside parentheses
         term2 = (2 * rho * (mean.cover.sp_D - mean.cover.sp_C)) / (mean.cover.sp_D + mean.cover.sp_C),
         
         ## calc inside of parentheses
         inpar = 1 + term1 - term2,
         
         ## calc SE
         SE.RII = outpar * inpar,
         
         treatment.period = "D") ## add in column to differentiate from post-drought RR

## Recovery ####
recov.SE.RII <- edge_all %>%
  filter(treatment.year == "recovery") %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  
  ungroup() %>%
  
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), ## calc response ratio
         
         ## calc error of RII
         ## rho
         rho = (((sd.cover.sp_D^2)/num.obs_D) - ((sd.cover.sp_C^2)/num.obs_C)) / ((sd.cover.sp_D^2/num.obs_D) + (sd.cover.sp_C^2/num.obs_C)), ## calc rho as part of standard error calc
         
         ## term outside of parentheses
         outpar = ((sd.cover.sp_D^2)/num.obs_D + (sd.cover.sp_C^2)/num.obs_C) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 1 inside parentheses
         term1 = ((mean.cover.sp_D - mean.cover.sp_C)^2) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 2 inside parentheses
         term2 = (2 * rho * (mean.cover.sp_D - mean.cover.sp_C)) / (mean.cover.sp_D + mean.cover.sp_C),
         
         ## calc inside of parentheses
         inpar = 1 + term1 - term2,
         
         ## calc SE
         SE.RII = outpar * inpar,
         
         treatment.period = "PD") ## add in column to differentiate from post-drought RR

RR.tog = rbind(drought.SE.RII, recov.SE.RII) %>%
  select(site, species, resp.ratio.site, SE.RII, treatment.period) %>%
  pivot_wider(names_from = treatment.period, values_from = c(resp.ratio.site, SE.RII))#

## merge with rank and persistence values for each species
edge_RR = left_join(RR.tog, rank_persist, by = c("site", "species"))

## Clean up ####
rm(edge_all, drought.SE.RII, recov.SE.RII, RR.tog, rank_persist)

## write.csv(edge_RR, "analyses/supplementary_analyses/keep_unknowns/edge_response_ratio_and_rarity_WITH_UNKNOWNS.csv")
