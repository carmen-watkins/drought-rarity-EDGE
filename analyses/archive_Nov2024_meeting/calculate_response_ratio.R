# Header #### 
## Script name: Calculate Response Ratio
##
## Purpose of script: Calculate the response ratio between drought and control plots across years during two time periods (drought & recovery). 
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

## References: 
## Response Ratio: Armas et al. 2004
## SE of Response Ratio: Armas et al. 2004 supplement A, file:///C:/Users/carme/Downloads/appendixA.htm

# Set up ####
## read in cleaned data
source("data-prep/classify_rank_persistence.R")

## load packages
library(ggpubr)

# Resp Ratio ####
## Drought ####
### 4-year ####
drought.SE.RII <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species, block, plot) %>%
  
  summarise(mean.cover.sp = mean(mean.plot.cover), ## mean cover by site across years
            sd.cover.sp = sd(mean.plot.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  select(-plot) %>%
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  
  mutate(mean.cover.sp_D = coalesce(mean.cover.sp_D, 0), 
         mean.cover.sp_C = coalesce(mean.cover.sp_C, 0)) %>%
  ## input 0 instead of NAs (NAs are present where there is no cover of a particular species in either drought or control)
  
  group_by(site, species, block) %>%
  
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

### 6-year ####
drought.SE.RII.6 <- edge_all %>%
  filter(treatment.year == "drought") %>% ## should select all 6 of the SEV drought years
  group_by(site, treatment, species, block, plot) %>%
  
  summarise(mean.cover.sp = mean(mean.plot.cover), ## mean cover by site across years
            sd.cover.sp = sd(mean.plot.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  select(-plot) %>%
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  
  mutate(mean.cover.sp_D = coalesce(mean.cover.sp_D, 0), 
         mean.cover.sp_C = coalesce(mean.cover.sp_C, 0)) %>%
  ## input 0 instead of NAs (NAs are present where there is no cover of a particular species in either drought or control)
  
  group_by(site, species, block) %>%
  
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


## Post-Drought ####
recov.SE.RII <- edge_all %>%
  filter(treatment.year == "recovery") %>% 
  
  group_by(site, treatment, species, block, plot) %>%
  
  summarise(mean.cover.sp = mean(mean.plot.cover), ## mean cover by site across years
            sd.cover.sp = sd(mean.plot.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  select(-plot) %>%
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  
  mutate(mean.cover.sp_D = coalesce(mean.cover.sp_D, 0), 
         mean.cover.sp_C = coalesce(mean.cover.sp_C, 0)) %>%
  ## input 0 instead of NAs (NAs would be present where there is no cover of a particular species in either drought or control)
  
  group_by(site, species, block) %>%
  
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
         
         treatment.period = "PD")

## Merge ####
RR.tog <- rbind(drought.SE.RII, recov.SE.RII) %>%
  select(site, species, block, resp.ratio.site, SE.RII, treatment.period) %>%
  pivot_wider(names_from = treatment.period, values_from = c(resp.ratio.site, SE.RII))# %>%
 # mutate(drought.RR = ifelse(is.na(drought.RR), 0, drought.RR), ## taking this part out 11/1/24 as it seems like it is giving an artifical value for species when really there should juust be no value. 0 can be achieved several ways, so it's not right to put 0's in for species that just didn't have a value during a certain time period.

## although that being said, if there is a row for a species it was around at some point during the time period at that site.
## it was either not present at all during drought period in cntrol or treat plots but showed up during post-drought; or alternatively it was present in drought period but post-drought it was lost entirely 

## need to think more about hwether to add these in and if they mean what I think they do...
         #recovery.RR = ifelse(is.na(recovery.RR), 0, recovery.RR))

## merge with rank and persistence values for each species
edge_RR <- left_join(RR.tog, rank_persist, by = c("site", "species"))


# Clean up ####
rm(edge_all, drought.SE.RII, recov.SE.RII, RR.tog, edge_w_zeros, rank_persist, SEVcheck)
