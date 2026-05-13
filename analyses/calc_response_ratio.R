# Header #### 
## Script name: Calculate Response Ratio
##
##' Purpose of script: Calculate the response ratio between drought and 
##' control plots across years during two time periods (drought & recovery) for 
##' main analyses.
##' 
##' For sensitivity analyses, other response ratios are calculated: 
##'     Post-drought responses:  first two or final two years post-drought
##'     7 year drought response
##'     
##' Calculate the average response ratio across all zero filled subplots to get 
##' one average control and drought value per species per site.
##
## Author: Carmen Watkins

## References: 
## Response Ratio: Armas et al. 2004
## SE of Response Ratio: Armas et al. 2004 supplement A, 
## file:///C:/Users/carme/Downloads/appendixA.htm

# Set up ####
## read in cleaned data
source("data-prep/classify_rank_persistence.R")

## load packages
library(ggpubr)

# Resp Ratio ####
## Drought ####
### 4-year ####
drought.SE.RII4 <- edge_all %>%
  ## filter drought years
  ## 0 is pre-treat year; drought was years 1-4
  filter(experiment.year %in% c(1:4)) %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  pivot_wider(names_from = "treatment", 
              values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  ## calculate resp ratio & SE
  mutate(resp.ratio.site = 
           (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D),
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
         treatment.period = "D4") ## add in column to differentiate from post-drought RR

### 7-year ####
drought.SE.RII6 <- edge_all %>%
  filter(treatment.year == "drought") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  pivot_wider(names_from = "treatment", 
              values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  ## calculate resp ratio & SE
  mutate(resp.ratio.site =
           (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), 
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
         treatment.period = "D6") ## add in column to differentiate from post-drought RR

## Post-Drought ####
### full ####
## calculate the response ratio for the full 4-year post-drought response period
fullrecov.SE.RII <- edge_all %>%
  filter(treatment.year == "recovery") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  pivot_wider(names_from = "treatment", 
              values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D),
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
         treatment.period = "PDfull") ## add in column to differentiate from post-drought RR

### first ####
## calculate the response ratio for the first two years of the post-drought
## period only.
firstrecov.SE.RII <- edge_all %>%
  filter(recov.year == "initial") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  pivot_wider(names_from = "treatment", 
              values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D),
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
         treatment.period = "PDfirst") ## add in column to differentiate from post-drought RR

### final ####
## calculate the response ratio for the final two years of the post-drought
## period only.
finalrecov.SE.RII <- edge_all %>%
  filter(recov.year == "final") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  pivot_wider(names_from = "treatment", 
              values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  ungroup() %>%
  ## calculate resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D),
         
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
         treatment.period = "PDfinal")

## Merge ####
RR.tog = rbind(drought.SE.RII4, drought.SE.RII6, fullrecov.SE.RII, 
               firstrecov.SE.RII, finalrecov.SE.RII) %>%
  select(site, species, resp.ratio.site, SE.RII, treatment.period) %>%
  pivot_wider(names_from = treatment.period, 
              values_from = c(resp.ratio.site, SE.RII))

## merge with rank and persistence values for each species
edge_RR = left_join(RR.tog, rank_persist, by = c("site", "species"))

# Clean up ####
rm(edge_all, drought.SE.RII4, drought.SE.RII6, fullrecov.SE.RII,
   firstrecov.SE.RII, finalrecov.SE.RII, RR.tog, rank_persist)
