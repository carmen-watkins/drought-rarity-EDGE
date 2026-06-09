# Header ####
##' Script name: Q1 Mixed Models
##'
##' Purpose of script: Run linear mixed effects models to test the overall 
##' effect of rarity on response ratio during drought and post-drought for
##' spatial and temporal rarity, that still accounts for effect of site.
##' 
##' Author: Carmen Watkins
##'

# Set up ####
## load packages
library(performance)
library(tidyverse)
library(car)
library(lmerTest)
library(MuMIn)

## load data
edge_RR = read.csv("data/edge_response_ratio_and_rarity.csv")

# Model ####
## Drought, Spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = edge_RR)
#check_model(mmsd)
summary(mmsd) 
## in-text results
Anova(mmsd, type = 2, test.statistic = "F")

## effect sizes
summary(mmsd)$coefficients ## SR: 0.8749143
confint(mmsd) ## SR: 0.67172472  1.0763269
r.squaredGLMM(mmsd)

## Drought, Temporal ####
mmtd = lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = edge_RR)
#check_model(mmtd)
summary(mmtd) 
## in-text results
Anova(mmtd, type = 2, test.statistic = "F")

## calc effect sizes
summary(mmtd)$coefficients ## SR: 0.7194338
confint(mmtd) ## SR: 0.55313000 0.88892308
r.squaredGLMM(mmtd)

## Post-drought Spatial ####
mmsp = lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = edge_RR)
#check_model(mmsp)
summary(mmsp) ## supp table
## in-text results
Anova(mmsp, type = 2, test.statistic = "F")

## effect sizes
summary(mmsp)$coefficients ## SR: 0.7602471
confint(mmsp) ## SR: 0.55780617  0.96531261
r.squaredGLMM(mmsp)

## Post-drought Temporal ####
mmtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = edge_RR)
#check_model(mmtp)
summary(mmtp) ## supp table
Anova(mmtp, type = 2, test.statistic = "F")

## calc effect sizes
summary(mmtp)$coefficients ## SR: 0.7602471
confint(mmtp)
r.squaredGLMM(mmtp)

# Table 1 ####
mmsd_coeff = as.data.frame(summary(mmsd)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_coeff = as.data.frame(summary(mmtd)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_coeff = as.data.frame(summary(mmsp)$coefficients)%>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_coeff = as.data.frame(summary(mmtp)$coefficients) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

coeff_df = rbind(mmsd_coeff, mmtd_coeff, mmsp_coeff, mmtp_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, 
         `Pr(>|t|)`) %>%
  ## round all numbers to 2 sigfigs except p-value
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 3))

#write.csv(coeff_df, "tables/review_tabs/Q1_mixed_mod_coeff_Tab1.csv", 
 #         row.names = F)
