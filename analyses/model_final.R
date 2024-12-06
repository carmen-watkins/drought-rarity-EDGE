# Header #### 
## Script name: Model Final
##
## Purpose of script: Run mixed effects models 
##
## use zero-filled data at subplot level
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
library(lmerTest)
library(viridisLite)
library(visreg)
library(rgl)
library(ggpubr)
library(xtable)

## load response ratio data
source("analyses/calc_response_ratio.R") 

## load site level predictors
source("data-prep/prep_model_predictors.R")

## join data
edge_RR_preds = left_join(edge_RR, site_pred_scaled, by = "site")

## create df's for modeling 
DRR4 = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_D4))

DRR6 = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_D6))

PDRRfull = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_PDfull))

PDRRfirst = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_PDfirst))

PDRRfinal = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_PDfinal))

# Drought ####
## 4-year ####
### Spatial Rarity ####
## start with most conservative model
md4s = lmer(resp.ratio.site_D4 ~ spatial_rarity + z_temp*z_precip + dom.rounded + spatial_rarity:z_precip + spatial_rarity:dom.rounded + (spatial_rarity|site), data = DRR4)

## summarise and create table
summary(md4s)
anova(md4s)
print(xtable(anova(md4s)))
print(xtable(summary(md4s)$coefficients))

## try alternative model with random intercept but no random slope
md4s_alt = lmer(resp.ratio.site_D4 ~ spatial_rarity + z_temp*z_precip + BP.dom.site + spatial_rarity:z_precip + spatial_rarity:BP.dom.site + (1|site), data = DRR4)
summary(md4s_alt)
anova(md4s_alt)

## try model with precip specified for particular interval
md4s_pi = lmer(resp.ratio.site_D4 ~ spatial_rarity + z_temp*z_precipDRR4 + BP.dom.site + spatial_rarity:z_precipDRR4 + spatial_rarity:BP.dom.site + (spatial_rarity|site), data = DRR4)

summary(md4s_pi)
anova(md4s_pi)
print(xtable(anova(md4s_pi)))

### Temporal Rarity ####
md4t = lmer(resp.ratio.site_D4 ~ temporal_rarity + z_temp*z_precip + BP.dom.site + temporal_rarity:z_precip + temporal_rarity:BP.dom.site + (temporal_rarity|site), data = DRR4)

summary(md4t)
anova(md4t)

print(xtable(anova(md4t)))
print(xtable(summary(md4t)$coefficients))

## 6-year ####
### Spatial Rarity ####
md6s = lmer(resp.ratio.site_D6 ~ spatial_rarity + z_temp*z_precip + BP.dom.site + spatial_rarity:z_precip + spatial_rarity:BP.dom.site + (spatial_rarity|site), data = DRR6)

summary(md6s)
anova(md6s)

print(xtable(anova(md6s)))

### Temporal Rarity ####
md6t = lmer(resp.ratio.site_D6 ~ temporal_rarity + z_temp*z_precip + BP.dom.site + temporal_rarity:z_precip + temporal_rarity:BP.dom.site + (temporal_rarity|site), data = DRR6)

summary(md6t)
anova(md6t)

print(xtable(anova(md6t)))

# Post-Drought ####
## full ####
### Spatial Rarity ####
mpds_full = lmer(resp.ratio.site_PDfull ~ spatial_rarity + z_temp*z_precip + BP.dom.site + spatial_rarity:z_precip + spatial_rarity:BP.dom.site + (spatial_rarity|site), data = PDRRfull)
# ISSUE ####
## model failed to converge, negative eigen value
summary(mpds_full)
anova(mpds_full)

print(xtable(anova(mpds_full)))
print(xtable(summary(mpds_full)$coefficients))

mpds_fullalt = lmer(resp.ratio.site_PDfull ~ spatial_rarity + z_temp*z_precip + BP.dom.site + spatial_rarity:z_precip + spatial_rarity:BP.dom.site + (1|site), data = PDRRfull)

summary(mpds_fullalt)
anova(mpds_fullalt)

### Temporal Rarity ####
mpdt_full = lmer(resp.ratio.site_PDfull ~ temporal_rarity + z_temp*z_precip + BP.dom.site + temporal_rarity:z_precip + temporal_rarity:BP.dom.site + (temporal_rarity|site), data = PDRRfull)

summary(mpdt_full)
anova(mpdt_full)

print(xtable(anova(mpdt_full)))
print(xtable(summary(mpdt_full)$coefficients))

## first ####
### Spatial Rarity ####
mpds_first = lmer(resp.ratio.site_PDfirst ~ spatial_rarity + z_temp*z_precip + BP.dom.site + spatial_rarity:z_precip + spatial_rarity:BP.dom.site + (spatial_rarity|site), data = PDRRfirst)

summary(mpds_first)
anova(mpds_first)

print(xtable(anova(mpds_first)))

### Temporal Rarity ####
mpdt_first = lmer(resp.ratio.site_PDfirst ~ temporal_rarity + z_temp*z_precip + BP.dom.site + temporal_rarity:z_precip + temporal_rarity:BP.dom.site + (temporal_rarity|site), data = PDRRfirst)

summary(mpdt_first)
anova(mpdt_first)

print(xtable(anova(mpdt_first)))

## final ####
### Spatial Rarity ####
mpds_final = lmer(resp.ratio.site_PDfinal ~ spatial_rarity + z_temp*z_precip + BP.dom.site + spatial_rarity:z_precip + spatial_rarity:BP.dom.site + (spatial_rarity|site), data = PDRRfinal)

summary(mpds_final)
anova(mpds_final)

print(xtable(anova(mpds_final)))

### Temporal Rarity ####
mpdt_final = lmer(resp.ratio.site_PDfinal ~ temporal_rarity + z_temp*z_precip + BP.dom.site + temporal_rarity:z_precip + temporal_rarity:BP.dom.site + (temporal_rarity|site), data = PDRRfinal)

summary(mpdt_final)
anova(mpdt_final)

print(xtable(anova(mpdt_final)))
