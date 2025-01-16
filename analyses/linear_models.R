# Header ####
## Script name: Linear Models

## Purpose of script: Run linear models to test effects of rarity and site on response ratios
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
library(performance)
library(parameters)
library(tidyverse)
library(car)

library(jtools)

# Explore Data ####
ggplot(edge_RR, aes(x=resp.ratio.site_D4)) +
  geom_histogram()

ggplot(edge_RR, aes(x=resp.ratio.site_PDfull)) +
  geom_histogram()

ggplot(edge_RR, aes(x=site, y=spatial_rarity))+
  geom_violin() +
  geom_jitter() +
  geom_boxplot(width = 0.1)

ggplot(edge_RR, aes(x=site, y=temporal_rarity))+
  geom_violin() +
  geom_jitter()

## check whether site predicts rarity
m1rarsite = lm(spatial_rarity ~ site, data = edge_RR)
summary(m1rarsite)
Anova(m1rarsite)
## not for spatial rarity

m2rarsite = lm(temporal_rarity ~ site, data = edge_RR)
summary(m2rarsite)
Anova(m2rarsite)
## yes for temporal rarity

# Drought ####
## Spatial ####
## 3 model options
md4s_all = lm(resp.ratio.site_D4 ~ spatial_rarity*site, data = edge_RR)
md4s_add = lm(resp.ratio.site_D4 ~ spatial_rarity + site, data = edge_RR)
md4s_int = lm(resp.ratio.site_D4 ~ spatial_rarity + spatial_rarity:site, data = edge_RR)

plot_summs(md4s_all, md4s_add, md4s_int)

summary(md4s_all)$coefficients

### indiv site models ####
ms1 = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "KNZ",])
summary(ms1)
ms2 = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "HYS",])
summary(ms2)
ms3 = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "CHY",])
summary(ms3)
ms4 = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "SGS",])
summary(ms4)
ms5 = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "SBL",])
summary(mt5)
ms6 = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "SBK",])
summary(mt6)

plot_summs(ms1, ms2, ms3, ms4, ms5, ms6)

## see what models are actually doing 
model.matrix(md4s_all)
#model.matrix(md4s_add)
#model.matrix(md4s_int)

## compare
### check model
check_model(md4s_all)
#check_model(md4s_add)
#check_model(md4s_int)

### AIC 
AIC(md4s_all)
#AIC(md4s_add)
#AIC(md4s_int)

### anova
#anova(md4s_all, md4s_add, md4s_int)
## model 2 (md4s_add) wins by AIC and by anova and by low VIF

## Temporal ####
## 3 model options
md4t_all = lm(resp.ratio.site_D4 ~ temporal_rarity*site, data = edge_RR)
#md4t_add = lm(resp.ratio.site_D4 ~ temporal_rarity + site, data = edge_RR)
#md4t_int = lm(resp.ratio.site_D4 ~ temporal_rarity + temporal_rarity:site, data = edge_RR)

## compare
### check model
check_model(md4t_all) ## high collinearity
#check_model(md4t_add)
#check_model(md4t_int)

### AIC 
AIC(md4t_all) ## 722.1
#AIC(md4t_add) ## 736.1
#AIC(md4t_int) ## 727.3

#anova(md4t_all, md4t_add, md4t_int)
## selects model 2 (md4t_add) with significant p-value

# Post-Drought ####
## Spatial ####
## 3 model options
mpd4s_all = lm(resp.ratio.site_PDfull ~ spatial_rarity*site, data = edge_RR)
#mpd4s_add = lm(resp.ratio.site_PDfull ~ spatial_rarity + site, data = edge_RR)
#mpd4s_int = lm(resp.ratio.site_PDfull ~ spatial_rarity + spatial_rarity:site, data = edge_RR)

## compare
### check model
check_model(mpd4s_all)
#check_model(mpd4s_add)
#check_model(mpd4s_int)

### AIC 
AIC(mpd4s_all)
#AIC(mpd4s_add)
#AIC(mpd4s_int)

### anova
#anova(mpd4s_all, mpd4s_add, mpd4s_int)

## Temporal ####
## 3 model options
mpd4t_all = lm(resp.ratio.site_PDfull ~ temporal_rarity*site, data = edge_RR)
#mpd4t_add = lm(resp.ratio.site_PDfull ~ temporal_rarity + site, data = edge_RR)
#mpd4t_int = lm(resp.ratio.site_PDfull ~ temporal_rarity + temporal_rarity:site, data = edge_RR)

## compare
### check model
check_model(mpd4t_all)
#check_model(mpd4t_add)
#check_model(mpd4t_int)

### AIC
AIC(mpd4t_all) 
#AIC(mpd4t_add)
#AIC(mpd4t_int)

### anova
#anova(mpd4t_all, mpd4t_add, mpd4t_int)
