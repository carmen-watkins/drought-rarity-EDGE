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
library(lmerTest)

library(jtools)

# Try mixed model ####
## try this model as way of estimating the overall effect of rarity on response ratio during drought and postdrought for spatial and temporal rarity.
## drought, spatial
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Anova(mmsd, type = 3) ## main table

## drought, temporal
mmtd = lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtd)
summary(mmtd) ## supp table
Anova(mmtd, type = 3) ## main table

## post-drought spatial
mmsp = lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsp)
summary(mmsp) ## supp table
Anova(mmsp, type = 3) ## main table

## post-drought temporal
mmtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtp)
summary(mmtp) ## supp table
Anova(mmtp, type = 3) ## main table

## plot 
p1 = effect_plot(mmsd, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")

p2 = effect_plot(mmtd, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = " ", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")

p3 = effect_plot(mmsp, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Post-Drought Response Ratio", x.label = "Spatial Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")

p4 = effect_plot(mmtp, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = " ", x.label = "Temporal Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(p1, p2, p3, p4)

## ggsave("figures/Jan2025/resp_ratio_v_rarity_mmfit.png", width = 6, height = 5.5)


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
