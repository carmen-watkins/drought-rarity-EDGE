# Header #### 
## Script name: Models
##
## Purpose of script: Run models to test the relationship between response ratios and rank, persistence, MAP level, site, etc.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

## References: 
## Response Ratio: Armas et al. 2004
## SE of Response Ratio: Armas et al. 2004 supplement A, file:///C:/Users/carme/Downloads/appendixA.htm

# Set Up ####
source("analyses/calculate_response_ratio.R") 

library(lmerTest)
library(car)
library(lmtest)

## categorize species and sites 
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(percrank > 0.5, "Abundant", "Scarce"),
         temporal = ifelse(persistence.site > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low")))

# Rank ####
## drought ####
DRR1 = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_D))

### fixed effects model ####
rd_fe = lm(resp.ratio.site_D~percrank*MAP_level, data = DRR1)
summary(rd_fe)
Anova(rd_fe)

### diagnostics, fixed effects model
qqnorm(resid(rd_fe))
qqline(resid(rd_fe))

plot(resid(rd_fe) ~ fitted(rd_fe))

## perform Breusch-Pagan test for heteroscedasticity
bptest(rd_fe)
AIC(rd_fe)

## null = homoscedasticity; residuals distrib with equal variance
## alt hypo = heteroscedasticity present
## p=val = 0.000066; heteroscedasticity is a problem in this model

### random effects model ####
rd_re =lmer(resp.ratio.site_D ~ percrank*MAP_level  + (1|site) + (1|block) + (1|species), data = DRR1)
summary(rd_re)
Anova(rd_re, type =  "III")

qqnorm(resid(rd_re))
qqline(resid(rd_re))

AIC(rd_re)
plot(resid(rd_re) ~ fitted(rd_re))

### weighted analysis ####
## first, remove NAs
DRR2 = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_D),
         !is.na(SE.RII_D))

#### fixed fx ####
## try weighted least squares regression
rd_wfe = lm(resp.ratio.site_D ~ percrank*MAP_level, data = DRR, weights = 1/DRR$SE.RII_D)

summary(rd_wfe)
Anova(rd_wfe)

qqnorm(resid(rd_wfe))
qqline(resid(rd_wfe))
AIC(rd_wfe)
## 595.8389

plot(rstandard(rd_wfe) ~ fitted(rd_wfe))
plot(resid(rd_wfe) ~ fitted(rd_wfe))


#### random fx ####
rd_wre =lmer(resp.ratio.site_D ~ percrank*MAP_level + (1|species), data = DRR2, weights = 1/DRR2$SE.RII_D)

## lmer, better for fitting
## refit using lme for AIC comparison
## supplement with other model options; otherwise choose best

summary(rd_wre)
Anova(rd_wre)

qqnorm(resid(rd_wre))
qqline(resid(rd_wre))

AIC(rd_wre)
plot(resid(rd_wre) ~ fitted(rd_wre))

## post-drought ####
PDRR1 = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_PD))

### fixed effects model ####
rpd_fe = lm(resp.ratio.site_PD~percrank*MAP_level, data = edge_RR_cats)
summary(rpd_fe)
Anova(rpd_fe)

### diagnostics, fixed effects model
qqnorm(resid(rpd_fe))
qqline(resid(rpd_fe))

plot(resid(rpd_fe) ~ fitted(rpd_fe))

### random effects model ####
rpd_re = lmer(resp.ratio.site_PD~percrank*MAP_level + (1|site) + (1|block) + (1|species), data = PDRR1)

summary(rpd_re)
Anova(rpd_re, type = "III")

### diagnostics, fixed effects model
qqnorm(resid(rpd_re))
qqline(resid(rpd_re))

plot(resid(rpd_re) ~ fitted(rpd_re))

### weighted analysis ####
## first, remove NAs
PDRR = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_PD),
         !is.na(SE.RII_PD))

#### fixed fx ####
## try weighted least squares regression
rpd_wfe = lm(resp.ratio.site_PD ~ percrank*MAP_level, data = PDRR, weights = 1/PDRR$SE.RII_PD)

summary(rpd_wfe)
Anova(rpd_wfe)

qqnorm(resid(rpd_wfe))
qqline(resid(rpd_wfe))
AIC(rpd_wfe)

plot(rstandard(rpd_wfe) ~ fitted(rpd_wfe))
plot(resid(rpd_wfe) ~ fitted(rpd_wfe))


#### random fx ####
rpd_wre =lmer(resp.ratio.site_PD ~ percrank*MAP_level + (1|site) + (1|block) + (1|species), data = PDRR, weights = 1/PDRR$SE.RII_PD)

summary(rpd_wre)
Anova(rpd_wre)

qqnorm(resid(rpd_wre))
qqline(resid(rpd_wre))

AIC(rpd_wre)
#plot(rstandard(rd_wre) ~ fitted(rd_wre))
plot(resid(rd_wre) ~ fitted(rd_wre))



# Persistence ####
hist(edge_RR_cats$persistence.site)

## drought ####
### visualize
### fixed effects model ####
pd_fe = lm(resp.ratio.site_D~persistence.site*MAP_level, data = edge_RR_cats)
summary(pd_fe)
Anova(pd_fe)

### diagnostics, fixed effects model
qqnorm(resid(pd_fe))
qqline(resid(pd_fe))

plot(resid(pd_fe) ~ fitted(pd_fe))

## perform Breusch-Pagan test for heteroscedasticity
bptest(pd_fe)
AIC(pd_fe)

## null = homoscedasticity; residuals distrib with equal variance
## alt hypo = heteroscedasticity present
## p=val = 0.000066; heteroscedasticity is a problem in this model

### random effects model ####
pd_re =lmer(resp.ratio.site_D ~ persistence.site*MAP_level  + (1|site) + (1|block) + (1|species), data = DRR1)

summary(pd_re)
Anova(pd_re, type = "III")

qqnorm(resid(pd_re))
qqline(resid(pd_re))

AIC(pd_re)
plot(resid(pd_re) ~ fitted(pd_re))

### weighted analysis ####
## first, remove NAs
DRR = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_D),
         !is.na(SE.RII_D))

#### fixed fx ####
## try weighted least squares regression
rd_wfe = lm(resp.ratio.site_D ~ percrank*MAP_level, data = DRR, weights = 1/DRR$SE.RII_D)

summary(rd_wfe)
Anova(rd_wfe)

qqnorm(resid(rd_wfe))
qqline(resid(rd_wfe))
AIC(rd_wfe)
## 595.8389

plot(rstandard(rd_wfe) ~ fitted(rd_wfe))
plot(resid(rd_wfe) ~ fitted(rd_wfe))


#### random fx ####
rd_wre =lmer(resp.ratio.site_D ~ percrank*MAP_level + (1|site) + (1|block) + (1|species), data = DRR, weights = 1/DRR$SE.RII_D)

## lmer, better for fitting
## refit using lme for AIC comparison
## supplement with other model options; otherwise choose best

summary(rd_wre)
Anova(rd_wre)

qqnorm(resid(rd_wre))
qqline(resid(rd_wre))

AIC(rd_wre)
#plot(rstandard(rd_wre) ~ fitted(rd_wre))
plot(resid(rd_wre) ~ fitted(rd_wre))

## post-drought ####
### run fixed effects model ####
ppd_fe = lm(resp.ratio.site_PD~persistence.site*MAP_level, data = edge_RR_cats)
summary(ppd_fe)
Anova(ppd_fe)

### diagnostics, fixed effects model
qqnorm(resid(ppd_fe))
qqline(resid(ppd_fe))

plot(resid(ppd_fe) ~ fitted(ppd_fe))

### random effects model ####
ppd_re = lmer(resp.ratio.site_PD~persistence.site*MAP_level + (1|site) + (1|block) + (1|species), data = PDRR1)

summary(ppd_re)
Anova(ppd_re, type = "III")

### diagnostics, fixed effects model
qqnorm(resid(rpd_re))
qqline(resid(rpd_re))

plot(resid(rpd_re) ~ fitted(rpd_re))





