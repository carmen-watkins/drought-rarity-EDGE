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
hist(edge_RR_cats$percrank)

## drought ####
### visualize ####
ggplot(edge_RR_cats, aes(x=percrank, y=resp.ratio.site_D)) +
  geom_point()
#ggsave("analyses/model_figs/rankvDRR.png", width = 5, height = 4)

hist(edge_RR_cats$resp.ratio.site_D)

### fixed effects model ####
rd_fe = lm(resp.ratio.site_D~percrank*MAP_level, data = edge_RR_cats)
summary(rd_fe)
Anova(rd_fe)

### diagnostics, fixed effects model
qqnorm(resid(rd_fe))
qqline(resid(rd_fe))

plot(resid(rd_fe) ~ fitted(rd_fe))

plot(rstandard(rd_fe) ~ edge_RR_cats[!is.na(edge_RR_cats$resp.ratio.site_D),]$resp.ratio.site_D)

## perform Breusch-Pagan test for heteroscedasticity
bptest(rd_fe)

## null = homoscedasticity; residuals distrib with equal variance
## alt hypo = heteroscedasticity present
## p=val = 0.000066; heteroscedasticity is a problem in this model

### random effects model ####
rd_re =lmer(resp.ratio.site_D ~ percrank*MAP_level + (1|site) + (1|species), data = edge_RR_cats)
summary(rd_re)
Anova(rd_re)

qqnorm(resid(rd_re))
qqline(resid(rd_re))

AIC(rd_re)
plot(resid(rd_re) ~ fitted(rd_re))

### weighted analysis ####
## first, remove NAs
DRR = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_D),
         !is.na(SE.RII_D))

## quantify NAs
NAcount_site = edge_RR_cats %>%
  mutate(NA.SE = ifelse(is.na(SE.RII_D), "Y", "N"),
         NA.RII = ifelse(is.na(resp.ratio.site_D), "Y", "N")) %>%
  group_by(site, MAP_level, NA.SE, NA.RII) %>%
  summarise(num_obs = n())

NAcount_MAP = edge_RR_cats %>%
  mutate(NA.SE = ifelse(is.na(SE.RII_D), "Y", "N"),
         NA.RII = ifelse(is.na(resp.ratio.site_D), "Y", "N")) %>%
  group_by(MAP_level, NA.SE, NA.RII) %>%
  summarise(num_obs = n())

NAcountoverall = edge_RR_cats %>%
  mutate(NA.SE = ifelse(is.na(SE.RII_D), "Y", "N"),
         NA.RII = ifelse(is.na(resp.ratio.site_D), "Y", "N")) %>%
  group_by(NA.SE, NA.RII) %>%
  summarise(num_obs = n())

ggplot(DRR, aes(x=percrank, y=persistence.site)) +
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_vline(xintercept = 0.5, color = "gray") +
  geom_point(size = 1.5) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse()

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
rd_wre =lmer(resp.ratio.site_D ~ percrank*MAP_level + (1|site) + (1|species), data = DRR, weights = 1/DRR$SE.RII_D)

## lmer, better for fitting
## refit using lme for AIC comparison
## supplement with other model options; otherwise choose best

summary(rd_re)
Anova(rd_re)

qqnorm(resid(rd_re))
qqline(resid(rd_wre))

AIC(rd_re)
#plot(rstandard(rd_wre) ~ fitted(rd_wre))
plot(resid(rd_re) ~ fitted(rd_re))

## post-drought ####
ggplot(edge_RR_cats, aes(x=percrank, y=resp.ratio.site_PD)) +
  geom_point()

ggsave("analyses/model_figs/rankvPDRR.png", width = 5, height = 4)

hist(edge_RR_cats$resp.ratio.site_PD)

### run fixed effects model
rpd_fe = lm(resp.ratio.site_PD~percrank*MAP_level, data = edge_RR_cats)
summary(rpd_fe)
Anova(rpd_fe)

### diagnostics, fixed effects model
qqnorm(resid(rpd_fe))
qqline(resid(rpd_fe))

plot(resid(rpd_fe) ~ fitted(rpd_fe))



# Persistence ####
hist(edge_RR_cats$persistence.site)

## drought ####
### visualize
ggplot(edge_RR_cats, aes(x=persistence.site, y=resp.ratio.site_D)) +
  geom_point()

ggsave("analyses/model_figs/persvDRR.png", width = 5, height = 4)

### run fixed effects model
pd_fe = lm(resp.ratio.site_D~persistence.site*MAP_level, data = edge_RR_cats)
summary(pd_fe)
Anova(pd_fe)

### diagnostics, fixed effects model
qqnorm(resid(pd_fe))
qqline(resid(pd_fe))

plot(resid(pd_fe) ~ fitted(pd_fe))


## post-drought ####
ggplot(edge_RR_cats, aes(x=persistence.site, y=resp.ratio.site_PD)) +
  geom_point()

ggsave("analyses/model_figs/persvPDRR.png", width = 5, height = 4)

hist(edge_RR_cats$resp.ratio.site_PD)

### run fixed effects model
ppd_fe = lm(resp.ratio.site_PD~persistence.site*MAP_level, data = edge_RR_cats)
summary(ppd_fe)
Anova(ppd_fe)

### diagnostics, fixed effects model
qqnorm(resid(ppd_fe))
qqline(resid(ppd_fe))

plot(resid(ppd_fe) ~ fitted(ppd_fe))






































summary(edge_FG_cats$drought.RR - rd_mod$fitted.values)

rem = lmer(resp.ratio.site_D~percrank*MAP_level + (1|species), data = edge_RR_cats)

summary(rem)
Anova(rem)

qqnorm(resid(rem))
qqline(resid(rem))

AIC(rem)
AIC(rd_mod)


summary(edge_FG_cats$drought.RR - rd_mod$fitted.values)

Anova(rd_mod)

qqnorm(resid(rd_mod))
qqline(resid(rd_mod))

plot(resid(rd_mod) ~ fitted(rd_mod))
## decreasing variance in the residuals
## this model is not a good fit for the data

## post-drought ####
plot(x=edge_FG_cats$percrank, y=edge_FG_cats$recovery.RR)

rpd_mod = lm(recovery.RR~percrank*MAP_level, data = edge_FG_cats)
summary(rpd_mod)

summary(edge_FG_cats$recovery.RR - rpd_mod$fitted.values)

Anova(rpd_mod)

qqnorm(resid(rpd_mod))
qqline(resid(rpd_mod))

plot(resid(rpd_mod) ~ fitted(rpd_mod))
## decreasing variance in the residuals
## this model is not a good fit for the data

# Persistence ####
## drought ####
plot(x=edge_FG_cats$persistence.site, y=edge_FG_cats$drought.RR)

pd_mod = lm(drought.RR~persistence.site*MAP_level, data = edge_FG_cats)
summary(pd_mod)

summary(edge_FG_cats$drought.RR - pd_mod$fitted.values)

Anova(pd_mod)

qqnorm(resid(pd_mod))
qqline(resid(pd_mod))

plot(resid(pd_mod) ~ fitted(pd_mod))
## decreasing variance in the residuals
## this model is not a good fit for the data

## post-drought ####
plot(x=edge_FG_cats$persistence.site, y=edge_FG_cats$recovery.RR)

ppd_mod = lm(recovery.RR~persistence.site*MAP_level, data = edge_FG_cats)
summary(ppd_mod)

summary(edge_FG_cats$recovery.RR - ppd_mod$fitted.values)

Anova(ppd_mod)

qqnorm(resid(ppd_mod))
qqline(resid(ppd_mod))

plot(resid(ppd_mod) ~ fitted(ppd_mod))
## decreasing variance in the residuals
## this model is not a good fit for the data
