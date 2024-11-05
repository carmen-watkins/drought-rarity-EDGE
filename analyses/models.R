
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
### visualize
ggplot(edge_RR_cats, aes(x=percrank, y=resp.ratio.site_D)) +
  geom_point()

ggsave("analyses/model_figs/rankvDRR.png", width = 5, height = 4)

hist(edge_RR_cats$resp.ratio.site_D)

### run fixed effects model
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

DRR = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_D),
         !is.na(SE.RII_D))

## try weighted least squares regression
test = lm(resp.ratio.site_D ~ percrank*MAP_level, data = DRR, weights = 1/DRR$SE.RII_D)

summary(test)
Anova(test)

qqnorm(resid(test))
qqline(resid(test))


test_rfx =lmer(resp.ratio.site_D ~ percrank*MAP_level + (1|site) + (1|species), data = DRR, weights = 1/DRR$SE.RII_D)

summary(test_rfx)
Anova(test_rfx)

qqnorm(resid(test_rfx))
qqline(resid(test_rfx))

AIC(test)
AIC(test_rfx)


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




glm()







# OLD ####
# Rank models ####
## drought ####
rank_drought_model <- lm(resp.ratio.site_D~percrank*site*FunctionalGroup, data = edge_RR)

summary(rank_drought_model)
Anova(rank_drought_model)

qqnorm(resid(rank_drought_model))
qqline(resid(rank_drought_model))
plot(resid(rank_drought_model) ~ fitted(rank_drought_model))
## these don't look great... 

### *no functional group ####
rank_drought_model2 <- lm(resp.ratio.site_D~percrank*site, data = edge_RR)
summary(rank_drought_model2)
Anova(rank_drought_model2)

qqnorm(resid(rank_drought_model2))
qqline(resid(rank_drought_model2))

plot(resid(rank_drought_model2) ~ fitted(rank_drought_model2))

## recovery ####
rank_recov_model <- lm(recovery.RR~percrank*site*FunctionalGroup, data = edge_FG)

summary(rank_recov_model)
Anova(rank_recov_model)

qqnorm(resid(rank_recov_model))
qqline(resid(rank_recov_model))
plot(resid(rank_recov_model) ~ fitted(rank_recov_model))

### * no functional group ####
rank_recov_model2 <- lm(recovery.RR~percrank*site, data = edge_FG)

summary(rank_recov_model2)
Anova(rank_recov_model2)

qqnorm(resid(rank_recov_model2))
qqline(resid(rank_recov_model2))
plot(resid(rank_recov_model2) ~ fitted(rank_recov_model2))

# Persistence models ####
## drought ####
persist_drought_model <- lm(drought.RR~persistence.site*site*FunctionalGroup, data = edge_FG)
summary(persist_drought_model)
Anova(persist_drought_model)

qqnorm(resid(persist_drought_model))
qqline(resid(persist_drought_model))
plot(resid(persist_drought_model) ~ fitted(persist_drought_model))

### * no functional group ####
persist_drought_model2 <- lm(drought.RR~persistence.site*site, data = edge_FG)

summary(persist_drought_model2)
Anova(persist_drought_model2)

qqnorm(resid(persist_drought_model2))
qqline(resid(persist_drought_model2))
plot(resid(persist_drought_model2) ~ fitted(persist_drought_model2))

## recovery ####
persist_recov_model <- lm(recovery.RR~persistence.site*site*FunctionalGroup, data = edge_FG)

summary(persist_recov_model)
anova(persist_recov_model)

qqnorm(resid(persist_recov_model))
qqline(resid(persist_recov_model))
plot(resid(persist_recov_model) ~ fitted(persist_recov_model))

### no functional group ####
persist_recov_model2 <- lm(recovery.RR~persistence.site*site, data = edge_FG)

summary(persist_recov_model2)
Anova(persist_recov_model2)

qqnorm(resid(persist_recov_model2))
qqline(resid(persist_recov_model2))
plot(resid(persist_recov_model2) ~ fitted(persist_recov_model2))


# Rarity Category models ####

m1 = lm(recovery.RR~drought.RR+rarity_cat, data = edge_FG_cats)
summary(m1)

t = anova(m1)

TukeyHSD(t)
