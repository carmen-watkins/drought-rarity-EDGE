
# Set Up ####
source("analyses/calculate_response_ratio.R") 

library(lmerTest)
library(car)

## categorize species and sites 
edge_FG_cats = edge_FG %>%
  mutate(spatial = ifelse(percrank > 0.5, "Abundant", "Scarce"),
         temporal = ifelse(persistence.site > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low")))

# Rank ####
## drought ####
plot(x=edge_FG_cats$percrank, y=edge_FG_cats$drought.RR)

rd_mod = lm(drought.RR~percrank*MAP_level, data = edge_FG_cats)
summary(rd_mod)

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
rank_drought_model <- lm(drought.RR~percrank*site*FunctionalGroup, data = edge_FG)

summary(rank_drought_model)
Anova(rank_drought_model)

qqnorm(resid(rank_drought_model))
qqline(resid(rank_drought_model))
plot(resid(rank_drought_model) ~ fitted(rank_drought_model))
## these don't look great... 

### *no functional group ####
rank_drought_model2 <- lm(drought.RR~percrank*site, data = edge_FG)
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
