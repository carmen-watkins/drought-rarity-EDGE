
# Set Up ####
source("analyses/calculate_response_ratio.R") 

library(lmerTest)
library(car)

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
