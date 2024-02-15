library(lmerTest)

## rank models
## drought
rank_drought_model <- lm(drought.RR~percrank*site*FunctionalGroup, data = edge_FG)

summary(rank_drought_model)
anova(rank_drought_model)

qqnorm(resid(rank_drought_model))
qqline(resid(rank_drought_model))

## these don't look great... 

plot(rank_drought_model, resid(rank_drought_model) ~ fitted(rank_drought_model))

## recovery
rank_recov_model <- lm(recovery.RR~percrank*site*FunctionalGroup, data = edge_FG)

summary(rank_recov_model)
anova(rank_recov_model)

qqnorm(resid(rank_recov_model))
qqline(resid(rank_recov_model))

## persistence models
## drought
persist_drought_model <- lm(drought.RR~persistence.site*site*FunctionalGroup, data = edge_FG)

summary(persist_drought_model)
anova(persist_drought_model)

qqnorm(resid(persist_drought_model))
qqline(resid(persist_drought_model))

## recovery
persist_recov_model <- lm(recovery.RR~persistence.site*site*FunctionalGroup, data = edge_FG)

summary(persist_recov_model)
anova(persist_recov_model)

qqnorm(resid(persist_recov_model))
qqline(resid(persist_recov_model))

