library(lmerTest)

## Do subordinate species respond differently than dominant species to drought and climate extremes? How does this vary over a precipitation gradient?

## asking here is the slope of the response ratio vs. rank different from 0? 
## if it is, then subordinate species do respond differently than dominant species?

## if this varies by site across the precip gradient, we would expect there to be slopes that differ from each other

yearly <- lmer(resp.ratio.site ~ percrank + site + (1|species), data = edge_yearly_w_predictors)
summary(yearly)

anova(yearly)


## When subordinate species respond, is it due to the environment (MAP or SPEI) or a competitive release (change in species richness or decline in dominant cover/biomass)?




## How do subordinate species recover from drought?

## Are drought legacy effects due to subordinate species responses?
  



## models
yearly <- lmer(resp.ratio.site ~ percrank + site + treatment.year + (1|species), data = edge_yearly_w_predictors)
summary(yearly)

anova(yearly)


anova(lmer(resp.ratio.site ~ percrank + site + tot.precip + (1|species), data = rr.precip))

rr.precip

yearly2 <- lmer(resp.ratio.site ~ percrank + site + experiment.year + (1|species), data = edge_yearly_w_predictors)
summary(yearly2)


all_factors <- lmer(resp.ratio.site~persistence.site*site*percrank + (1|species), data = edge_w_predictors.site)
car::Anova(all_factors)


#Do dominant and subordinate species recover differently from drought? 
m2<-lmer(resp.ratio.site~persistence.site*site*percrank + (1|species), data = edge_w_predictors.site.recov)
car::Anova(m2)




colnames(RR.slope.rich)

map <- lm(slope.drought.rank~MAP.mm, data = RR.slope.rich)
summary(map)

rich.model <- lm(slope.drought.rank~SR, data = RR.slope.rich)
summary(rich.model)


map2 <- lm(slope.recov.rank~MAP.mm, data = RR.slope.rich)
summary(map2)

rich.model2 <- lm(slope.recov.rank~SR, data = RR.slope.rich)
summary(rich.model2)


map3 <- lm(slope.drought.persistence~MAP.mm, data = RR.slope.rich)
summary(map3)

rich.model3 <- lm(slope.drought.persistence~SR, data = RR.slope.rich)
summary(rich.model3)


map4 <- lm(slope.recov.persistence~MAP.mm, data = RR.slope.rich)
summary(map4)

rich.model4 <- lm(slope.recov.persistence~SR, data = RR.slope.rich)
summary(rich.model4)
