# Set up env ####
library(lme4)

source("calculate_response_ratio.R")

## Model everything together

all_factors <- lmer(resp.ratio.site~persistence.site*site*percrank + (1|species), data = edge_w_predictors.site)
summary(all_factors)

persistence <- lm(resp.ratio.site~persistence.site, data = edge_w_predictors.site)
summary(persistence)

persistence.block <- lm(resp.ratio.block~persistence.plot, data = edge_w_predictors.block)
summary(persistence.block)

rank <- lm(resp.ratio.site~percrank, data = edge_w_predictors.site)
summary(rank)
