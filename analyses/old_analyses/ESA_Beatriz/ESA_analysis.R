################################
# Further data analysis by B
################################
#ESA Talk Questions:
# 1-Do subordinate species respond differently than dominant species to drought? 
# 2-Do subordinate species responses to drought vary over a precipitation gradient(Start w/ MAP check if SPEI)?

#Load Required libraries
library(lme4)
library(lmerTest)

source("calculate_response_ratio.R")
#Note: Carmen's models in script titled: "response_ratio_models


######
#Analysis for Question 1: Do subordinate species respond differently than dominant species to drought? 

#Look at data distribution of resp.ratio.site
hist(edge_w_predictors.site$resp.ratio.site)
#data looks normally distributed


all_factors <- lmer(resp.ratio.site~persistence.site*site*percrank + (1|species), data = edge_w_predictors.site)
car::Anova(all_factors)


#Do dominant and subordinate species recover differently from drought? 
m2<-lmer(resp.ratio.site~persistence.site*site*percrank + (1|species), data = edge_w_predictors.site.recov)
car::Anova(m2)

