# Header #### 
## Script name: Response Ratio Yearly
##
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots to rank & persistence yearly & by SPEI
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up Env ####
library(ggpubr)

## read in response ratio calcs
source("analyses/calculate_response_ratio.R")
source("data-prep/clean_ppt_data.R")
theme_set(theme_classic())

edge_yearly_w_years <- left_join(edge_yearly_w_predictors, year.key, by = c("site", "experiment.year", "treatment.year"))

edge_yearly_w_precip <- left_join(edge_yearly_w_years, growing.season.tot, by = c("site", "year"))


colnames(edge_yearly_w_precip)

## species level response ratio vs. growing season precipitation
ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "KNZ" & !is.na(edge_yearly_w_precip$resp.ratio.site),], aes(x=tot.precip, y=resp.ratio.site)) +
  geom_point() +
  facet_wrap(~species) +
  geom_smooth(method = "lm")

ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "CHY" & !is.na(edge_yearly_w_precip$resp.ratio.site),], aes(x=tot.precip, y=resp.ratio.site)) +
  geom_point() +
  facet_wrap(~species) +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(-1,1))

