## Header ####
## Script name: Q1B PD split Mixed Models

## Purpose of script: Run linear mixed effects models to test whether the first two or final years of the post-drought period influence recovery 
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)

library(jtools)
library(xtable)

source("analyses/calc_response_ratio.R") 
source("analyses/color_palettes.R")


edge_PDlong = edge_RR %>%
  select(site, species, resp.ratio.site_PDfirst, resp.ratio.site_PDfinal, spatial_rarity, temporal_rarity) %>%
  pivot_longer(cols = c("resp.ratio.site_PDfirst", "resp.ratio.site_PDfinal"), names_to = "PD_period", values_to = "resp.ratio")

mmpdss = lmer(resp.ratio ~ spatial_rarity + PD_period + (1|site), data = edge_PDlong)
summary(mmpdss)

Anova(mmpdss)

mmpdts = lmer(resp.ratio ~ temporal_rarity + PD_period + (1|site), data = edge_PDlong)
summary(mmpdts)

Anova(mmpdts)

## Table ####
mmpdss_tab = as.data.frame(Anova(mmpdss, type = 2, test.statistic = "F")) %>%
  mutate(rarity = "Spatial")

mmpdts_tab = as.data.frame(Anova(mmpdts, type = 2, test.statistic = "F")) %>%
  mutate(rarity = "Temporal")

pd_anova_df = rbind(mmpdss_tab, mmpdts_tab) %>%
  rownames_to_column(var = "type") %>%
  select(rarity, type, `F`, Df, Df.res, `Pr(>F)`) %>%
  mutate_if(is.numeric, round, digits=3) 

write.csv(pd_anova_df, "tables/final_tables/pd_final_initial_mixed_mod_anova_table.csv", row.names = F)

