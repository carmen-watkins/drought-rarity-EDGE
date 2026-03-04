## Header ####
## Script name: Q1B PD split Mixed Models

## Purpose of script: Run linear mixed effects models to test whether the 
## first two or final years of the post-drought period influence recovery 
##
## Author: Carmen Watkins
##

library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)
library(jtools)
library(xtable)
library(MuMIn)
library(ggpubr)

source("analyses/calc_response_ratio.R") 
#source("analyses/color_palettes.R")

## reformat data frame
edge_PDlong = edge_RR %>%
  select(site, species, resp.ratio.site_PDfirst, resp.ratio.site_PDfinal, 
         spatial_rarity, temporal_rarity) %>%
  pivot_longer(cols = c("resp.ratio.site_PDfirst", "resp.ratio.site_PDfinal"), 
               names_to = "PD_period", values_to = "resp.ratio") %>%
  mutate(PD_period = ifelse(PD_period == "resp.ratio.site_PDfirst", "initial", "final"))

## spatial rarity model
mmpdss = lmer(resp.ratio ~ spatial_rarity + PD_period + (1|site), 
              data = edge_PDlong)
summary(mmpdss)

Anova(mmpdss, type = 2, test.statistic = "F")

r.squaredGLMM(mmpdss)


## temporal rarity model
mmpdts = lmer(resp.ratio ~ temporal_rarity + PD_period + (1|site), 
              data = edge_PDlong)
summary(mmpdts)

Anova(mmpdts, type = 2, test.statistic = "F")
r.squaredGLMM(mmpdts)

## Table ####
## put model outputs into a table
mmpdss_tab = as.data.frame(Anova(mmpdss, type = 2, test.statistic = "F")) %>%
  mutate(rarity = "Spatial")

mmpdts_tab = as.data.frame(Anova(mmpdts, type = 2, test.statistic = "F")) %>%
  mutate(rarity = "Temporal")

pd_anova_df = rbind(mmpdss_tab, mmpdts_tab) %>%
  rownames_to_column(var = "type") %>%
  select(rarity, type, `F`, Df, Df.res, `Pr(>F)`) %>%
  mutate_if(is.numeric, round, digits=3) 

## save table
#write.csv(pd_anova_df, "tables/final_tables/pd_final_initial_mixed_mod_anova_table.csv",
          #row.names = F)

mmpdss_coeff = as.data.frame(summary(mmpdss)$coefficients) %>% 
  mutate(rarity = "Spatial")

mmpdts_coeff = as.data.frame(summary(mmpdts)$coefficients) %>% 
  mutate(rarity = "Temporal")

coeff_df = rbind(mmpdss_coeff, mmpdts_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(rarity, type, Estimate, `Std. Error`, df, `t value`, 
         `Pr(>|t|)`) %>%
  mutate_if(is.numeric, round, digits = 3)

write.csv(coeff_df, "tables/review_tabs/TabS5_mixed_mod_coeff_split_postdrought.csv", row.names = F)

# Figure S7 ####
pds = edge_PDlong %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio, color = PD_period)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  labs(color = "PD Period") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = c("#855C75","#D9AF6B")) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.09", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.14", size = 3, parse = TRUE)

pdt = edge_PDlong %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio, color = PD_period)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Post-Drought") +
  xlab("Temporal Rarity") +
  labs(color = "PD Period") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = c("#855C75","#D9AF6B")) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.10", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.12", size = 3, parse = TRUE)

ggarrange(pds, pdt, common.legend = T, labels = c("a", "b"), legend = "bottom")

ggsave("figures/review_figs/FigS9_split_post_drought.tiff", width = 6, height = 3.5)
ggsave("figures/review_figs/supp/FigS9_split_post_drought.png", width = 6, height = 3.5)


#"#855C75","#D9AF6B","#AF6458","#736F4C","#526A83",#625377,#68855C,#9C9C5E,#A06177,#8C785D,#467378,#7C7C7C

