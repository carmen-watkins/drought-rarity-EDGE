# Header ####
## Script name: Q1 Mixed Models

## Purpose of script: Run linear mixed effects models to test the overall effect of rarity on response ratio
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)

library(jtools)
library(xtable)

source("analyses/calc_response_ratio.R") 
source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
#wes_palette("Royal3")

# Model ####
## model as way of estimating the overall effect of rarity on response ratio during drought and postdrought for spatial and temporal rarity. good that it still accounts for effect of site.

## drought, spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Anova(mmsd, type = 2) ## main table

xint = -(-0.30282)/0.86135
## VarCorr could be helpful for extracting model output

coef(mmsd)$site[,"(Intercept)"]

confint(mmsd)

## drought, temporal ####
mmtd = lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtd)
summary(mmtd) ## supp table
Anova(mmtd, type = 3, test.statistic = "F") ## main table
confint(mmtd)

## post-drought spatial ####
mmsp = lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsp)
summary(mmsp) ## supp table
Anova(mmsp, type = 2, test.statistic = "F") ## main table
confint(mmsp)

## post-drought temporal ####
mmtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtp)
summary(mmtp) ## supp table
Anova(mmtp, type = 3, test.statistic = "F") ## main table
confint(mmtp)

# Create Tables ####
## Anova ####
### decided to use type II Anovas - for when data is unbalanced and DON'T want to consider interactions
mmsd_tab = as.data.frame(Anova(mmsd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_tab = as.data.frame(Anova(mmtd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_tab = as.data.frame(Anova(mmsp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_tab = as.data.frame(Anova(mmtp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

anova_df = rbind(mmsd_tab, mmtd_tab, mmsp_tab, mmtp_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` )

#write.csv(anova_df, "tables/mixed_mod_anova_table.csv")
#xtable(anova_df)

## Coeff ####
mmsd_coeff = as.data.frame(summary(mmsd)$coefficients) %>% 
  #lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = .) %>% 
  #tidy(conf.int = TRUE) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_coeff = as.data.frame(summary(mmtd)$coefficients) %>% 
  #lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = .) %>% 
  #tidy(conf.int = TRUE) %>%
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_coeff = as.data.frame(summary(mmsp)$coefficients)%>%
 # lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = .) %>% 
  #tidy(conf.int = TRUE) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_coeff = as.data.frame(summary(mmtp)$coefficients) %>%
 # lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = .) %>% 
#  tidy(conf.int = TRUE) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

coeff_df = rbind(mmsd_coeff, mmtd_coeff, mmsp_coeff, mmtp_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`)

#xtable(coeff_df)
#write.csv(coeff_df, "tables/mixed_mod_coeff_table.csv", row.names = F)

# Plot Fig 1 ####
p1 = effect_plot(mmsd, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

## too many sites & points for coloring points to be easily interpretable; can include supplementary figures to make this point
#ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull)) +
 # geom_point(aes(color = site)) +
 # geom_smooth(method = "lm", color = "black") +
#  scale_color_manual(values = pal)

p2 = effect_plot(mmtd, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = " ", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

p3 = effect_plot(mmsp, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Post-Drought Response Ratio", x.label = "Spatial Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

p4 = effect_plot(mmtp, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = " ", x.label = "Temporal Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

ggarrange(p1, p2, p3, p4, labels = "AUTO")

## ggsave("figures/Jan2025/resp_ratio_v_rarity_mmfit.tiff", width = 7.5, height = 7)

