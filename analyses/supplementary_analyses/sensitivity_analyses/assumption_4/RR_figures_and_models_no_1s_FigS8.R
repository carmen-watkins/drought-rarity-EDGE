# Header ####
##
## Script name: RR figs and models for no 1's
##
## Purpose of script: create figures and run models to test whether removing 
## observations that were gained/lost but NOT present in pre-treatment years 
## changes the pattern.
##
## Author: Carmen Watkins

# Set up ####
## load packages
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)
library(jtools)
library(xtable)

## load data
source("analyses/supplementary_analyses/sensitivity_analyses/assumption_4/filter_1s.R")

theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

## Prep Data ####
## filter out species with response ratio's of 1 or -1 that were not present
## in the pre-treatment data at a site
filtered_RR = edge_RR %>%
  filter(!(species %in% c(Kdrop$species) & site == "KNZ"),
         !(species %in% c(Hdrop$species) & site == "HYS"),
         !(species %in% c(Cdrop$species) & site == "CHY"),
         !(species %in% c(SGdrop$species) & site == "SGS"),
         !(species %in% c(SLdrop$species) & site == "SBL"),
         !(species %in% c(SKdrop$species) & site == "SBK"))

## order sites by precip
filtered_RR$site = factor(filtered_RR$site, levels = c("KNZ", "HYS", "CHY", 
                                                       "SGS", "SBL", "SBK"))

## plot
filtered_RR %>%
ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(filtered_RR, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site)


# Fig S8 ####
SR_drought = ggplot(filtered_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4, 
                                     color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.06", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.13", size = 3, parse = TRUE)

SR_postdrought = ggplot(filtered_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull,
                                         color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.08", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.10", size = 3, parse = TRUE)

TR_drought = ggplot(filtered_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4, 
                                     color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.09", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.14", size = 3, parse = TRUE)

TR_postdrought = ggplot(filtered_RR, aes(x=temporal_rarity, y=resp.ratio.site_PDfull,
                                         color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.09", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.09", size = 3, parse = TRUE)

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = "auto", common.legend = T, legend = "bottom", ncol = 2, 
          nrow=2)
ggsave("figures/review_figs/FigS8_resp_ratio_v_rarity_no1s.tiff", 
       width = 18, height = 16, units = "cm")

ggsave("figures/review_figs/supp/FigS8_resp_ratio_v_rarity_no1s.png", 
       width = 18, height = 16, units = "cm")

# Model ####
## model as way of estimating the overall effect of rarity on response ratio 
## during drought and postdrought for spatial and temporal rarity. good that 
## it still accounts for effect of site.

## drought, spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = filtered_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Anova(mmsd, type = 2, test.statistic = "F") ## main table
r.squaredGLMM(mmsd)
#eta_squared()

## drought, temporal ####
mmtd = lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = filtered_RR)

#check_model(mmtd)
summary(mmtd) ## supp table
Anova(mmtd, type = 2, test.statistic = "F") ## main table
confint(mmtd)

r.squaredGLMM(mmtd)

## post-drought spatial ####
mmsp = lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = filtered_RR)

#check_model(mmsp)
summary(mmsp) ## supp table
Anova(mmsp, type = 2, test.statistic = "F") ## main table
confint(mmsp)
r.squaredGLMM(mmsp)


## post-drought temporal ####
mmtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = filtered_RR)

#check_model(mmtp)
summary(mmtp) ## supp table
Anova(mmtp, type = 3, test.statistic = "F") ## main table
confint(mmtp)

r.squaredGLMM(mmtp)

# Create Tables ####
## Anova ####
### decided to use type II Anovas - for when data is unbalanced and DON'T want 
## to consider interactions
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
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` ) %>%
  mutate_if(is.numeric, round, digits = 3)

# write.csv(anova_df, "tables/mixed_mod_anova_table_SA_remove_1s.csv")


## Coeff ####
mmsd_coeff = as.data.frame(summary(mmsd)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_coeff = as.data.frame(summary(mmtd)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_coeff = as.data.frame(summary(mmsp)$coefficients)%>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_coeff = as.data.frame(summary(mmtp)$coefficients) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

coeff_df = rbind(mmsd_coeff, mmtd_coeff, mmsp_coeff, mmtp_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, 
         `Pr(>|t|)`) %>%
  mutate_if(is.numeric, round, digits = 3)

write.csv(coeff_df, "tables/review_tabs/TabS5_mixed_mod_coeff_table_remove_gainslosses.csv", row.names = F)
