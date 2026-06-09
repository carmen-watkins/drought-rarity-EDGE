# Header #### 
## Script name: Calc RR, Analyse RR, KEEP UNKNOWNS 
##
## Purpose of script: Calculate the Response Ratio from data with unknown species
## kept in. Create response ratio figures and run response ratio models to compare
## to outputs from main text results.
##
## Author: Carmen Watkins
##

# Set up ####
## load packages
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)
library(ggpubr)

## load data
edge_RR = read.csv("analyses/supplementary_analyses/keep_unknowns/edge_response_ratio_and_rarity_WITH_UNKNOWNS.csv")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Plot####
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Fig S7 ####
## test out main pattern
SR_drought = ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D, color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.15", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.21", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.91 [0.71, 1.11]", size = 3.5, parse = FALSE)

SR_postdrought = ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PD, color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.15", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.18", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.89 [0.70, 1.07]", size = 3.5, parse = FALSE)

TR_drought = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D, color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.13", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.16", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.66 [0.50, 0.82]", size = 3.5, parse = FALSE)

TR_postdrought <- ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PD, color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.10", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.12", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.60 [0.44, 0.76]", size = 3.5, parse = FALSE)

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = "auto", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

## ggsave("figures/review_figs/FigS7_RR_v_rarity_keep_unknowns.tiff", width = 18, height = 16, units = "cm")

ggsave("figures/final_figs/supp_figs/FigS7_RR_v_rarity_keep_unknowns.png",
       width = 18, height = 16, units = "cm")

# Check Models ####
## drought, spatial ####
mmsd = lmer(resp.ratio.site_D ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsd)
summary(mmsd) 
confint(mmsd)
r.squaredGLMM(mmsd)

Anova(mmsd, type = 2, test.statistic = "F")

## drought, temporal ####
mmtd = lmer(resp.ratio.site_D ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtd)
summary(mmtd) 
confint(mmtd)
r.squaredGLMM(mmtd)

Anova(mmtd, type = 2, test.statistic = "F") ## main table


## post-drought spatial ####
mmsp = lmer(resp.ratio.site_PD ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsp)
summary(mmsp)
confint(mmsp)
r.squaredGLMM(mmsp)

Anova(mmsp, type = 2, test.statistic = "F") ## main table

## post-drought temporal ####
mmtp = lmer(resp.ratio.site_PD ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtp)
summary(mmtp) 
confint(mmtp)
r.squaredGLMM(mmtp)

Anova(mmtp, type = 3, test.statistic = "F") ## main table

# Create Tables ####
## Anova ####
### decided to use type II Anovas - for when data is unbalanced and DON'T want to consider interactions
mmsd_tab = as.data.frame(Anova(mmsd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial") %>%
  mutate_if(is.numeric, round, digits = 3)

mmtd_tab = as.data.frame(Anova(mmtd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Temporal") %>%
  mutate_if(is.numeric, round, digits = 3)

mmsp_tab = as.data.frame(Anova(mmsp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial") %>%
  mutate_if(is.numeric, round, digits = 3)

mmtp_tab = as.data.frame(Anova(mmtp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal") %>%
  mutate_if(is.numeric, round, digits = 3)

anova_df = rbind(mmsd_tab, mmtd_tab, mmsp_tab, mmtp_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` )
## write.csv(anova_df, "tables/review_tabs/TabS4_mixed_mod_anova_table_SA_Keep_Unknowns.csv")


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
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`) %>%
  mutate_if(is.numeric, round, digits = 3)

## write.csv(coeff_df, "tables/review_tabs/TabS4_mixed_mod_coeff_table_SA_Keep_Unknowns.csv", row.names = F)

