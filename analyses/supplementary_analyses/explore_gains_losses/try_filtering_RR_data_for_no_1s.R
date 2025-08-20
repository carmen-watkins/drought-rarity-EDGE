

library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)

library(jtools)
library(xtable)


filtered_RR = edge_RR %>%
  filter(!(species %in% c(Kdrop$species) & site == "KNZ"),
         !(species %in% c(Hdrop$species) & site == "HYS"),
         !(species %in% c(Cdrop$species) & site == "CHY"),
         !(species %in% c(SGdrop$species) & site == "SGS"),
         !(species %in% c(SLdrop$species) & site == "SBL"),
         !(species %in% c(SKdrop$species) & site == "SBK"))

filtered_RR$site = factor(filtered_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

filtered_RR %>%
ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(filtered_RR, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site)



SR_drought <- ggplot(filtered_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1))

SR_postdrought <- ggplot(filtered_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

TR_drought <- ggplot(filtered_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

TR_postdrought <- ggplot(filtered_RR, aes(x=temporal_rarity, y=resp.ratio.site_PDfull)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)
ggsave("figures/final/supplement/FigS11_resp_ratio_v_rarity.tiff", width = 6, height = 5.5)



# Model ####
## model as way of estimating the overall effect of rarity on response ratio during drought and postdrought for spatial and temporal rarity. good that it still accounts for effect of site.

## drought, spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = filtered_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Anova(mmsd, type = 2) ## main table

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
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` ) %>%
  mutate_if(is.numeric, round, digits = 3)

write.csv(anova_df, "tables/mixed_mod_anova_table_SA_remove_1s.csv")


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
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`) %>%
  mutate_if(is.numeric, round, digits = 3)

write.csv(coeff_df, "tables/mixed_mod_coeff_table_SA_remove_1s.csv", row.names = F)


























