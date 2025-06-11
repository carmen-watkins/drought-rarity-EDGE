
## visualize d4 vs. d7 response ratios to show no difference between the two.

# Set up ####
source("analyses/calc_response_ratio.R") 
source("analyses/color_palettes.R")

## load packages
library(broom)
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(jtools)
library(xtable)

library(ggpubr)

theme_set(theme_classic())
pal <- wes_palette("Royal3")

# Figure S4 ####
SR_d = ggplot(edge_RR[edge_RR$site %in% c("SBK", "SBL"),], aes(x= spatial_rarity)) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D4), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D6), method = "lm", alpha = 0.1, linewidth = 0.75, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[5], pal[6])) +
  xlab("Spatial Rarity") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1))

TR_d = ggplot(edge_RR[edge_RR$site %in% c("SBK", "SBL"),], aes(x=temporal_rarity)) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D4), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D6), method = "lm", alpha = 0.1, linewidth = 0.75, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[5], pal[6])) +
  xlab("Temporal Rarity") +
  ylab(" ") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

ggarrange(SR_d, TR_d, labels = "AUTO", common.legend = T, legend = "bottom")

#ggsave("figures/Mar2025/FigS4_resp_ratio_v_rarity.png", width = 6, height = 3.5)

# Model ####
## spatial, drought ####
sites = c("SBL", "SBK")

mod_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Drought")
  
  ## append
  mod_df = rbind(mod_df, tmp) %>%
    filter(!is.na(term))
  
}


## temporal, drought ####
modt_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Drought")
  
  ## append
  modt_df = rbind(modt_df, tmp) %>%
    filter(!is.na(term))
  
}


sp_mods_tab = mod_df %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, ".", " "))))) %>%
  mutate_if(is.numeric, round, digits = 3)
write.csv(sp_mods_tab, "tables/site_model_output_spatial_SA_7yrdrought.csv")


tmp_mods_tab = modt_df %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, ".", " "))))) %>%
  mutate_if(is.numeric, round, digits = 3)
write.csv(tmp_mods_tab, "tables/site_model_output_temporal_SA_7yrdrought.csv")



