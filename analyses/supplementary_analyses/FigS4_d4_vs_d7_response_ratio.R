# Header ####
## Script name: Fig S7: D4 vs. D7 response ratios
##
## Purpose of script: visualize d4 vs. d7 response ratios to show no 
## difference between the two.

##
## Author: Carmen Watkins

# Set up ####
source("analyses/calc_response_ratio.R") 
#source("analyses/color_palettes.R")

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
#pal <- wes_palette("Royal3")
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# prep data ###
edge_SEV = edge_RR %>%
  filter(site %in% c("SBK", "SBL")) %>%
  select(site, species, spatial_rarity, temporal_rarity, resp.ratio.site_D4, 
         resp.ratio.site_D6) %>%
  pivot_longer(cols = c("resp.ratio.site_D4", "resp.ratio.site_D6"), 
               names_to = "drought_length", values_to = "response_ratio") %>%
  mutate(drought_length = ifelse(drought_length == "resp.ratio.site_D4", "4 Years", "6 Years"))

# Figure S4 ####
SR_d = ggplot(edge_SEV, aes(x=spatial_rarity, y = response_ratio, 
                            colour = drought_length)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25) +
  facet_wrap(~site) +
  xlab("Spatial Rarity") +
  ylab("Drought Response Ratio") +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1)) +
  labs(color = "Drought Length") +
  scale_color_manual(values = c("#88CCEE", "#2d1c82"))

TR_d = ggplot(edge_SEV, aes(x=temporal_rarity, y = response_ratio, 
                     colour = drought_length)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25) +
  facet_wrap(~site) +
  xlab("Temporal Rarity") +
  ylab("Drought Response Ratio") +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1)) +
  labs(color = "Drought Length") +
  scale_color_manual(values = c("#88CCEE", "#2d1c82"))

ggarrange(SR_d, TR_d, nrow = 2, ncol = 1, labels = "AUTO", common.legend = T, legend = "bottom")

#ggsave("figures/review_figs/FigS7_resp_ratio_v_rarity.tiff", width = 6, height = 6)

# Model ####
## spatial, drought ####
sites = c("SBL", "SBK")

mod_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, 
                    p.value = NA,  conf.low = NA, conf.high = NA, site = NA, 
                    period = NA)

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
modt_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, 
                     p.value = NA,  conf.low = NA, conf.high = NA, site = NA, 
                     period = NA)

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
                                       ifelse(p.value < 0.1 & p.value > 0.05, 
                                              ".", " "))))) %>%
  mutate_if(is.numeric, round, digits = 3)
write.csv(sp_mods_tab, "tables/site_model_output_spatial_SA_7yrdrought.csv")


tmp_mods_tab = modt_df %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05,
                                              ".", " "))))) %>%
  mutate_if(is.numeric, round, digits = 3)
#write.csv(tmp_mods_tab, "tables/site_model_output_temporal_SA_7yrdrought.csv")



