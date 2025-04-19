# Header ####
## Script name: Q2 Linear Models by Site

## Purpose of script: Run linear models to test the effect of rarity on response ratio separately at each site
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
## load packages
library(broom)
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(jtools)
library(xtable)

source("analyses/calc_response_ratio.R") 
source("data-prep/prep_model_predictors.R")
source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
# Model ####
## during drought
knzD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "KNZ",])
summary(knzD)
summary(knzD)$r.squared
hysD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "HYS",])
summary(hysD)
chyD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "CHY",])
summary(chyD)
sgsD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "SGS",])
summary(sgsD)
sblD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "SBL",])
summary(sblD)
sbkD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "SBK",])
summary(sbkD)

## post-drought
knzP = lm(resp.ratio.site_PDfull ~ spatial_rarity, data = edge_RR[edge_RR$site == "KNZ",])
summary(knzP)
hysP = lm(resp.ratio.site_PDfull ~ spatial_rarity, data = edge_RR[edge_RR$site == "HYS",])
summary(hysP)
chyP = lm(resp.ratio.site_PDfull ~ spatial_rarity, data = edge_RR[edge_RR$site == "CHY",])
summary(chyP)
sgsP = lm(resp.ratio.site_PDfull ~ spatial_rarity, data = edge_RR[edge_RR$site == "SGS",])
summary(sgsP)
sblP = lm(resp.ratio.site_PDfull ~ spatial_rarity, data = edge_RR[edge_RR$site == "SBL",])
summary(sblP)
sbkP = lm(resp.ratio.site_PDfull ~ spatial_rarity, data = edge_RR[edge_RR$site == "SBK",])
summary(sbkP)

# Create DFs of Model ####
## spatial, drought ####
sites = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")

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

## spatial, post-drought ####
mod_dfp = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought")
  
  ## append
  mod_dfp = rbind(mod_dfp, tmp) %>%
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

## temporal, post-drought ####
modt_dfp = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought")
  
  ## append
  modt_dfp = rbind(modt_dfp, tmp) %>%
    filter(!is.na(term))
  
}

## combine ####
sp_mods = rbind(mod_df, mod_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmp_mods = rbind(modt_df, modt_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Create Table ####
sp_mods_tab = sp_mods %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, ".", " ")))))

#write.csv(sp_mods_tab, "tables/site_model_output_spatial.csv")

tmp_mods_tab = tmp_mods %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, ".", " ")))))

#write.csv(tmp_mods_tab, "tables/site_model_output_temporal.csv")

#xtable(sp_mods_tab)

# Plot ####
## Coeff plots ####
### temporal, drought
temp_slope = tmp_mods %>%
  filter(period == "Drought", term == "temporal_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
    y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  #theme(axis.text.x=element_text(size=12)) +
  #theme(#axis.text.y=element_text(size=12),
      #  axis.title=element_text(size=13)) +
  theme(text = element_text(size = 13))


temp_int = tmp_mods %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Drought",
       y = "") +
  labs(fill = "Site")  +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))


## spatial rarity, drought
spat_slope = sp_mods %>%
  filter(period == "Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
    y = "Slope") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))


spat_int = sp_mods %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Drought",
       y = "Intercept") +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))


### temporal, post-drought
temp_slopep = tmp_mods %>%
  filter(period == "Post-Drought", term == "temporal_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))


temp_intp = tmp_mods %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Post-Drought",
       y = "") +
  labs(fill = "Site")  +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))


## spatial rarity, post-drought
spat_slopep = sp_mods %>%
  filter(period == "Post-Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))


spat_intp = sp_mods %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Post-Drought",
       y = " ") +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))



### plot ####
ggarrange(spat_slope, spat_slopep, temp_slope, temp_slopep,
          spat_int, spat_intp, temp_int, temp_intp,
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom", 
          labels = "AUTO")
# ggsave("figures/Jan2025/site_slopes_intercepts_drought.tiff", width = 8.5, height = 5.5)

### SPATIAL ONLY ####
spat_slope = sp_mods %>%
  filter(period == "Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "Slope") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))

spat_int = sp_mods %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Coeff Estimate",
       y = "Intercept") +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))

## spatial rarity, post-drought
spat_slopep = sp_mods %>%
  filter(period == "Post-Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))


spat_intp = sp_mods %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Coeff Estimate",
       y = " ") +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))


ggarrange(spat_slope, spat_slopep, spat_int, spat_intp, labels = "AUTO", common.legend = TRUE, legend = "bottom")

ggsave("figures/Jan2025/SO_site_slopes_intercepts_drought.tiff", width = 5.5, height = 5.5)


## Rearranged ####
sp_mods %>%
  filter(period == "Drought", term == "spatial_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Coeff Estimate",
       y = "Slope") +
  labs(fill = "Site") +
  #theme(axis.text.y = element_blank(),
       # axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13)) +
  scale_y_discrete(limits=rev)

ggsave("figures/Jan2025/SO_site_slopes_drought_FORCOMPARISONONLY.tiff", width = 3.5, height = 4)

sp_mods %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Coeff Estimate",
       y = "Intercept") +
  labs(fill = "Site") +
  #theme(axis.text.y = element_blank(),
  # axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(text = element_text(size = 13)) +
  scale_y_discrete(limits=rev)

ggsave("figures/Jan2025/SO_site_intercept_drought_FORCOMPARISONONLY.tiff", width = 3.5, height = 4)









## Coeff v Pred plots ####
spmods_pred = left_join(sp_mods, site_pred_scaled, by = "site") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmpmods_pred = left_join(tmp_mods, site_pred_scaled, by = "site") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

### drought ####
#### spatial ####
## spatial, dominance
pD1 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Drought") %>%
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
    xlab(" ") +
  ylab(" ") +
  labs(fill = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
    axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))

pT1 = spmods_pred %>%
    filter(term == "spatial_rarity", period == "Drought") %>%
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))

pP1 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab("Spatial Rarity Slope") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))

#### temporal ####
pDt = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab(" ") +
  labs(color = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "black", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

pTt = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab(" ") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

pPt = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab("Temporal Rarity Slope") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

ggarrange(pP1, pT1, pD1, 
          pPt, pTt, pDt, 
    ncol = 3, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Mar2025/Fig4_site_slopes_predictors_drought_both_rarity.tiff", width = 8.5, height = 6)


### post-drought ####
#### spatial ####
pD2 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))

pT2 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab("Spatial Rarity Slope") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))

pP2 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))

#### temporal ####
pDt2 = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Post-Drought") %>%
  
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab(" ") +
  labs(color = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

pTt2 = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Post-Drought") %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab("Temporal Rarity Slope") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

pPt2 = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab("") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

#### make fig ####
ggarrange(pT2, pD2, pP2,
          pTt2, pDt2, pPt2,
          ncol = 3, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Jan2025/site_slopes_predictors_postdrought_both_rarity.png", width = 8.5, height = 6)
