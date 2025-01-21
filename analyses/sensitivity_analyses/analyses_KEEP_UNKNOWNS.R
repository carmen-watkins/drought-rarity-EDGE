## I assume this is the unknowns models

# Set up ####

## load packages
library(broom)
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)
library(jtools)
library(xtable)

source("analyses/senstivity_analyses/calc_response_ratio_KEEP_UNKNOWNS_sensA.R") 
source("analyses/color_palettes.R")
source("data-prep/prep_model_predictors.R")


## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
#wes_palette("Royal3")

# Q1 ####
## Model ####
## model as way of estimating the overall effect of rarity on response ratio during drought and postdrought for spatial and temporal rarity. good that it still accounts for effect of site.

### drought, spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Anova(mmsd, type = 3) ## main table

## generate latex tables
print(xtable(summary(mmsd)$coefficients)) ## supp
xtable(Anova(mmsd, type = 3, test.statistic = "F")) ## main

mmsd_tab = as.data.frame(Anova(mmsd, type = 3, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

mmsd_coeff = edge_RR %>% 
  lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = .) %>% 
  tidy(conf.int = TRUE) 
## this does a good job for fixed effects, less so for random effects - the variance of random effects is in the estimate column in the new df

summary(mmsd)$coefficients
summary(mmsd)$groups

summary(mmsd)$effect

## VarCorr could be helpful for extracting model output

### drought, temporal ####
mmtd = lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtd)
summary(mmtd) ## supp table
Anova(mmtd, type = 3, test.statistic = "F") ## main table

mmtd_tab = as.data.frame(Anova(mmtd, type = 3, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Temporal")

### post-drought spatial ####
mmsp = lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsp)
summary(mmsp) ## supp table
Anova(mmsp, type = 3, test.statistic = "F") ## main table

mmsp_tab = as.data.frame(Anova(mmsp, type = 3, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

### post-drought temporal ####
mmtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtp)
summary(mmtp) ## supp table
Anova(mmtp, type = 3, test.statistic = "F") ## main table

mmtp_tab = as.data.frame(Anova(mmtp, type = 3, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

anova_df = rbind(mmsd_tab, mmtd_tab, mmsp_tab, mmtp_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` )

xtable(anova_df)


## Plot Fig 1 ####
p1 = effect_plot(mmsd, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  ggtitle("With Unknowns")

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
        axis.title=element_text(size=13)) +
  ggtitle("")

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

## ggsave("figures/Jan2025/resp_ratio_v_rarity_mmfit.png", width = 7.5, height = 7)

# Q2 ####
## Model ####
## during drought
knzD = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR[edge_RR$site == "KNZ",])
summary(knzD)
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

## Create DFs of Model ####
### spatial, drought ####
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

### spatial, post-drought ####
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

### temporal, drought ####
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

### temporal, post-drought ####
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

### combine ####
sp_mods = rbind(mod_df, mod_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmp_mods = rbind(modt_df, modt_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Plot ####
### Coeff plots ####
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
  coord_cartesian(xlim = c(-0.8, 2.1))

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
        axis.ticks.y = element_blank())

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
  coord_cartesian(xlim = c(-0.8, 2.1))

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
        axis.ticks.y = element_blank())

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
  coord_cartesian(xlim = c(-0.8, 2.1))

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
        axis.ticks.y = element_blank())

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
  coord_cartesian(xlim = c(-0.8, 2.1))

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
        axis.ticks.y = element_blank())


#### plot ####
ggarrange(spat_slope, spat_slopep, temp_slope, temp_slopep,
          spat_int, spat_intp, temp_int, temp_intp,
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom", 
          labels = "AUTO")
# ggsave("figures/Jan2025/site_slopes_intercepts_drought.tiff", width = 8.5, height = 5.5)


### Coeff v Pred plots ####
#### spatial ####
spmods_pred = left_join(sp_mods, site_pred_scaled, by = "site")

pD = spmods_pred %>%
  filter(term == "spatial_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab("Spatial Rarity Slope") +
  labs(color = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pT = spmods_pred %>%
  filter(term == "spatial_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab(" ") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pP = spmods_pred %>%
  filter(term == "spatial_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab(" ") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

ggarrange(pD, pP, pT, ncol = 3, nrow = 1, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Jan2025/site_slopes_predictors_spatial.png", width = 7, height = 5)

#### temporal ####
tmpmods_pred = left_join(tmp_mods, site_pred_scaled, by = "site")

pDt = tmpmods_pred %>%
  filter(term == "temporal_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab("Temporal Rarity Slope") +
  labs(color = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pTt = tmpmods_pred %>%
  filter(term == "temporal_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab(" ") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pPt = tmpmods_pred %>%
  filter(term == "temporal_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab("") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

ggarrange(pDt, pPt, pTt, ncol = 3, nrow = 1, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Jan2025/site_slopes_predictors_temporal.png", width = 7, height = 5)


