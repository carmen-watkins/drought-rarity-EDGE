## plot linear model outputs

library(broom)

source("data-prep/prep_model_predictors.R")


# Models ####
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

# Model DF ####
## Spatial Rarity ####
### drought ####
KNZ_mod = edge_RR[edge_RR$site == "KNZ",] %>% 
  lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "KNZ", 
         period = "Drought")

HYS_mod = edge_RR[edge_RR$site == "HYS",] %>% 
  lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "HYS", 
         period = "Drought")

CHY_mod = edge_RR[edge_RR$site == "CHY",] %>% 
  lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "CHY", 
         period = "Drought")

SGS_mod = edge_RR[edge_RR$site == "SGS",] %>% 
  lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SGS", 
         period = "Drought")

SBL_mod = edge_RR[edge_RR$site == "SBL",] %>% 
  lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBL", 
         period = "Drought")

SBK_mod = edge_RR[edge_RR$site == "SBK",] %>% 
  lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBK", 
         period = "Drought")

### post-drought ####
KNZp_mod = edge_RR[edge_RR$site == "KNZ",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "KNZ", 
         period = "Post-Drought")

HYSp_mod = edge_RR[edge_RR$site == "HYS",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "HYS", 
         period = "Post-Drought")

CHYp_mod = edge_RR[edge_RR$site == "CHY",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "CHY", 
         period = "Post-Drought")

SGSp_mod = edge_RR[edge_RR$site == "SGS",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SGS", 
         period = "Post-Drought")

SBLp_mod = edge_RR[edge_RR$site == "SBL",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBL", 
         period = "Post-Drought")

SBKp_mod = edge_RR[edge_RR$site == "SBK",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBK", 
         period = "Post-Drought")

## join together
all_mod = rbind(KNZ_mod, HYS_mod, CHY_mod, SGS_mod, SBL_mod, SBK_mod, 
                KNZp_mod, HYSp_mod, CHYp_mod, SGSp_mod, SBLp_mod, SBKp_mod) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Temporal ####
### drought ####
KNZ_modt = edge_RR[edge_RR$site == "KNZ",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "KNZ", 
         period = "Drought")

HYS_modt = edge_RR[edge_RR$site == "HYS",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "HYS", 
         period = "Drought")

CHY_modt = edge_RR[edge_RR$site == "CHY",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "CHY", 
         period = "Drought")

SGS_modt = edge_RR[edge_RR$site == "SGS",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SGS", 
         period = "Drought")

SBL_modt = edge_RR[edge_RR$site == "SBL",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBL", 
         period = "Drought")

SBK_modt = edge_RR[edge_RR$site == "SBK",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBK", 
         period = "Drought")

### post-drought ####
KNZp_modt = edge_RR[edge_RR$site == "KNZ",] %>% 
  lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "KNZ", 
         period = "Post-Drought")

HYSp_modt = edge_RR[edge_RR$site == "HYS",] %>% 
  lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "HYS", 
         period = "Post-Drought")

CHYp_modt = edge_RR[edge_RR$site == "CHY",] %>% 
  lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "CHY", 
         period = "Post-Drought")

SGSp_modt = edge_RR[edge_RR$site == "SGS",] %>% 
  lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SGS", 
         period = "Post-Drought")

SBLp_modt = edge_RR[edge_RR$site == "SBL",] %>% 
  lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBL", 
         period = "Post-Drought")

SBKp_modt = edge_RR[edge_RR$site == "SBK",] %>% 
  lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
  tidy(conf.int = TRUE) %>%
  mutate(site = "SBK", 
         period = "Post-Drought")

## join together
all_modt = rbind(KNZ_modt, HYS_modt, CHY_modt, SGS_modt, SBL_modt, SBK_modt, 
                KNZp_modt, HYSp_modt, CHYp_modt, SGSp_modt, SBLp_modt, SBKp_modt) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Plot ####
## Coeff plots ####
### temporal, drought
temp_slope = all_modt %>%
  filter(period == "Drought", term == "temporal_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Rarity Slope",
    y = NULL) +
  labs(fill = "Site")

temp_int = all_modt %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept",
       y = NULL) +
  labs(fill = "Site")  +
  coord_cartesian(xlim = c(-1, 0.25))

## spatial rarity, drought
spat_slope = all_mod %>%
  filter(period == "Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
    y = NULL) +
  labs(fill = "Site")

spat_int = all_mod %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = NULL) +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.25))

### temporal, post-drought
temp_slopep = all_modt %>%
  filter(period == "Post-Drought", term == "temporal_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Rarity Slope",
       y = NULL) +
  labs(fill = "Site")

temp_intp = all_modt %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept",
       y = NULL) +
  labs(fill = "Site")  +
  coord_cartesian(xlim = c(-1, 0.25))

## spatial rarity, post-drought
spat_slopep = all_mod %>%
  filter(period == "Post-Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = NULL) +
  labs(fill = "Site")

spat_intp = all_mod %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = NULL) +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.25))

ggarrange(spat_slope, spat_slopep, temp_slope, temp_slopep,
          spat_int, spat_intp, temp_int, temp_intp,
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
# ggsave("figures/Jan2025/site_slopes_intercepts_drought.png", width = 7, height = 6)


## Coeff v Pred plots ####
mod_pred = left_join(all_mod, site_pred_scaled, by = "site")

pD = mod_pred %>%
  filter(term == "spatial_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%

  ggplot(aes(x=BP.dom.site, y=estimate)) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
    xlab("Site-Level Dominance") +
  ylab("Estimate") +
  labs(color = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")
  
pT = mod_pred %>%
  filter(term == "spatial_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab("Estimate") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pP = mod_pred %>%
  filter(term == "spatial_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%

  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab("Estimate") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

ggarrange(pD, pP, pT, ncol = 3, nrow = 1, common.legend = T, legend = "bottom")

# ggsave("figures/Jan2025/site_slopes_predictors.png", width = 4, height = 7.5)


mod_predt = left_join(all_modt, site_pred_scaled, by = "site")

pDt = mod_predt %>%
  filter(term == "temporal_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab("Temporal Rarity Coefficient Estimate") +
  labs(color = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pTt = mod_predt %>%
  filter(term == "temporal_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab("Temporal Rarity Coefficient Estimate") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

pPt = mod_predt %>%
  filter(term == "temporal_rarity") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab("Temporal Rarity Coefficient Estimate") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")

ggarrange(pDt, pPt, pTt, ncol = 3, nrow = 1, common.legend = T, legend = "bottom")
