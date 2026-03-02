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
  mutate(drought_length = ifelse(drought_length == "resp.ratio.site_D4", "4 Years", "7 Years"))

# Figure S4 ####
## R2 vals
R2vals = data.frame(site = c("SBL", "SBK"), x = rep(-0.8, 2), 
                    y = rep(1.15, 2), R2 = c(0.196, 0.404))


SBLS = edge_SEV %>%
  filter(site == "SBL") %>%
  ggplot(aes(x=spatial_rarity, y = response_ratio, 
                            colour = drought_length)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25) +
  xlab("Spatial Rarity") +
  ylab("Response Ratio") +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  labs(color = "Drought Length") +
  scale_color_manual(values = c("#88CCEE", "#2d1c82"))+
  annotate("text", x = 0.1, y=1.16, label = "R^2: 0.196", size = 3,
           parse = TRUE) +
  ggtitle("SBL") +
  theme(axis.title=element_text(size=13))

SBKS = edge_SEV %>%
  filter(site == "SBK") %>%
  ggplot(aes(x=spatial_rarity, y = response_ratio, 
             colour = drought_length)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25) +
  xlab("Spatial Rarity") +
  ylab(" ") +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  labs(color = "Drought Length") +
  scale_color_manual(values = c("#88CCEE", "#2d1c82"))+
  annotate("text", x = 0.1, y=1.16, label = "R^2: 0.404", size = 3,
           parse = TRUE) +
  ggtitle("SBK") +
  theme(axis.title=element_text(size=13))

SBLT = edge_SEV %>%
  filter(site == "SBL") %>%
  ggplot(aes(x=temporal_rarity, y = response_ratio, 
             colour = drought_length)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25) +
  xlab("Temporal Rarity") +
  ylab("Response Ratio") +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  labs(color = "Drought Length") +
  scale_color_manual(values = c("#88CCEE", "#2d1c82"))+
  annotate("text", x = 0.1, y=1.16, label = "R^2: 0.125 ", size = 3,
           parse = TRUE) +
  ggtitle("SBL") +
  theme(axis.title=element_text(size=13))

SBKT = edge_SEV %>%
  filter(site == "SBK") %>%
  ggplot(aes(x=temporal_rarity, y = response_ratio, 
             colour = drought_length)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25) +
  xlab("Temporal Rarity") +
  ylab(" ") +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  labs(color = "Drought Length") +
  scale_color_manual(values = c("#88CCEE", "#2d1c82"))+
  annotate("text", x = 0.1, y=1.16, label = "R^2: 0.382", size = 3,
           parse = TRUE) +
  ggtitle("SBK") +
  theme(axis.title=element_text(size=13))

ggarrange(SBLS, SBKS, SBLT, SBKT, nrow = 2, ncol = 2, labels = "auto", 
          common.legend = T, legend = "bottom")

#ggsave("figures/review_figs/FigS10_resp_ratio_v_rarity.tiff", width = 18, height = 16, units = "cm")


# Model ####
## sbl, spatial
sbls = lm(response_ratio ~ spatial_rarity + drought_length, 
   data = edge_SEV[edge_SEV$site == "SBL",])

summary(sbls)

sbls_coeff = as.data.frame(summary(sbls)$coefficients) %>% 
  mutate(site = "SBL",
         rarity = "Spatial")
Anova(sbls, type = 2, test.statistic = "F")

## sbl, temporal
sblt = lm(response_ratio ~ temporal_rarity + drought_length, 
          data = edge_SEV[edge_SEV$site == "SBL",])

summary(sblt)
sblt_coeff = as.data.frame(summary(sblt)$coefficients) %>% 
  mutate(site = "SBL",
         rarity = "Temporal")
Anova(sblt, type = 2, test.statistic = "F")


## sbk, spatial
sbks = lm(response_ratio ~ spatial_rarity + drought_length, 
         data = edge_SEV[edge_SEV$site == "SBK",])

summary(sbks)
sbks_coeff = as.data.frame(summary(sbks)$coefficients) %>% 
  mutate(site = "SBK",
         rarity = "Spatial")
Anova(sbks, type = 2, test.statistic = "F")

## sbk, temporal
sbkt = lm(response_ratio ~ temporal_rarity + drought_length, 
          data = edge_SEV[edge_SEV$site == "SBK",])

summary(sbkt)
sbkt_coeff = as.data.frame(summary(sbkt)$coefficients) %>% 
  mutate(site = "SBK",
         rarity = "Temporal")
Anova(sbkt, type = 2, test.statistic = "F")


## combine
d4d7 = rbind(sbls_coeff, sblt_coeff, sbks_coeff, sbkt_coeff) %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 3))) %>%
  mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 10)) %>%
  rownames_to_column(var = "type")

## write.csv(d4d7, "tables/review_tabs/TabS6site_model_coeff_4v7yrdrought.csv", 
   ##      row.names = F)
