# Header #### 
## Script name: Calculate Response Ratio
##
## Purpose of script: Calculate the response ratio between drought and control plots across years during two time periods (drought & recovery). 
##
## use zero-filled data at subplot level
##
## Calculate RII average across all zero filled subplots to get one average control and drought value per species per site.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

## References: 
## Response Ratio: Armas et al. 2004
## SE of Response Ratio: Armas et al. 2004 supplement A, file:///C:/Users/carme/Downloads/appendixA.htm

# Set up ####
## read in cleaned data
source("data-prep/classify_rank_persistence_zero_filled_at_sub.R")

library(ggpubr)

# Resp Ratio ####
## Post-Drought ####
init.recov.SE.RII <- edge_all %>%
  mutate(recov_filter = ifelse(site %in% c("SBK", "SBL") & experiment.year %in% c(9, 8), "initial_recov",
                               ifelse(site %in% c("SBK", "SBL") & experiment.year %in% c(11, 10), "final_recov",
                                      ifelse(site %in% c("KNZ", "HYS", "CHY", "SGS") & experiment.year %in% c(5, 6), "initial_recov",
                                             
                                             ifelse(site %in% c("KNZ", "HYS", "CHY", "SGS") & experiment.year %in% c(7, 8), "final_recov", "not_recov"))))) %>%
  
  filter(recov_filter == "initial_recov") %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  
  ungroup() %>%
  
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), ## calc response ratio
         
         ## calc error of RII
         ## rho
         rho = (((sd.cover.sp_D^2)/num.obs_D) - ((sd.cover.sp_C^2)/num.obs_C)) / ((sd.cover.sp_D^2/num.obs_D) + (sd.cover.sp_C^2/num.obs_C)), ## calc rho as part of standard error calc
         
         ## term outside of parentheses
         outpar = ((sd.cover.sp_D^2)/num.obs_D + (sd.cover.sp_C^2)/num.obs_C) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 1 inside parentheses
         term1 = ((mean.cover.sp_D - mean.cover.sp_C)^2) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 2 inside parentheses
         term2 = (2 * rho * (mean.cover.sp_D - mean.cover.sp_C)) / (mean.cover.sp_D + mean.cover.sp_C),
         
         ## calc inside of parentheses
         inpar = 1 + term1 - term2,
         
         ## calc SE
         SE.RII = outpar * inpar,
         
         treatment.period = "init_PD") ## add in column to differentiate from post-drought RR

final.recov.SE.RII <- edge_all %>%
  mutate(recov_filter = ifelse(site %in% c("SBK", "SBL") & experiment.year %in% c(9, 8), "initial_recov",
                               ifelse(site %in% c("SBK", "SBL") & experiment.year %in% c(11, 10), "final_recov",
                                      ifelse(site %in% c("KNZ", "HYS", "CHY", "SGS") & experiment.year %in% c(5, 6), "initial_recov",
                                             
                                             ifelse(site %in% c("KNZ", "HYS", "CHY", "SGS") & experiment.year %in% c(7, 8), "final_recov", "not_recov"))))) %>%
  
  filter(recov_filter == "final_recov") %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  
  ungroup() %>%
  
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), ## calc response ratio
         
         ## calc error of RII
         ## rho
         rho = (((sd.cover.sp_D^2)/num.obs_D) - ((sd.cover.sp_C^2)/num.obs_C)) / ((sd.cover.sp_D^2/num.obs_D) + (sd.cover.sp_C^2/num.obs_C)), ## calc rho as part of standard error calc
         
         ## term outside of parentheses
         outpar = ((sd.cover.sp_D^2)/num.obs_D + (sd.cover.sp_C^2)/num.obs_C) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 1 inside parentheses
         term1 = ((mean.cover.sp_D - mean.cover.sp_C)^2) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 2 inside parentheses
         term2 = (2 * rho * (mean.cover.sp_D - mean.cover.sp_C)) / (mean.cover.sp_D + mean.cover.sp_C),
         
         ## calc inside of parentheses
         inpar = 1 + term1 - term2,
         
         ## calc SE
         SE.RII = outpar * inpar,
         
         treatment.period = "final_PD") ## add in column to differentiate from post-drought RR


## Merge ####
RR.tog <- rbind(init.recov.SE.RII, final.recov.SE.RII) %>%
  select(site, species, resp.ratio.site, SE.RII, treatment.period) #%>%
 

## merge with rank and persistence values for each species
edge_RR <- left_join(RR.tog, rank_persist, by = c("site", "species"))

edge_RR$treatment.period = factor(edge_RR$treatment.period, levels = c("init_PD", "final_PD"))

edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

rankR3 <- ggplot(edge_RR, aes(x= percrank, y=resp.ratio.site)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  facet_wrap(~treatment.period) +
  xlab("Rank") +
  ylab("Post-Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persR3 <- ggplot(edge_RR, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  facet_wrap(~treatment.period) +
  xlab("Persistence") +
  ylab("Post-Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(rankR3, persR3, labels = "AUTO", common.legend = T, 
          legend = "bottom", ncol = 1, nrow=2)

ggsave("figures/Nov2024_meeting/figure3_recov_split.png", width = 8, height = 6)
