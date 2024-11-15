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


source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
wes_palette("Royal3")


# Set up ####
## read in cleaned data
#source("data-prep/classify_rank_persistence_zero_filled_at_sub.R")

# Resp Ratio ####
## Drought ####
### 4-year ####
drought.SE.RII <- edge_all %>%
  
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  
  ## RARIFY
  group_by(year, site, block, plot, species) %>% 
  slice_sample(n=2) %>% ## select 2 of the observations for each subplot
  ungroup() %>%
  
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
         
         treatment.period = "D") ## add in column to differentiate from post-drought RR

## Post-Drought ####
recov.SE.RII <- edge_all %>%
  filter(treatment.year == "recovery") %>% ## 0 is pre-treat year; drought was years 1-4
  
  ## RARIFY
  group_by(year, site, block, plot, species) %>% 
  slice_sample(n=2) %>% ## select 2 of the observations for each subplot
  ungroup() %>%
  
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
         
         treatment.period = "PD") ## add in column to differentiate from post-drought RR


## Merge ####
RR.tog <- rbind(drought.SE.RII, recov.SE.RII) %>%
  select(site, species, resp.ratio.site, SE.RII, treatment.period) %>%
  pivot_wider(names_from = treatment.period, values_from = c(resp.ratio.site, SE.RII))# %>%
# mutate(drought.RR = ifelse(is.na(drought.RR), 0, drought.RR), ## taking this part out 11/1/24 as it seems like it is giving an artifical value for species when really there should juust be no value. 0 can be achieved several ways, so it's not right to put 0's in for species that just didn't have a value during a certain time period.

## although that being said, if there is a row for a species it was around at some point during the time period at that site.
## it was either not present at all during drought period in cntrol or treat plots but showed up during post-drought; or alternatively it was present in drought period but post-drought it was lost entirely 

## need to think more about hwether to add these in and if they mean what I think they do...
#recovery.RR = ifelse(is.na(recovery.RR), 0, recovery.RR))

## merge with rank and persistence values for each species
edge_RR_RARIFY <- left_join(RR.tog, rank_persist_RARIFY, by = c("site", "species"))


# Check RESults ####
edge_RR_RARIFY$site = factor(edge_RR_RARIFY$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))


## test out main pattern
rankD3 <- ggplot(edge_RR_RARIFY, aes(x= percrank, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Relative MAP") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

rankR3 <- ggplot(edge_RR_RARIFY, aes(x=percrank, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Percent Rank") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persD3 <- ggplot(edge_RR_RARIFY, aes(x=persistence.site, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persR3 <- ggplot(edge_RR_RARIFY, aes(x=persistence.site, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Persistence") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

meanD3 <- ggplot(edge_RR_RARIFY, aes(x=mean.ctrl.cov, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

meanR3 <- ggplot(edge_RR_RARIFY, aes(x=mean.ctrl.cov, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Mean Cov (space/time)") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

logmeanD3 = ggplot(edge_RR_RARIFY, aes(x=log(mean.ctrl.cov), y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

logmeanR3 <- ggplot(edge_RR_RARIFY, aes(x=log(mean.ctrl.cov), y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Log Mean Cov (space/time)") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

absrankD3 = ggplot(edge_RR_RARIFY, aes(x=absrank, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

absrankR3 <- ggplot(edge_RR_RARIFY, aes(x=absrank, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Absolute Rank") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(rankD3, absrankD3, persD3, meanD3, logmeanD3, rankR3, absrankR3, persR3, meanR3, logmeanR3,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 5, nrow=2)

ggsave("figures/Nov2024_meeting/figure3_expanded_RARIFY.png", width = 16, height = 8.5)


ggplot(edge_RR_RARIFY, aes(x = percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(size = 1.5) +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse()

ggsave("figures/Nov2024_meeting/figure2_RARIFY.png", width = 6.5, height = 8)

