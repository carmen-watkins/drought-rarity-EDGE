# Set up ####
## Response Ratio of various time windows
## look at 2-final years of drought vs. first 2 years of recovery
## look at 3 year windows also
library(tidyverse)

## load data
source("data-prep/classify_rank_persistence.R")
#source("data-prep/clean_edge_data.R")
FG <- read.csv("data/edge_species_info.csv")

theme_set(theme_classic())

# Calculate RR ####
## 2 Year ####
### Drought ####
resp.ratio.site.2yr <- edge_all %>%
  filter(experiment.year %in% c(3:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
edge_w_predictors.site.2yr <- left_join(resp.ratio.site.2yr, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.2yr$site <- as.factor(edge_w_predictors.site.2yr$site)
edge_w_predictors.site.2yr <- edge_w_predictors.site.2yr %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### Recovery ####
resp.ratio.site.recov.2yr <- edge_all %>%
  filter(experiment.year %in% c(5:6)) %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank and persistence values for each species
edge_w_predictors.site.recov.2yr <- left_join(resp.ratio.site.recov.2yr, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.recov.2yr$site <- as.factor(edge_w_predictors.site.recov.2yr$site)
edge_w_predictors.site.recov.2yr <- edge_w_predictors.site.recov.2yr %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### Add Funct Grp ####
edge_temp <- edge_w_predictors.site.2yr %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG.2yr <- left_join(edge_temp, FG, by = "species")

edge_temp2 <- edge_w_predictors.site.recov.2yr %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG_recov.2yr <- left_join(edge_temp2, FG, by = "species")

### Merge DR & RR ####
drought.RR.2yr <- edge_FG.2yr %>%
  mutate(resp.ratio.drought = resp.ratio.site,
         mean.cov.drought = mean.cov) %>%
  select(site, species, resp.ratio.drought, mean.cov.drought, persistence.site, percrank, FunctionalGroup)

recov.RR.2yr <- edge_FG_recov.2yr %>%
  mutate(resp.ratio.recov = resp.ratio.site,
         mean.cov.recov = mean.cov) %>%
  select(site, species, resp.ratio.recov, mean.cov.recov, FunctionalGroup)

response.ratio.tog.2yr <- left_join(drought.RR.2yr, recov.RR.2yr, by = c("site", "species", "FunctionalGroup")) %>%
  mutate(precip.bin = ifelse(site %in% c("KNZ", "HYS"), "high",
                             ifelse(site %in% c("CHY", "SGS"), "med", "low")))

## make precip bins an ordered factor
response.ratio.tog.2yr$precip.bin <- as.factor(response.ratio.tog.2yr$precip.bin)

response.ratio.tog.2yr <- response.ratio.tog.2yr %>%
  mutate(precip.bin = fct_relevel(precip.bin, "high", "med", "low"))


## 3 Year ####
### Drought ####
resp.ratio.site.3yr <- edge_all %>%
  filter(experiment.year %in% c(2:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
edge_w_predictors.site.3yr <- left_join(resp.ratio.site.3yr, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.3yr$site <- as.factor(edge_w_predictors.site.3yr$site)
edge_w_predictors.site.3yr <- edge_w_predictors.site.3yr %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### Recovery ####
resp.ratio.site.recov.3yr <- edge_all %>%
  filter(experiment.year %in% c(5:7)) %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank and persistence values for each species
edge_w_predictors.site.recov.3yr <- left_join(resp.ratio.site.recov.3yr, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.recov.3yr$site <- as.factor(edge_w_predictors.site.recov.3yr$site)
edge_w_predictors.site.recov.3yr <- edge_w_predictors.site.recov.3yr %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### Add Funct Grp ####
edge_temp <- edge_w_predictors.site.3yr %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG.3yr <- left_join(edge_temp, FG, by = "species")

edge_temp2 <- edge_w_predictors.site.recov.3yr %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG_recov.3yr <- left_join(edge_temp2, FG, by = "species")

### Merge DR & RR ####
drought.RR.3yr <- edge_FG.3yr %>%
  mutate(resp.ratio.drought = resp.ratio.site,
         mean.cov.drought = mean.cov) %>%
  select(site, species, resp.ratio.drought, mean.cov.drought, persistence.site, percrank, FunctionalGroup)

recov.RR.3yr <- edge_FG_recov.3yr %>%
  mutate(resp.ratio.recov = resp.ratio.site,
         mean.cov.recov = mean.cov) %>%
  select(site, species, resp.ratio.recov, mean.cov.recov, FunctionalGroup)

response.ratio.tog.3yr <- left_join(drought.RR.3yr, recov.RR.3yr, by = c("site", "species", "FunctionalGroup")) %>%
  mutate(precip.bin = ifelse(site %in% c("KNZ", "HYS"), "high",
                             ifelse(site %in% c("CHY", "SGS"), "med", "low")))

## make precip bins an ordered factor
response.ratio.tog.3yr$precip.bin <- as.factor(response.ratio.tog.3yr$precip.bin)

response.ratio.tog.3yr <- response.ratio.tog.3yr %>%
  mutate(precip.bin = fct_relevel(precip.bin, "high", "med", "low"))


# Visualize ####
## 2 Year ####
### Functional Groups ####
ggplot(response.ratio.tog.2yr, aes(x=resp.ratio.drought, resp.ratio.recov, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  ggtitle("2-year Response Ratio Window")

ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR_FG_site_2yr.png", width = 5, height = 4.5)

### Precip Bins ####
ggplot(response.ratio.tog.2yr, aes(x=resp.ratio.drought, resp.ratio.recov, color = precip.bin)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~precip.bin, nrow = 1, ncol = 3) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#42B7B9","#ca562c", "#D691C1")) +
  labs(color="Relative PPT Bin") +
  ggtitle("2-year Response Ratio Window")

ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR_2yr_window_ppt_bins.png", width = 7.5, height = 3)

## 3 Year ####
### Functional Groups ####
ggplot(response.ratio.tog.3yr, aes(x=resp.ratio.drought, resp.ratio.recov, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  ggtitle("3-year Response Ratio Window")

ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR_FG_site_3yr.png", width = 5, height = 4.5)

### Precip Bins ####
ggplot(response.ratio.tog.3yr, aes(x=resp.ratio.drought, resp.ratio.recov, color = precip.bin)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~precip.bin, nrow = 1, ncol = 3) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#42B7B9","#ca562c", "#D691C1")) +
  labs(color="Relative PPT Bin") +
  ggtitle("3-year Response Ratio Window")

ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR_3yr_window_ppt_bins.png", width = 7.5, height = 3)



