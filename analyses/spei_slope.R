# Header #### 
## Script name: SPEI Slope
##
## Purpose of script: Calculate the slope between each species cover and the SPEI values in a particular year. Explore the relationship between this slope and a species' rank and persistence. 
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

## NOTE: I'm not sure if this is a useful analysis- leaving the code here so we can come back to it if desired, but it's not top priority!

## Set up env ####
source("data-prep/clean_spei_data.R") ## read in spei data
source("data-prep/clean_edge_data.R")
source("data-prep/classify_rank_persistence.R")
theme_set(theme_classic())

## merge spei with edge data
edge_w_spei <- left_join(edge_all, spei.exp, by = c("site", "year")) %>%
  filter(treatment == "C")

# Calculate Slope ####
## absolute cover ####
slope.abs <- edge_w_spei %>%
  group_by(site, species) %>%
  summarise(slope = lm(mean.plot.cover~spei)$coefficients[2]) %>%
  mutate(slope.magnitude = abs(slope), 
         slope.direction = ifelse(slope > 0, "positive", "negative")) 

## join with rank & persistence data
slope_abs_w_rankpersist <- left_join(slope.abs, rank_persist, by = c("site", "species"))

## reorder sites
slope_abs_w_rankpersist$site <- as.factor(slope_abs_w_rankpersist$site)
slope_abs_w_rankpersist <- slope_abs_w_rankpersist %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

## relative cover ####
slope.rel <- edge_w_spei %>%
  group_by(site, species) %>%
  summarise(slope = lm(relative.sp.cover~spei)$coefficients[2]) %>%
  mutate(slope.magnitude = abs(slope),
         slope.direction = ifelse(slope > 0, "positive", "negative"))

## join with rank & persistence data
slope_rel_w_rankpersist <- left_join(slope.rel, rank_persist, by = c("site", "species"))

## reorder sites
slope_rel_w_rankpersist$site <- as.factor(slope_rel_w_rankpersist$site)
slope_rel_w_rankpersist <- slope_rel_w_rankpersist %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

# Visualize ####
## absolute cover ####
### plain slope ####
#### rank ####
ggplot(slope_abs_w_rankpersist, aes(x=percrank, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("% Rank") +
  ylab("Slope of Abs Cover v. SPEI")

ggsave("preliminary_figs/slope_rank_persistence/abs_cov_slope_rank.png", width = 6, height = 4)

#### persistence ####
ggplot(slope_abs_w_rankpersist, aes(x=persistence.site, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Slope of Abs Cover v. SPEI")

ggsave("preliminary_figs/slope_rank_persistence/abs_cov_slope_persist.png", width = 6, height = 4)

### slope magnitude ####
#### rank ####
ggplot(slope_abs_w_rankpersist, aes(x=percrank, y=slope.magnitude, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Magnitude of Slope (Abs Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/abs_cov_mag_slope_rank.png", width = 6, height = 4)

#### persistence ####
ggplot(slope_abs_w_rankpersist, aes(x=persistence.site, y=slope.magnitude, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Magnitude of Slope (Abs Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/abs_cov_mag_slope_persist.png", width = 6, height = 4)

### log of slope magnitude ####
#### rank ####
ggplot(slope_abs_w_rankpersist, aes(x=percrank, y=log(slope.magnitude), color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Log(Magnitude of Slope) (Abs Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/abs_cov_log_mag_slope_rank.png", width = 6, height = 4)

#### persistence ####
ggplot(slope_abs_w_rankpersist, aes(x=persistence.site, y=log(slope.magnitude), color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Log(Magnitude of Slope) (Abs Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/abs_cov_log_mag_slope_persist.png", width = 6, height = 4)

## relative cover ####
### plain slope ####
#### rank ####
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope of Rel Cover v. SPEI")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_slope_rank.png", width = 6, height = 4)

#### persistence ####
ggplot(slope_rel_w_rankpersist, aes(x=persistence.site, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope of Rel Cover v. SPEI")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_slope_persist.png", width = 6, height = 4)

### slope magnitude ####
#### rank ####
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=slope.magnitude, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Magnitude of Slope (Rel Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_mag_slope_rank.png", width = 6, height = 4)

#### persistence ####
ggplot(slope_rel_w_rankpersist, aes(x=persistence.site, y=slope.magnitude, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Magnitude of Slope (Rel Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_mag_slope_persist.png", width = 6, height = 4)

### log of slope magnitude ####
#### rank ####
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=log(slope.magnitude), color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Log(Slope Magnitude), (Rel Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_log_mag_slope_rank.png", width = 6, height = 4)

ggplot(slope_rel_w_rankpersist[!is.na(slope_rel_w_rankpersist$slope.magnitude),], aes(x=percrank, y=log(slope.magnitude), color = site)) +
  geom_point() +
  #facet_wrap(~slope.direction) +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Log(Slope Magnitude), (Rel Cover v. SPEI)")

#### persistence ####
ggplot(slope_rel_w_rankpersist, aes(x=persistence.site, y=log(slope.magnitude), color = site)) +
  geom_point() +
  facet_wrap(~site) +
  #geom_smooth()+
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Log(Slope Magnitude), (Rel Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_log_mag_slope_persist.png", width = 6, height = 4)

# Explore Slope Direction ####
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Slope of Rel Cover v. SPEI") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggplot(slope_abs_w_rankpersist, aes(x=percrank, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Slope of Abs Cover v. SPEI") +
  geom_hline(yintercept = 0, linetype = "dashed")
