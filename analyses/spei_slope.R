## Set up env
source("data-prep/clean_spei_data.R")
source("data-prep/classify_rank_persistence.R")
theme_set(theme_classic())

# Calculate Slope ####
## absolute cover ####
slope.abs <- edge_w_spei %>%
  group_by(site, species) %>%
  summarise(slope = lm(mean.plot.cover~spei)$coefficients[2]) %>%
  mutate(slope.magnitude = abs(slope))

slope_abs_w_rankpersist <- left_join(slope.abs, rank_persist, by = c("site", "species"))

slope_abs_w_rankpersist$site <- as.factor(slope_abs_w_rankpersist$site)
slope_abs_w_rankpersist <- slope_abs_w_rankpersist %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

## relative cover ####
slope.rel <- edge_w_spei %>%
  group_by(site, species) %>%
  summarise(slope = lm(relative.sp.cover~spei)$coefficients[2]) %>%
  mutate(slope.magnitude = abs(slope))

slope_rel_w_rankpersist <- left_join(slope.rel, rank_persist, by = c("site", "species"))

slope_rel_w_rankpersist$site <- as.factor(slope_rel_w_rankpersist$site)
slope_rel_w_rankpersist <- slope_rel_w_rankpersist %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

# Visualize ####
## absolute cover ####
### rank ####
## plain slope, no modifications
ggplot(slope_abs_w_rankpersist, aes(x=percrank, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Slope of Abs Cover v. SPEI")

## slope magnitude
ggplot(slope_w_rankpersist, aes(x=percrank, y=slope.magnitude, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Magnitude of Slope (Abs Cover v. SPEI)")

## log of slope magnitude
ggplot(slope_w_rankpersist, aes(x=percrank, y=log(slope.magnitude), color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Log(Magnitude of Slope) (Abs Cover v. SPEI)")

## relative cover ####
### rank ####
## plain slope, no modifications
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Slope of Rel Cover v. SPEI")

## slope magnitude
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=slope.magnitude, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Magnitude of Slope (Rel Cover v. SPEI)")

## slope magnitude
ggplot(slope_rel_w_rankpersist, aes(x=percrank, y=log(slope.magnitude), color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Log(Slope Magnitude), (Rel Cover v. SPEI)")

ggsave("preliminary_figs/slope_rank_persistence/rel_cov_log_slope_magnitude_rank.png", width = 6, height = 4)

### persistence ####
ggplot(slope_rel_w_rankpersist, aes(x=persistence.site, y=slope, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free")





