
# Old Figure 2 ####
KNZd <- ggplot(edge_rare[edge_rare$site == "KNZ" & edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  ylab("Mean Cover") + xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

KNZr <- ggplot(edge_rare[edge_rare$site == "KNZ" & edge_rare$rarity == "rare species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#BDC881")) +
  ylab("Mean Cover") + xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

HYSd <- ggplot(edge_rare[edge_rare$site == "HYS" & edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  ylab(NULL) + xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

HYSr <- ggplot(edge_rare[edge_rare$site == "HYS" & edge_rare$rarity == "rare species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#BDC881")) +
  ylab(NULL) +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

CHYd <- ggplot(edge_rare[edge_rare$site == "CHY" & edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  ylab(NULL) +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

CHYr <- ggplot(edge_rare[edge_rare$site == "CHY" & edge_rare$rarity == "rare species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c( "#BDC881")) +
  ylab(NULL) + xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

SGSd <- ggplot(edge_rare[edge_rare$site == "SGS" & edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  ylab("Mean Cover") +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

SGSr <- ggplot(edge_rare[edge_rare$site == "SGS" & edge_rare$rarity == "rare species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#BDC881")) +
  ylab("Mean Cover") +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

SEV_blued <- ggplot(edge_rare[edge_rare$site == "SBL" & edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  ylab(NULL) +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

SEV_bluer <- ggplot(edge_rare[edge_rare$site == "SBL" & edge_rare$rarity == "rare species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#BDC881")) +
  ylab(NULL) +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

SEV_blackd <- ggplot(edge_rare[edge_rare$site == "SBK" & edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  ylab(NULL) +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

SEV_blackr <- ggplot(edge_rare[edge_rare$site == "SBK" & edge_rare$rarity == "rare species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#BDC881")) +
  ylab(NULL) +  xlab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

## all together ####
ggarrange(KNZd, HYSd, CHYd, 
          KNZr, HYSr, CHYr,
          SGSd, SEV_blued, SEV_blackd, 
          SGSr, SEV_bluer, SEV_blackr, 
          ncol = 3, nrow = 4, common.legend = T, legend = "bottom", 
          labels = "AUTO")

ggsave("preliminary_figs/march_2024/dom_rare_cover_responses_time.png", width = 10, height = 10)




p <- ggplot(edge_rare, aes(x=as.integer(year2), y=mean.cover, color = rarity)) +
  #geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 3) +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  labs(color = NULL, linetype = "Treatment", ) +
  theme(text = element_text(size = 13)) +
  theme(legend.position="bottom")

legend <- get_legend(p)
as_ggplot(legend)


edge_rare[edge_rare$site == "KNZ" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "CHY" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "HYS" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "SGS" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "SEV_black" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "SEV_blue" & edge_rare$rarity == "dom",]$sp

ggplot(edge_sum[edge_sum$site == "KNZ" & edge_sum$percrank < 0.7,], aes(x=year, y=mean.cov, color = treatment)) +
  facet_wrap(~species, scales = "free", ncol = 6) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#008080", "#ca562c"))

ggplot(edge_sum[edge_sum$site == "HYS",], aes(x=year, y=mean.cov, color = treatment)) +
  facet_wrap(~species, scales = "free") +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#008080", "#ca562c"))

# Relative Cover 
edge_rare_rel <- edge_sum %>%
  mutate(rarity = ifelse(percrank > 0.98, "dom", "rare")) %>%
  ungroup() %>%
  group_by(site, treatment, rarity, year) %>%
  summarise(mean.relative.cov = mean(mean.rel.cov), 
            sp = list(species)) %>%
  mutate(drought_start_year = ifelse(site %in% c("SEV_blue", "SEV_black"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SEV_blue", "SEV_black"), 2016, 2017))

edge_rare_rel$site <- factor(edge_rare$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

ggplot(edge_rare_rel, aes(x=year, y=mean.relative.cov, color = treatment, shape = rarity)) +
  geom_point(size = 1.5) +
  geom_line() +
  facet_wrap(~site*rarity, scales = "free") +
  scale_color_manual(values = c("#008080", "#ca562c")) +
  scale_shape_manual(values = c(19, 1)) +
  ylab("Mean Cover") +
  xlab("Year") +
  labs(color = "Treatment", shape = "Rarity") +
  geom_vline(aes(xintercept = drought_start_year), data = line_data) +
  geom_vline(aes(xintercept = drought_end_year), data = line_data)




# Total cover ####
ggplot(edge_all, aes(x=year, y=total.plot.cover, color = treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site) +
  geom_vline(aes(xintercept = drought_start_year), data = line_data) +
  geom_vline(aes(xintercept = drought_end_year), data = line_data)

edge_meancov <- edge_all %>%
  group_by(site, year, treatment) %>%
  summarise(mean.cov = mean(total.plot.cover), 
            se.cov = calcSE(total.plot.cover))

edge_meancov$site <- as.factor(edge_meancov$site)
edge_meancov <- edge_meancov %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

ggplot(edge_meancov, aes(x=year, y=mean.cov, color = treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  scale_color_manual(values = c("#008080", "#ca562c")) +
  ylab("Average Total Cover") +
  xlab("Year") +
  labs(color = "Treatment") +
  geom_errorbar(aes(ymin = mean.cov - se.cov, ymax = mean.cov + se.cov), width = 0.25) +
  geom_vline(aes(xintercept = drought_start_year), data = line_data, linetype = "dashed") +
  geom_vline(aes(xintercept = drought_end_year), data = line_data, linetype = "dashed")

ggsave("preliminary_figs/meeting_jan_2024/total_cov_timeseries.png", width = 5, height = 4.5)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c



# OLD ####

# Indiv species cover ####
ggplot(edge_all[edge_all$site == "KNZ",], aes(x=year, y=mean.plot.cover, color = as.factor(plot), shape = treatment)) +
  facet_wrap(~species, scales = "free") +
  geom_point() +
  geom_line() +
  scale_shape_manual(values = c(19, 1))

## precip & cov
ppt <- growing.season.tot %>%
  mutate(year = Year) %>%
  ungroup() %>%
  select(-Year, -Site)

northern <- edge_all %>%
  filter(site %in% c("KNZ", "HYS", "CHY", "SGS"),
         treatment == "C")

test <- left_join(northern, growing.season.tot, by = c("site", "year"))

test2 <- left_join(test, rank_persist, by = c("site", "species")) %>%
  group_by(site, treatment, species, year, percrank, persistence.site) %>%
  summarise(mean.cov = mean(mean.plot.cover),
            se.cov = calcSE(mean.plot.cover), 
            mean.precip = mean(tot.precip))

test3 <- test2 %>%
  mutate(rarity = ifelse(percrank > 0.98, "dom", "rare")) %>%
  ungroup() %>%
  group_by(site, treatment, rarity, year, mean.precip) %>%
  summarise(mean.cover = mean(mean.cov), 
            sp = list(species))


ggplot(test3, aes(x=mean.precip, y=mean.cover, color = treatment)) +
  geom_point() +
  facet_wrap(~site*rarity, scale = "free") #+
#geom_smooth()
#scale_color_viridis_c(direction = -1)




