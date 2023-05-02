source("data-prep/join_control_edge_data.R")

# Explore ####
cats_grouped <- edge_all_class %>%
  group_by(site, treatment, year, nickname) %>%
  summarise(mean_cover = mean(max.cover, na.rm = T)) 

cats_grouped_overall <- edge_all_class %>%
  group_by(site, treatment, nickname) %>%
  summarise(mean_cover = mean(max.cover, na.rm = T),
            se_cover = calcSE(max.cover))

cats_grouped$site <- as.factor(cats_grouped$site)
cats_grouped_overall$site <- as.factor(cats_grouped_overall$site)

releveled <- cats_grouped %>%
  mutate(site = fct_relevel(site, "knz", "hay", "CHY", "SGS", "SEV_blue", "SEV_black"))
releveled_overall <- cats_grouped_overall %>%
  mutate(site = fct_relevel(site, "knz", "hay", "CHY", "SGS", "SEV_blue", "SEV_black"))

ggplot(releveled, aes(x=year, y=mean_cover, color = nickname)) +
  geom_line() +
  facet_wrap(~site*treatment, scales = "free")
ggsave("preliminary_figs/edge_all_temp_cats_grouped.png", height = 7, width = 9)

ggplot(releveled_overall[releveled_overall$nickname != "CoreDom" & !is.na(releveled_overall$nickname),], aes(x=site, y=mean_cover, color = treatment)) +
  geom_errorbar(aes(ymin = mean_cover - se_cover, ymax = mean_cover + se_cover), color = "black", width = 0.2) +
  geom_point(size = 2) +
  facet_wrap(~nickname, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  theme(axis.text.x = element_text(angle = 45))


ggsave("preliminary_figs/sub_drought_cover_resp.png", height = 3, width = 8)





time.period.sum <- edge_all_class %>%
  mutate(time.period = ifelse(treatment == "D" & year > 2017, "recovery", 
                              ifelse(treatment == "D" & year < 2018, "drought", "control"))) %>%
  group_by(site, treatment, nickname, time.period) %>%
  summarise(mean_cover = mean(max.cover, na.rm = T), se_cover = calcSE(max.cover)) %>%
  filter(!is.na(nickname))



time.period.sum$site <- as.factor(time.period.sum$site)

time.period.sum <- time.period.sum %>%
  mutate(site = fct_relevel(site, "knz", "hay", "CHY", "SGS", "SEV_blue", "SEV_black"))


ggplot(time.period.sum[time.period.sum$time.period != "control",], aes(x=time.period, y=mean_cover, color = site)) +
  #geom_errorbar(aes(ymin = mean_cover - se_cover, ymax = mean_cover + se_cover), color = "black", width = 0.25) +
  geom_point(size = 3) + 
  geom_line(aes(group = site)) +
  facet_wrap(~nickname, scales = "free") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a", "#de8a5a", "#ca562c"))
#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

ggsave("preliminary_figs/recov_all_sites.png", height = 3.5, width = 5)
