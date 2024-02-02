source("data-prep/clean_edge_data.R")
source("data-prep/classify_rank_persistence.R")
theme_set(theme_classic())


source("data-prep/clean_ppt_data.R")

## do a dominance metric on sites? 

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

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
  
  
  





## merge with rank and persistence values for each species
edge_rarity <- left_join(edge_all, rank_persist, by = c("site", "species"))

edge_rarity$species <- as.factor(edge_rarity$species)

edge_sum <- edge_rarity %>%
  group_by(site, treatment, species, year, percrank, persistence.site) %>%
  summarise(mean.cov = mean(mean.plot.cover),
            se.cov = calcSE(mean.plot.cover))



  #ungroup() %>%
  #group_by(site) %>%
  #factor(species,levels=edge_sum$percrank,ordered=TRUE)

edge_rare <- edge_sum %>%
  mutate(rarity = ifelse(percrank > 0.98, "dom", "rare")) %>%
  ungroup() %>%
  group_by(site, treatment, rarity, year) %>%
  summarise(mean.cover = mean(mean.cov), 
            sp = list(species))

edge_rare$site <- factor(edge_rare$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

ggplot(edge_rare, aes(x=year, y=mean.cover, color = treatment, shape = rarity)) +
  geom_point(size = 1.5) +
  geom_line() +
  facet_wrap(~site*rarity, scales = "free") +
  scale_color_manual(values = c("#008080", "#ca562c")) +
  scale_shape_manual(values = c(15, 20)) +
  ylab("Mean Cover") +
  xlab("Year") +
  labs(color = "Treatment", shape = "Rarity")

ggsave("preliminary_figs/meeting_jan_2024/dom_rare_cover_responses_time.png", width = 7, height = 4)

edge_rare[edge_rare$site == "KNZ" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "CHY" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "HYS" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "SGS" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "SEV_black" & edge_rare$rarity == "dom",]$sp
edge_rare[edge_rare$site == "SEV_blue" & edge_rare$rarity == "dom",]$sp


#edge_sum$species<-factor(edge_sum$species,levels=edge_sum$percrank,ordered=TRUE)




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




# Total cover ####
ggplot(edge_all, aes(x=year, y=total.plot.cover, color = treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site)

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
  geom_errorbar(aes(ymin = mean.cov - se.cov, ymax = mean.cov + se.cov), width = 0.25)

ggsave("preliminary_figs/meeting_jan_2024/total_cov_timeseries.png", width = 5, height = 4.5)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

# Indiv species cover ####
ggplot(edge_all[edge_all$site == "KNZ",], aes(x=year, y=mean.plot.cover, color = as.factor(plot), shape = treatment)) +
  facet_wrap(~species, scales = "free") +
  geom_point() +
  geom_line() +
  scale_shape_manual(values = c(19, 1))










