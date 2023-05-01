
library(tidyverse)
## Read in Data
## species categorizations (synchrony data)
source("shape-shifting-subordinates/clean_synchrony_data.R")
## EDGE data
source("shape-shifting-subordinates/clean_merge_all_edge_data.R")

source("shape-shifting-subordinates/categorize_species_CHY_SGS.R")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

theme_set(theme_bw())

ggplot(field_catdat, aes(x=nickname)) +
  geom_bar() +
  facet_wrap(~site)

ggplot(field_catdat, aes(x=mpersist_pct)) +
  geom_histogram()
## much better


## CHY and SGS sites still need categorization

## Join species classifications with EDGE data

sev_edge <- edge_all %>%
  filter(site == "SEV_black" | site == "SEV_blue") %>%
  #mutate(field = site) %>%
  #select(-site) %>%
  #mutate(site = "sev") %>%
  #filter(site == "")
  mutate(species.name = species) %>%
  select(-species) %>%
  mutate(species = kartez)

sev_edge_class <- left_join(sev_edge, field_catdat[field_catdat$site == "sev",], by = c("species")) %>%
  select(-species) %>%
  mutate(species = species.name) %>%
  select(-species.name) %>%
  mutate(site = site.x) %>%
  select(-site.y, -site.x)
  

knz_edge <- edge_all %>%
  filter(site == "KNZ") %>%
  mutate(site = "knz")


field_catdat_knz <- field_catdat %>%
  filter(site == "knz") %>%
  mutate(genuscode = toupper(substr(species, 1, 3)), 
         sp.ep = strsplit(species, "_") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-genuscode, -sp.ep, -spepcode, -species)

knz_edge_class <- left_join(knz_edge, field_catdat_knz, by = c("spcode", "site"))# %>%
  #mutate(field = NA)

field_catdat_hay <- field_catdat %>%
  filter(site == "hay") %>%
  mutate(genuscode = toupper(substr(species, 1, 3)), 
         sp.ep = strsplit(species, " ") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-genuscode, -sp.ep, -spepcode, -species)


hay_edge <- edge_all %>%
  filter(site == "HYS") %>%
  mutate(site = "hay")

hay_edge_class <- left_join(hay_edge, field_catdat_hay, by = c("spcode", "site")) #%>%
  #mutate(field = NA)

# CHY & SGS ####
chy_sgs_class <- left_join(edge_all[edge_all$site == "SGS" | edge_all$site == "CHY", ], field_catdat_cs, by = c("site", "species"))



## join together
temp <- rbind(sev_edge_class, knz_edge_class)
edge3 <- rbind(temp, hay_edge_class)

edge_all_class <- rbind(edge3, chy_sgs_class)



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
ggsave("shape-shifting-subordinates/preliminary_figs/edge_all_temp_cats_grouped.png", height = 7, width = 9)

ggplot(releveled_overall[releveled_overall$nickname != "CoreDom" & !is.na(releveled_overall$nickname),], aes(x=site, y=mean_cover, color = treatment)) +
  geom_errorbar(aes(ymin = mean_cover - se_cover, ymax = mean_cover + se_cover), color = "black", width = 0.2) +
  geom_point(size = 2) +
  facet_wrap(~nickname, scales = "free") +
  scale_color_manual(values = c("#003366", "#FFA630"))
  

ggsave("shape-shifting-subordinates/preliminary_figs/sub_drought_cover_resp.png", height = 2.5, width = 7)





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

ggsave("shape-shifting-subordinates/preliminary_figs/recov_all_sites.png", height = 3.5, width = 5)

