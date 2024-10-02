# Set up ####
## read in data ####
source("data-prep/clean_edge_data.R")
source("data-prep/classify_rank_persistence.R")
theme_set(theme_classic())

source("data-prep/clean_ppt_data.R")
library(lubridate)
library(cowplot)

## do a dominance metric on sites? 

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Calc means ####
## add in drought start & end years for graphing later
edge_all <- edge_all %>%
  mutate(drought_start_year = ifelse(site %in% c("SBL", "SBK"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SBL", "SBK"), 2016, 2017))

## merge with rank and persistence values for each species
edge_rarity <- left_join(edge_all, rank_persist, by = c("site", "species"))

edge_rarity$species <- as.factor(edge_rarity$species)

edge_rarity2 <- edge_rarity %>%
  mutate(rarity = ifelse(percrank > 0.98, "dominant species", "subordinate species"))

ggplot(edge_rarity2, aes(x=mean.plot.cover)) +
  geom_histogram() +
 # facet_grid(vars(rarity),vars(site)) +
  facet_wrap(~site)+
  geom_vline(xintercept = 100)

## total cover ####
edge_sum_plot_tc <- edge_rarity %>%
  #mutate(rarity = ifelse(percrank > 0.98, "dominant species", "subordinate species")) %>% ## classify sp by abundance
  group_by(site, treatment, year, block, plot) %>% 
  ## take plot means of dom vs. sub sp
  summarise(tot.cov = sum(mean.plot.cover)) %>%
  mutate(drought_start_year = ifelse(site %in% c("SBL", "SBK"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SBL", "SBK"), 2016, 2017))

## visualize
ggplot(edge_sum_plot_tc, aes(x=tot.cov)) + 
  geom_histogram() +
  facet_wrap(~site) +
  geom_vline(xintercept = 100)


edge_sum_site_tc <- edge_sum_plot_tc %>%
  group_by(site, treatment, year) %>%
  summarise(mean.tot.cov = mean(tot.cov),
            se.tot.cov = calcSE(tot.cov)) %>%
  mutate(drought_start_year = ifelse(site %in% c("SBL", "SBK"), 2013, 2014),
                                                 drought_end_year = ifelse(site %in% c("SBL", "SBK"), 2016, 2017))

edge_sum_site_tc$site = as.factor(edge_sum_site_tc$site)
edge_sum_site_tc = edge_sum_site_tc %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Figure S4 ####
## visualize
ggplot(edge_sum_site_tc, aes(x=year, y=mean.tot.cov, linetype = treatment))+
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin = mean.tot.cov - se.tot.cov, ymax = mean.tot.cov + se.tot.cov), width = 0.25) +
  facet_wrap(~site, ncol = 2, nrow = 3, scales = "free") +
  ylab("Mean Total Cover") +
  xlab("Year")

ggsave("preliminary_figs/june_2024/total_cover.png", width = 6, height = 5)

ggplot(edge_sum_plot, aes(x=sum.cov)) + 
  geom_histogram() +
  facet_grid(vars(rarity),vars(site)) +
  geom_vline(xintercept = 100)

## plot level ####
## calculate mean cover for dom vs. sub sp at the plot level in each year
edge_sum_plot <- edge_rarity %>%
  mutate(rarity = ifelse(percrank > 0.98, "dominant species", "subordinate species")) %>% ## classify sp by abundance
  group_by(site, treatment, year, block, plot, rarity) %>% 
  ## take plot means of dom vs. sub sp
  summarise(mean.cov = mean(mean.plot.cover),
            sum.cov = sum(mean.plot.cover))

ggplot(edge_sum_plot, aes(x=sum.cov)) + 
  geom_histogram() +
  facet_grid(vars(rarity),vars(site)) +
  geom_vline(xintercept = 100)

## site level ####
## calc mean cover for dom vs. sub sp at the site level in each year
edge_sum_site <- edge_sum_plot %>%
  ungroup() %>%
  group_by(site, treatment, rarity, year) %>%
  summarise(mean.cover = mean(mean.cov),
            se.cover = calcSE(mean.cov),
            #sp = list(species),
            mean.sum.cover = mean(sum.cov),
            se.sum.cover = calcSE(sum.cov)) %>%
  mutate(drought_start_year = ifelse(site %in% c("SBL", "SBK"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SBL", "SBK"), 2016, 2017)) %>%
  mutate(fakeM = 10,
         fakeD = 25,
         fulldate = paste0(year, fakeM, fakeD),
         fulldate2 = ymd(fulldate),
         year2 = year(fulldate2))

edge_sum_site$site <- factor(edge_sum_site$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## create line data for graphing
line_data <- edge_sum_site %>%
  ungroup() %>%
  select(site, drought_start_year, drought_end_year) %>%
  distinct()

str(edge_sum_site)

edge_sum_site$year <- as.character(edge_sum_site$year)
edge_sum_site$year <- as.Date(edge_sum_site$year, format = "%Y")

# Figure 2 ####
dom <- ggplot(edge_sum_site[edge_sum_site$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment), linewidth= 1.25) +
  geom_point(size = 2) +
  facet_grid(site~rarity, scales = "free") +
 # scale_color_manual(values = c("#175149")) +
  xlab("Year") +  ylab("Mean Species Cover") +
  labs(linetype = "Rainfall Treatment") +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

sub <- ggplot(edge_sum_site[edge_sum_site$rarity == "subordinate species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment), linewidth= 1.25) +
  geom_point(size = 2) +
  facet_grid(site~rarity, scales = "free") +
  #scale_color_manual(values = c( "#BDC881")) +
  xlab("Year") +  ylab(NULL) +
  labs(linetype = "Rainfall Treatment") +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom") +
  guides(linetype = "none")

ggarrange(dom, sub, common.legend = T, legend = "bottom")

ggsave("preliminary_figs/aug_sept_2024/figure2_meansp_cov_updated.tiff", width = 8, height = 8)
ggsave("preliminary_figs/aug_sept_2024/figure2_meansp_cov_updated.png", width = 8, height = 8)

# Figure S5 ####
ggplot(edge_sum_site, aes(x=as.integer(year2), y=mean.sum.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_wrap(~site, scales = "free", nrow = 3, ncol = 2) +
  scale_color_manual(values = c("#175149", "#BDC881")) +
  xlab("Year") +  ylab("Mean Total Cover") +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.sum.cover - se.sum.cover, ymax = mean.sum.cover + se.sum.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

ggsave("preliminary_figs/june_2024/mean_total_cover_by_RA.png", width = 8, height = 8)

# Rank Abundance ####
rank_mean_all <- edge_rarity2 %>%
  group_by(site, treatment, rarity, species) %>% 
  summarise(mean.cov = mean(mean.plot.cover), 
            rank = median(percrank))

rank_mean_all$site = as.factor(rank_mean_all$site)
rank_mean_all = rank_mean_all %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

ggplot(rank_mean_all, aes(x=-(rank), y=mean.cov, color = treatment))+
  geom_point() +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  ylab("Cover") +
  xlab("Rank") +
  scale_color_manual(values = c("#008080", "#de8a5a"))

ggsave("preliminary_figs/june_2024/rank_abund_curves.png", width = 6, height = 5)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

