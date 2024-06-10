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
  mutate(drought_start_year = ifelse(site %in% c("SEV_blue", "SEV_black"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SEV_blue", "SEV_black"), 2016, 2017))

## merge with rank and persistence values for each species
edge_rarity <- left_join(edge_all, rank_persist, by = c("site", "species"))

edge_rarity$species <- as.factor(edge_rarity$species)

## calculate mean cover for dom vs. sub sp at the plot level in each year
edge_sum_plot <- edge_rarity %>%
  mutate(rarity = ifelse(percrank > 0.98, "dominant species", "subordinate species")) %>% ## classify sp by abundance
  group_by(site, treatment, year, block, plot, rarity) %>% 
  ## take plot means of dom vs. sub sp
  summarise(mean.cov = mean(mean.plot.cover),
            sum.cov = sum(mean.plot.cover)) %>%
  mutate(site = ifelse(site == "SEV_blue", "SBL",
                       ifelse(site == "SEV_black", "SBK", site)))

## calc mean cover for dom vs. sub sp at the site level in each year
edge_rare <- edge_sum_plot %>%
  ungroup() %>%
  group_by(site, treatment, rarity, year) %>%
  summarise(mean.cover = mean(mean.cov),
            se.cover = calcSE(mean.cov),
            #sp = list(species),
            mean.sum.cover = mean(sum.cov),
            se.sum.cover = calcSE(sum.cov)) %>%
  mutate(drought_start_year = ifelse(site %in% c("SEV_blue", "SEV_black"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SEV_blue", "SEV_black"), 2016, 2017)) %>%
  mutate(fakeM = 10,
         fakeD = 25,
         fulldate = paste0(year, fakeM, fakeD),
         fulldate2 = ymd(fulldate),
         year2 = year(fulldate2), 
         site = ifelse(site == "SEV_blue", "SBL",
                       ifelse(site == "SEV_black", "SBK", site)))

edge_rare$site <- factor(edge_rare$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## create line data for graphing
line_data <- edge_rare %>%
  ungroup() %>%
  select(site, drought_start_year, drought_end_year) %>%
  distinct()

str(edge_rare)

edge_rare$year <- as.character(edge_rare$year)
edge_rare$year <- as.Date(edge_rare$year, format = "%Y")

# Figure 2 ####
dom <- ggplot(edge_rare[edge_rare$rarity == "dominant species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_grid(site~rarity, scales = "free") +
  scale_color_manual(values = c("#175149")) +
  xlab("Year") +  ylab("Mean Cover") +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom")

sub <- ggplot(edge_rare[edge_rare$rarity == "subordinate species",], aes(x=as.integer(year2), y=mean.cover)) +
  geom_rect(aes(xmin = drought_start_year,xmax = drought_end_year,ymin = -Inf, ymax = Inf),
            fill="#E6E6E6", alpha = .2) +
  geom_line(aes(linetype = treatment, color = rarity), linewidth= 1.25) +
  geom_point(aes(color = rarity), size = 2) +
  facet_grid(site~rarity, scales = "free") +
  scale_color_manual(values = c( "#BDC881")) +
  xlab("Year") +  ylab(NULL) +
  labs(color = NULL, linetype = "Treatment", ) +
  coord_cartesian(xlim = c(2012, 2021)) +
  theme(text = element_text(size = 14.5)) +
  geom_errorbar(aes(ymin = mean.cover - se.cover, ymax = mean.cover + se.cover, color = rarity), width = 0.25, linewidth = 0.8) +
  theme(legend.position="bottom") +
  guides(linetype = "none")

ggarrange(dom, sub)

ggsave("preliminary_figs/june_2024/figure2.png", width = 8, height = 8)

