
# Set up ####
## load packages
library(tidyverse)
library(ggpubr)
library(cowplot)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in precip data
precip = read.csv("data/growingseason_precip_totals_allyears.csv")
sev_ppt = read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")

source("data-prep/clean_spei_data.R")
#source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Clean data ####
sev_temporal = sev_ppt %>%
  select(site, year, season, season.precip) %>%
  distinct() %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL")) %>%
  filter(season == "fall") %>%
  select(-season) 

names(sev_temporal) = c("site", "year", "tot.precip")

north_temporal <- precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip))
  
names(north_temporal) = c("site", "year", "tot.precip")

ppt_all = rbind(sev_temporal, north_temporal) %>%
  mutate(site = as.factor(site), 
         site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Plot ####
ppt_north = ppt_all %>%
  filter(!site %in% c("SBL", "SBK")) %>%
  ggplot(aes(x = year, y = tot.precip)) +
  
  geom_rect(aes(xmin = 2014, xmax = 2017, ymin = -Inf, 
                ymax = Inf), fill="#E6E6E6", alpha = .2) +
  
  geom_point(aes(color = site), size = 2) +
  geom_line(aes(color = site)) +
  xlab("Year") +
  ylab("Precip (mm)") +
  labs(color = "Site") +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 1000))

ppt_sev = ppt_all %>%
  filter(site %in% c("SBL", "SBK"),
         year != 2012) %>%
  ggplot(aes(x = year, y = tot.precip)) +
  
  geom_rect(aes(xmin = 2013, xmax = 2019, ymin = -Inf, 
                ymax = Inf), fill="#E6E6E6", alpha = .2) +
  
  geom_point(aes(color = site), size = 2) +
  geom_line(aes(color = site)) +
  xlab("Year") +
  ylab("Precip (mm)") +
  labs(color = "Site") +
  scale_color_manual(values = c(pal[5], pal[6])) +
  theme(text = element_text(size = 12))  +
  coord_cartesian(ylim = c(0, 1000)) +
  scale_x_continuous(breaks = seq(2013, 2023, 3))

spei_north = spei_exp %>%
  filter(site != "SEV") %>%
ggplot(aes(x = year, y = spei)) +
  
  geom_rect(aes(xmin = 2014, xmax = 2017, ymin = -Inf, 
                ymax = Inf), fill="#E6E6E6", alpha = .2) +
  
  geom_point(aes(color = site), size = 2) +
  geom_line(aes(color = site)) +
  xlab("Year") +
  ylab("SPEI") +
  labs(color = "Site") +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-2.5, 2)) #+
  #scale_x_continuous(breaks = seq(2013, 2023, 2))

spei_sev = spei_exp %>%
  filter(site == "SEV") %>%
  ggplot(aes(x = year, y = spei)) +
  
  geom_rect(aes(xmin = 2013, xmax = 2019, ymin = -Inf, 
                ymax = Inf), fill="#E6E6E6", alpha = .2) +
  
  geom_point(aes(color = site), size = 2) +
  geom_line(aes(color = site)) +
  xlab("Year") +
  ylab("SPEI") +
  labs(color = "Site") +
  scale_color_manual(values = pal[6]) +
  theme(text = element_text(size = 12)) +
  coord_cartesian(ylim = c(-2.5, 2)) +
  scale_x_continuous(breaks = seq(2013, 2023, 3))

## explore distrib of SPEI over all years
spei_hist = spei_all %>%
  ungroup() %>%
  ggplot(aes(x=spei)) +
  geom_density() +
  facet_wrap(~site, ncol = 5, nrow = 1) +
  geom_jitter(data = spei_exp, aes(x=spei, y=0.1, color = exp.year, 
                                   fill = exp.year),
              height = 0.01, width = 0, alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c( "#D69C4E", "#435163", "white")) +
  scale_fill_manual(values = c( "#D69C4E", "#435163")) +
  xlab("Growing Seasion SPEI") +
  ylab("Frequency") +
  labs(color = "Experiment Year", fill = "Experiment Year") +
  guides(fill = "none") +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 12))

p1 = plot_grid(ppt_north, ppt_sev, spei_north, spei_sev, ncol = 2, 
               labels = c("(a)", "(b)", "(c)", "(d)"), align = "v", label_x = -0.03)

p2 = plot_grid(spei_hist, labels = c("(e)"))

plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 0.5))

ggsave("figures/review_figs/precip_year_FigSX.tiff",
       width = 18, height = 15, units = "cm")  

