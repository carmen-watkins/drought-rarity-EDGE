# Set up ####
## read in data ####
source("data-prep/clean_cover_dat_fill_zeros.R")
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

names(edge_all)

edge_all$site = as.factor(edge_all$site)

cover_sum = edge_all %>%
  group_by(year, site, treatment, treatment.year, experiment.year, block, plot, 
           subplot) %>%
  summarise(mean.sub.cov = sum(max.cover)) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))
  
ggplot(cover_sum, aes(x=experiment.year, y=mean.sub.cov, color = site)) +
  geom_point() +
  scale_color_manual(values = pal)

cover_sum2 = cover_sum %>%
  group_by(year, site, treatment, treatment.year, experiment.year) %>%
  summarise(site.mean = mean(mean.sub.cov),
            site.se = calcSE(mean.sub.cov)) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"),
         drought_start_year = ifelse(site %in% c("SBL", "SBK"), 2013, 2014),
         drought_end_year = ifelse(site %in% c("SBL", "SBK"), 2019, 2017)) %>%
  mutate(fakeM = 10,
         fakeD = 25,
         fulldate = paste0(year, fakeM, fakeD),
         fulldate2 = ymd(fulldate),
         year2 = year(fulldate2)) %>%
  mutate(treatment = ifelse(treatment == "C", "Ambient", "Drought"))

## create line data for graphing
#line_data <- cover_sum2 %>%
 # ungroup() %>%
  #select(site, drought_start_year, drought_end_year) %>%
  #distinct()

ggplot(cover_sum2, aes(x=year2, y=site.mean)) +
  
  geom_rect(aes(xmin = drought_start_year, xmax = drought_end_year, ymin = -Inf, 
                ymax = Inf), fill="#E6E6E6", alpha = .2) +
  
  geom_point(size = 1.5, aes(color = treatment)) +
  geom_errorbar(aes(ymin = site.mean - site.se, ymax = site.mean + site.se, 
                    color = treatment), width = 0.25) +
  geom_line(aes(color = treatment)) +
  facet_wrap(~site, ncol = 2, nrow = 3, scales = "free") +
  scale_shape_manual(values = c(15, 16)) + 
  scale_color_manual(values = c(pal[1], pal[4])) +
  xlab("Year") +
  ylab("Mean Total SubPlot Cover") +
  labs(color = NULL)
  
ggsave("figures/Jan2025/mean_sub_cover.png", width = 8, height = 6)
