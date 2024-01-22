# Header #### 
## Script name: Response Ratio Yearly
##
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots to rank & persistence yearly & by SPEI
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up Env ####
library(ggpubr)

## read in response ratio calcs
source("analyses/calculate_response_ratio.R")
source("data-prep/clean_ppt_data.R")
theme_set(theme_classic())

# Clean Data ####
edge_yearly_w_years <- left_join(edge_yearly_w_predictors, year.key, by = c("site", "experiment.year", "treatment.year"))

edge_yearly_w_precip <- left_join(edge_yearly_w_years, growing.season.tot, by = c("site", "year"))

# Explore ####
colnames(edge_yearly_w_precip)

## overall resp ratio vs. precip
ggplot(edge_yearly_w_precip, aes(x=tot.precip, y=resp.ratio.site, color = percrank)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm")

# Precip Fig ####
knz <- ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "KNZ" & !is.na(edge_yearly_w_precip$tot.precip),], aes(x=percrank, y=resp.ratio.site, color = as.factor(tot.precip))) +
  geom_point() +
  #facet_wrap(~as.factor(tot.precip)) +
  geom_smooth(method = "lm", alpha = 0.01) +
  scale_color_manual(values = c("#C75DAB","#D691C1","#E4C1D9","#F1F1F1","#A7D3D4", "#009B9E","#42B7B9","#009392")) +
  ggtitle("KNZ") +
  geom_hline(yintercept = 0, linetype = "dashed")

hys <- ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "HYS" & !is.na(edge_yearly_w_precip$tot.precip),], aes(x=percrank, y=resp.ratio.site, color = as.factor(tot.precip))) +
  geom_point() +
  #facet_wrap(~as.factor(tot.precip)) +
  geom_smooth(method = "lm", alpha = 0.01) +
  scale_color_manual(values = c("#C75DAB","#D691C1","#E4C1D9","#F1F1F1","#A7D3D4", "#009B9E","#42B7B9","#009392")) +
  ggtitle("HYS") +
  geom_hline(yintercept = 0, linetype = "dashed")

sgs <- ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "SGS" & !is.na(edge_yearly_w_precip$tot.precip),], aes(x=percrank, y=resp.ratio.site, color = as.factor(tot.precip))) +
  geom_point() +
  #facet_wrap(~as.factor(tot.precip)) +
  geom_smooth(method = "lm", alpha = 0.01) +
  scale_color_manual(values = c("#C75DAB","#D691C1","#E4C1D9","#F1F1F1","#A7D3D4", "#009B9E","#42B7B9","#009392")) +
  ggtitle("SGS") +
  geom_hline(yintercept = 0, linetype = "dashed")

chy <- ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "CHY" & !is.na(edge_yearly_w_precip$tot.precip),], aes(x=percrank, y=resp.ratio.site, color = as.factor(tot.precip))) +
  geom_point() +
  #facet_wrap(~as.factor(tot.precip)) +
  geom_smooth(method = "lm", alpha = 0.01) +
  scale_color_manual(values = c("#C75DAB","#D691C1","#E4C1D9","#F1F1F1","#A7D3D4", "#009B9E","#42B7B9","#009392")) +
  ggtitle("CHY") +
  geom_hline(yintercept = 0, linetype = "dashed")


ggarrange(knz, hys, chy, sgs, ncol = 2, nrow = 2, common.legend = TRUE, labels = "AUTO", legend = "right")

ggsave("preliminary_figs/resp.ratio.precip.png", width = 7, height = 5)


na.precip <- edge_yearly_w_precip[is.na(edge_yearly_w_precip$tot.precip),]
#009392,#39b185,#9ccb86,#e9e29c,#eeb479,#e88471,#cf597e
#009B9E,#42B7B9,#A7D3D4,#F1F1F1,#E4C1D9,#D691C1,#C75DAB

# Sp level resp ####
## species level response ratio vs. growing season precipitation
ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "KNZ" & !is.na(edge_yearly_w_precip$resp.ratio.site),], aes(x=tot.precip, y=resp.ratio.site)) +
  geom_point() +
  facet_wrap(~species) +
  geom_smooth(method = "lm")

ggplot(edge_yearly_w_precip[edge_yearly_w_precip$site == "CHY" & !is.na(edge_yearly_w_precip$resp.ratio.site),], aes(x=tot.precip, y=resp.ratio.site)) +
  geom_point() +
  facet_wrap(~species) +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(-1,1))

