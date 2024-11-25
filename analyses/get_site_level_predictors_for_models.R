# Header #### 
## Script name: Site Level Predictors
##
## Purpose of script: Classify each species at each site by its rank and persistence at the site using data from control plots in the EDGE experiment.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned cover data
source("data-prep/cleaning_fill_zeros_at_subplot_edge.R") 

source("data-prep/clean_ppt_data.R")

MAP = read.csv("data/map_data.csv") %>%
  mutate(site = ifelse(site == "SEV_blue", "SBL", 
                       ifelse(site == "SEV_black", "SBK", site)))

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## filter data to include control plots only
## use edge data with zeros for accurate calculations
controls <- edge_all %>%
  filter(treatment == "C")

# Calc Berger-Parker Dominance ####
## the relative abundance of the most abundant species in the plot
BP_dominance = controls %>%
  group_by(year, site, block, plot, species) %>%
  summarise(mean.cov.plot = mean(max.cover)) %>%
  group_by(year, site, plot) %>%
  summarise(tot.cov = sum(mean.cov.plot), 
            dom.cov = max(mean.cov.plot),
            BP.dom = dom.cov/tot.cov) %>%
  group_by(site) %>%
  summarise(BP.dom.site = mean(BP.dom))


BP_dominance$site = factor(BP_dominance$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

ggplot(BP_dominance, aes(x=site, y=BP.dom.site, color = site)) +
  geom_point(size = 4) +
  scale_color_manual(values = pal)

MAP$site = factor(MAP$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

ggplot(MAP, aes(x=site, y=MAP.mm, color = site)) +
  geom_point(size = 4) +
  scale_color_manual(values = pal)

ggplot(MAP, aes(x=site, y=MAT.C, color = site)) +
  geom_point(size = 4) +
  scale_color_manual(values = pal)

site_pred = left_join(MAP, BP_dominance, by = "site")
site_pred_final = left_join(site_ppt, site_pred, by = "site")


site_pred_final$site = factor(site_pred_final$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

ggplot(site_pred_final, aes(x=site, y=mean_ppt, color = site)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_ppt - se_ppt, ymax = mean_ppt + se_ppt)) +
  scale_color_manual(values = pal)


## cov of dominant / total cov

## should this be done at plot level?
## should it be done regardless of species?