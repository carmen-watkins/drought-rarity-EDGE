# Header #### 
## Script name: Prep model predictors
##
## Purpose of script: Prepare site level predictor data for use in models
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned cover data
source("data-prep/classify_rank_persistence.R")

## load precip data
source("data-prep/clean_ppt_data.R")

## load site MAP & MAT data
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

# Merge ####
site_pred = left_join(MAP, BP_dominance, by = "site")
site_pred_final = left_join(site_ppt, site_pred, by = "site")

# Scale Variables ####
site_pred_scaled = site_pred_final %>%
  mutate(mean_temp = mean(MAT.C),
         sd_temp = sd(MAT.C),
         mean_ppt_across = mean(mean_ppt),
         sd_ppt = sd(mean_ppt),
         z_precip = (mean_ppt - mean_ppt_across)/sd_ppt,
         z_temp = (MAT.C - mean_temp)/sd_temp, 
         dom.rounded = round(BP.dom.site, digits = 3),
         
         ## scale ppt intervals
         meanDRR4 = mean(pptDRR4),
         sdDRR4 = sd(pptDRR4),
         meanPDRRfull = mean(pptPDRRfull),
         sdPDRRfull = sd(pptPDRRfull),
         meanPDRRfinal = mean(pptPDRRfinal),
         sdPDRRfinal = sd(pptPDRRfinal),
         meanPDRRfirst = mean(pptPDRRfirst),
         sdPDRRfirst = sd(pptPDRRfirst),
         
         z_precipDRR4 = (pptDRR4 - meanDRR4)/sdDRR4,
         z_precipPDRRfull = (pptPDRRfull - meanPDRRfull)/sdPDRRfull,
         z_precipPDRRfinal = (pptPDRRfinal - meanPDRRfinal)/sdPDRRfinal,
         z_precipPDRRfirst = (pptPDRRfirst - meanPDRRfirst)/sdPDRRfirst
         
         )

# Clean Env ####
rm(site_pred, site_pred_final, BP_dominance, controls, MAP, site_ppt, dom, ppt_temp, edge_all, rank_persist)
