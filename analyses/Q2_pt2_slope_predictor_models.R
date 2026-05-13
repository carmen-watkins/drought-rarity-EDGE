# Header ####
## Script name: Q2 pt 2 Slope Predictor Models

## Purpose of script: Run models to test whether site level predictors 
## (precip, temp, dominance) can predict SLOPES of rarity - response ratio 
## relationship
##
## Author: Carmen Watkins
##

# Set up ####
## load packages
library(cowplot)

## read in data
source("analyses/Q2_pt1_linear_models_by_site.R")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

## prep data ####
## join site predictors with model outputs.
## for spatial rarity models
spmods_pred = left_join(sp_mods, site_pred_scaled, by = c("site"))

## for temporal rarity models
tmpmods_pred = left_join(tmp_mods, site_pred_scaled, by = c("site"))

# Mk Site Table ####
## make site table as basis of Table S1
head(site_pred_scaled)
site_tab = site_pred_scaled %>%
  select(site, grassland.type, MAP.mm, MAT.C, aridity, BP.dom.site)
#write.csv(site_tab, "tables/review_tabs/site_info_TabS1.csv", row.names = F)

# Models ####
## Spatial, Drought ####
### precipitation ####
spm1 = lm(estimate ~ z_precip, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Drought",])
summary(spm1)
confint(spm1)

## save model outputs as dataframe
spm1_df = as.data.frame(summary(spm1)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Precip")

### temperature ####
spm2 = lm(estimate ~ z_temp, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Drought",])
summary(spm2)
confint(spm2)

## save model outputs as dataframe
spm2_df = as.data.frame(summary(spm2)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Temp")

### dominance ####
spm3 = lm(estimate ~ dom.rounded, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Drought",])
summary(spm3)
confint(spm3)
## save model outputs as dataframe
spm3_df = as.data.frame(summary(spm3)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Dominance")

### aridity ####
spm4 = lm(estimate ~ aridity, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Drought",])

summary(spm4)

spm4_df = as.data.frame(summary(spm4)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Aridity")

### soil capacity ####
spmsfc = lm(estimate ~ z_soil, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Drought",])

summary(spmsfc)
confint(spmsfc)

spmsfc_df = as.data.frame(summary(spmsfc)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Soil Field Capacity")

## Temporal, Drought ####
### precipitation ####
tpm1 = lm(estimate ~ z_precip, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" &
                                tmpmods_pred$period == "Drought",])
summary(tpm1)
confint(tpm1)

## save model outputs as dataframe
tpm1_df = as.data.frame(summary(tpm1)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Precip")

### temperature ####
tpm2 = lm(estimate ~ z_temp, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" &
                                tmpmods_pred$period == "Drought",])
summary(tpm2)
confint(tpm2)

## save model outputs as dataframe
tpm2_df = as.data.frame(summary(tpm2)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Temp")

### dominance ####
tpm3 = lm(estimate ~ dom.rounded, 
          tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                         tmpmods_pred$period == "Drought",])
summary(tpm3)
confint(tpm3)

## save model outputs as dataframe
tpm3_df = as.data.frame(summary(tpm3)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Dominance")

### aridity ####
tpm4 = lm(estimate ~ aridity, 
          tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                         tmpmods_pred$period == "Drought",])
summary(tpm4)

## save model outputs as dataframe
tpm4_df = as.data.frame(summary(tpm4)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Aridity")

### soil capacity ####
tpmsfc = lm(estimate ~ z_soil, 
          tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                         tmpmods_pred$period == "Drought",])
summary(tpmsfc)

## save model outputs as dataframe
tpmsfc_df = as.data.frame(summary(tpmsfc)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Soil Field Capacity")


## Combine ####
slope_pred_tab = rbind(spm1_df, spm2_df, spm3_df, spm4_df, spmsfc_df, tpm1_df, 
                       tpm2_df,  tpm3_df, tpm4_df, tpmsfc_df) %>%
  rownames_to_column(var = "type") %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  select(period, rarity, predictor, type, Estimate, `Std. Error`, 
         `t value`, `Pr(>|t|)`)

## write.csv(slope_pred_tab, "tables/review_tabs/slope_predictor_mod_TabS14.csv", 
   ##       row.names = F)

## Spatial, Post-Drought ####
### precipitation ####
spm5 = lm(estimate ~ z_precip, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Post-Drought",])
summary(spm5)

## save model outputs as dataframe
spm5_df = as.data.frame(summary(spm5)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Precipitation")

### temperature ####
spm6 = lm(estimate ~ z_temp, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Post-Drought",])
summary(spm6)

## save model outputs as dataframe
spm6_df = as.data.frame(summary(spm6)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Temperature")

### dominance ####
spm7 = lm(estimate ~ dom.rounded, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Post-Drought",])
summary(spm7)

## save model outputs as dataframe
spm7_df = as.data.frame(summary(spm7)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Dominance")

### aridity ####
spm8 = lm(estimate ~ aridity, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Post-Drought",])
summary(spm8)

## save model outputs as dataframe
spm8_df = as.data.frame(summary(spm8)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Aridity")

### soil capacity #### 
spm9 = lm(estimate ~ z_soil, 
          data = spmods_pred[spmods_pred$term == "spatial_rarity" & 
                               spmods_pred$period == "Post-Drought",])
summary(spm9)
confint(spm9)

## save model outputs as dataframe
spm9_df = as.data.frame(summary(spm9)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Soil Field Capacity")


## Temporal, Post-Drought ####
## precipitation
tpm5 = lm(estimate ~ z_precip, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                                tmpmods_pred$period == "Post-Drought",])
summary(tpm5)

## save model outputs as dataframe
tpm5_df = as.data.frame(summary(tpm5)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Precipitation")

## temperature
tpm6 = lm(estimate ~ z_temp, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                                tmpmods_pred$period == "Post-Drought",])
summary(tpm6)

## save model outputs as dataframe
tpm6_df = as.data.frame(summary(tpm6)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Temperature")

## dominance
tpm7 = lm(estimate ~ dom.rounded, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                                tmpmods_pred$period == "Post-Drought",])
summary(tpm7)

## save model outputs as dataframe
tpm7_df = as.data.frame(summary(tpm7)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Dominance")

## aridity
tpm8 = lm(estimate ~ aridity, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                                tmpmods_pred$period == "Post-Drought",])
summary(tpm8)

## save model outputs as dataframe
tpm8_df = as.data.frame(summary(tpm8)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Aridity")

### soil capacity ####
tpm9 = lm(estimate ~ z_soil, 
          data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & 
                                tmpmods_pred$period == "Post-Drought",])
summary(tpm9)
confint(tpm9)
## save model outputs as dataframe
tpm9_df = as.data.frame(summary(tpm9)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Soil Field Capacity")

## Combine ####
slope_pred_tab_PD = rbind(spm5_df, spm6_df, spm7_df, spm8_df, spm9_df, tpm5_df, 
                          tpm6_df, tpm7_df, tpm8_df, tpm9_df) %>%
  rownames_to_column(var = "type") %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  select(period, rarity, predictor, type, Estimate, `Std. Error`, 
         `t value`, `Pr(>|t|)`)

all_slope_pred = rbind(slope_pred_tab, slope_pred_tab_PD)

#write.csv(all_slope_pred, "tables/final_tables/slope_predictor_mod_TabS9.csv", 
 #         row.names = F)