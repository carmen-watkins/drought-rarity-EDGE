
## quick models to test whether site level predictors can predict sloeps of rarity - response ratio relationship

## make site table
head(site_pred_scaled)

site_tab = site_pred_scaled %>%
  select(site, grassland.type, MAP.mm, MAT.C, BP.dom.site)

#write.csv(site_tab, "tables/site_info.csv")


head(sp_mods)

# Models ####
## Spatial, Drought ####
spm1 = lm(estimate ~ z_precip, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Drought",])
summary(spm1)

spm1_df = as.data.frame(summary(spm1)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Precip")

spm2 = lm(estimate ~ z_temp, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Drought",])
summary(spm2)

spm2_df = as.data.frame(summary(spm2)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Temp")

spm3 = lm(estimate ~ dom.rounded, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Drought",])
summary(spm3)

spm3_df = as.data.frame(summary(spm3)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Drought", 
         predictor = "Dominance")

## Temporal, Drought ####
tpm1 = lm(estimate ~ z_precip, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Drought",])
summary(tpm1)

tpm1_df = as.data.frame(summary(tpm1)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Precip")

tpm2 = lm(estimate ~ z_temp, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Drought",])
summary(tpm2)

tpm2_df = as.data.frame(summary(tpm2)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Temp")

tpm3 = lm(estimate ~ dom.rounded, tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Drought",])
summary(tpm3)

tpm3_df = as.data.frame(summary(tpm3)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Drought", 
         predictor = "Dominance")

## Combine ####
slope_pred_tab = rbind(spm1_df, spm2_df, spm3_df, tpm1_df, tpm2_df, tpm3_df) %>%
  mutate(signif = ifelse(`Pr(>|t|)` < 0.001, "***", 
                                                                                                    ifelse(`Pr(>|t|)` < 0.01 & `Pr(>|t|)` > 0.001, "**",
                                                                                                           ifelse(`Pr(>|t|)` > 0.01 & `Pr(>|t|)` < 0.05, "*", 
                                                                                                                  ifelse(`Pr(>|t|)` < 0.1 & `Pr(>|t|)` > 0.05, ".", " "))))) %>%
  rownames_to_column(var = "type") %>%
  select(predictor, period, rarity, type, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, signif)

#write.csv(slope_pred_tab, "tables/slope_predictor_mod_table.csv", row.names = F)


## Spatial, Post-Drought ####
spm4 = lm(estimate ~ z_precip, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Post-Drought",])
summary(spm4)

spm4_df = as.data.frame(summary(spm4)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Precipitation")

spm5 = lm(estimate ~ z_temp, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Post-Drought",])
summary(spm5)

spm5_df = as.data.frame(summary(spm5)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Temperature")

spm6 = lm(estimate ~ dom.rounded, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Post-Drought",])
summary(spm6)

spm6_df = as.data.frame(summary(spm6)$coeff) %>%
  mutate(rarity = "Spatial", 
         period = "Post-Drought", 
         predictor = "Dominance")


## Temporal, Post-Drought ####
tpm4 = lm(estimate ~ z_precip, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Post-Drought",])
summary(tpm4)

tpm4_df = as.data.frame(summary(tpm4)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Precipitation")

tpm5 = lm(estimate ~ z_temp, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Post-Drought",])
summary(tpm5)

tpm5_df = as.data.frame(summary(tpm5)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Temperature")

tpm6 = lm(estimate ~ dom.rounded, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Post-Drought",])
summary(tpm6)

tpm6_df = as.data.frame(summary(tpm6)$coeff) %>%
  mutate(rarity = "Temporal", 
         period = "Post-Drought", 
         predictor = "Dominance")

## Combine ####
slope_pred_tab_PD = rbind(spm4_df, spm5_df, spm6_df, tpm4_df, tpm5_df, tpm6_df) %>%
  mutate(signif = ifelse(`Pr(>|t|)` < 0.001, "***", 
                         ifelse(`Pr(>|t|)` < 0.01 & `Pr(>|t|)` > 0.001, "**",
                                ifelse(`Pr(>|t|)` > 0.01 & `Pr(>|t|)` < 0.05, "*", 
                                       ifelse(`Pr(>|t|)` < 0.1 & `Pr(>|t|)` > 0.05, ".", " "))))) %>%
  rownames_to_column(var = "type") %>%
  select(predictor, period, rarity, type, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, signif)

write.csv(slope_pred_tab_PD, "tables/slope_predictor_mod_table_PD.csv", row.names = F)


