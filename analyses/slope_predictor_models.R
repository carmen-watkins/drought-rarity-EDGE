

head(site_pred_scaled)

site_tab = site_pred_scaled %>%
  select(site, grassland.type, MAP.mm, MAT.C, BP.dom.site)

write.csv(site_tab, "tables/site_info.csv")


head(sp_mods)


spm1 = lm(estimate ~ z_precip, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Drought",])
summary(spm1)


spm2 = lm(estimate ~ z_temp, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Drought",])
summary(spm2)

spm3 = lm(estimate ~ dom.rounded, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Drought",])
summary(spm3)


spm4 = lm(estimate ~ z_precip, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Post-Drought",])
summary(spm4)


spm5 = lm(estimate ~ z_temp, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Post-Drought",])
summary(spm5)

spm6 = lm(estimate ~ dom.rounded, data = spmods_pred[spmods_pred$term == "spatial_rarity" & spmods_pred$period == "Post-Drought",])
summary(spm6)



tpm1 = lm(estimate ~ z_precip, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Drought",])
summary(tpm1)


tpm2 = lm(estimate ~ z_temp, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Drought",])
summary(tpm2)

tpm3 = lm(estimate ~ dom.rounded, tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Drought",])
summary(tpm3)

tpm4 = lm(estimate ~ z_precip, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Post-Drought",])
summary(tpm4)


tpm5 = lm(estimate ~ z_temp, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Post-Drought",])
summary(tpm5)

tpm6 = lm(estimate ~ dom.rounded, data = tmpmods_pred[tmpmods_pred$term == "temporal_rarity" & tmpmods_pred$period == "Post-Drought",])
summary(tpm6)

