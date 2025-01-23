

chyDd = lm(resp.ratio.site_D4 ~ spatial_rarity*Duration, data = edge_RR2[edge_RR2$site == "CHY",])
summary(chyDd)



ms = lmer(resp.ratio.site_D4 ~ spatial_rarity*Duration + (1|site), data = edge_RR2[!edge_RR2$Duration %in% c("unk", "annual/perennial"),])
summary(ms)
check_model(ms)

Anova(ms, type = 3, test = "F")

mt = lmer(resp.ratio.site_D4 ~ temporal_rarity*Duration + (1|site), data = edge_RR2[!edge_RR2$Duration %in% c("unk", "annual/perennial"),])
summary(mt)
check_model(mt)

Anova(mt, type = 3, test = "F")

msp = lmer(resp.ratio.site_PDfull ~ spatial_rarity*Duration + (1|site), data = edge_RR2[!edge_RR2$Duration %in% c("unk", "annual/perennial"),])
summary(msp)
check_model(msp)

Anova(msp)

mtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity*Duration + (1|site), data = edge_RR2[!edge_RR2$Duration %in% c("unk", "annual/perennial"),])
summary(mtp)
check_model(mtp)

Anova(mtp)

