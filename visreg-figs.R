## Figures

# Drought ####
## Spatial Rarity ####
visreg(md4s, "spatial_rarity")
visreg(md4s_alt, "spatial_rarity")
visreg(md4s, "BP.dom.site")
visreg(md4s_alt, "BP.dom.site")
visreg(md4s, "spatial_rarity", by = "z_precip")
visreg(md4s_alt, "spatial_rarity", by = "z_precip")
visreg(md4s, "spatial_rarity", by = "BP.dom.site")
visreg(md4s_alt, "spatial_rarity", by = "BP.dom.site")

visreg(md4s, "spatial_rarity", type = "conditional", by = "BP.dom.site", breaks = 6, ylab = "Response Ratio", xlab = "Spatial Rarity") 
visreg(md4s_alt, "spatial_rarity", type = "conditional", by = "BP.dom.site", breaks = 6, ylab = "Response Ratio", xlab = "Spatial Rarity") 

## Temporal Rarity ####
visreg(md4t, "temporal_rarity", type = "conditional", by = "BP.dom.site", breaks = 6, ylab = "Response Ratio", xlab = "Temporal Rarity") 
visreg(md4t, "temporal_rarity")
visreg(md4t, "BP.dom.site")
visreg(md4t, "temporal_rarity", by = "z_precip")
visreg(md4t, "temporal_rarity", by = "BP.dom.site")

# Post-Drought ####
## Spatial Rarity ####
visreg(mpds_full, "spatial_rarity")
visreg(mpds_full, "BP.dom.site")
visreg(mpds_full, "spatial_rarity", by = "z_precip")
visreg(mpds_full, "spatial_rarity", by = "BP.dom.site")

visreg(md4s, "spatial_rarity", type = "conditional", by = "BP.dom.site", breaks = 6, ylab = "Response Ratio", xlab = "Spatial Rarity") 

## Temporal Rarity ####
visreg(md4t, "temporal_rarity", type = "conditional", by = "BP.dom.site", breaks = 6, ylab = "Response Ratio", xlab = "Temporal Rarity") 
visreg(md4t, "temporal_rarity")
visreg(md4t, "BP.dom.site")
visreg(md4t, "temporal_rarity", by = "z_precip")
visreg(md4t, "temporal_rarity", by = "BP.dom.site")

