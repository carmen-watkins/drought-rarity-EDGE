## Figures

library(wesanderson)
library(ggpubr)
wes_palettes

# Drought ####
## Rarity-Dom Int ####
## spatial rarity plot
sp = visreg(md4s,"spatial_rarity", by="dom.rounded",
           overlay = TRUE, partial = FALSE, rug = FALSE,
           plot=FALSE)

spp = ggplot(p$fit, aes(spatial_rarity, visregFit, linetype=factor(dom.rounded), fill=factor(dom.rounded))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.25, 
              colour="grey50", linetype=1, size=0.2) +
  geom_line(linewidth = 2, aes(color = factor(dom.rounded)), linetype = "solid") +
  scale_color_manual(values = c("#e597b9", "#ad5fad", "#573b88")) +
  scale_fill_manual(values = c("#e597b9", "#ad5fad", "#573b88")) +
  labs(linetype="Site Dominance", fill="Site Dominance", color = "Site Dominance") +
  xlab("Spatial Rarity") +
  ylab("Fit of Drought Response Ratio")

## temporal rarity plot
tp = visreg(md4t,"temporal_rarity", by="BP.dom.site",
            overlay = TRUE, partial = FALSE, rug = FALSE,
            plot=FALSE)

tpp = ggplot(tp$fit, aes(temporal_rarity, visregFit, linetype=factor(BP.dom.site), fill=factor(BP.dom.site))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.25, 
              colour="grey50", linetype=1, size=0.2) +
  geom_line(linewidth = 2, aes(color = factor(BP.dom.site)), linetype = "solid") +
  scale_color_manual(values = c("#e597b9", "#ad5fad", "#573b88")) +
  scale_fill_manual(values = c("#e597b9", "#ad5fad", "#573b88")) +
  labs(linetype="Site Dominance", fill="Site Dominance", color = "Site Dominance") +
  xlab("Temporal Rarity") +
  ylab(" ")

ggarrange(spp, tpp, common.legend = TRUE, legend = "bottom")
ggsave("figures/Dec2024/drr-raritydom-int.png", width = 8, height = 3.75)

## Rarity-Ppt Int ####
sp2 = visreg(md4s,"spatial_rarity", by="z_precip",
           overlay = TRUE, partial = FALSE, rug = FALSE,
           plot=FALSE)

spp2 = ggplot(sp2$fit, aes(spatial_rarity, visregFit, linetype=factor(z_precip), fill=factor(z_precip))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.25, 
              colour="grey50", linetype=1, size=0.2) +
  geom_line(linewidth = 2, aes(color = factor(z_precip)), linetype = "solid") +
  scale_color_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(linetype="Site Precipitation", fill="Site Precipitation", color = "Site Precipitation") +
  xlab("Spatial Rarity") +
  ylab("Fit of Drought Response Ratio")

tp2 = visreg(md4t,"temporal_rarity", by="z_precip",
             overlay = TRUE, partial = FALSE, rug = FALSE,
             plot=FALSE)

tpp2 = ggplot(tp2$fit, aes(temporal_rarity, visregFit, linetype=factor(z_precip), fill=factor(z_precip))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.25, 
              colour="grey50", linetype=1, size=0.2) +
  geom_line(linewidth = 2, aes(color = factor(z_precip)), linetype = "solid") +
  scale_color_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(linetype="Site Precipitation", fill="Site Precipitation", color = "Site Precipitation") +
  xlab("Temporal Rarity") +
  ylab(" ")

ggarrange(spp2, tpp2, common.legend = TRUE, legend = "bottom")
ggsave("figures/Dec2024/drr-rarityppt-int.png", width = 8, height = 3.75)

