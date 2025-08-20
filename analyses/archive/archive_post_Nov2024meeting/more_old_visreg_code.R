

p2 = visreg(md4s,"spatial_rarity", by="z_precip",
            overlay = TRUE, partial = FALSE, rug = FALSE,
            plot=FALSE)

ggplot(p2$fit, aes(spatial_rarity, visregFit, linetype=factor(z_precip), fill=factor(z_precip))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.25, 
              colour="grey50", linetype=1, size=0.2) +
  geom_line(linewidth = 2, aes(color = factor(z_precip)), linetype = "solid") +
  scale_color_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(linetype="Site Precipitation", fill="Site Precipitation", color = "Site Precipitation") +
  xlab("Spatial Rarity") +
  ylab("Fit Response Ratio")

ggsave("figures/Dec2024/drr-spatial-rarityppt-int.png", width = 5.5, height = 3.5)














#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c
pal[5]


wes_palette("Royal2")[3]
wes_palette("Cavalcanti1")[4]
wes_palette("Cavalcanti1")[5]

wes_palette("Darjeeling2")[2]
wes_palette("Darjeeling2")[3]
wes_palette("Darjeeling2")[4]


#f9ddda,#f2b9c4,#e597b9,#ce78b3,#ad5fad,#834ba0,#573b88


visreg(md4s,"spatial_rarity", by="dom.rounded", type = "conditional", breaks = 3, overlay = TRUE,
       fill = list(col=c("gray", alpha=0.95)),
       line = list(col=c("#9986A5", "#79402E", "#CCBA72")),
       
       points = list(col=c("#9986A5", "#79402E", "#CCBA72"), cex=1)) 



p_md4s_dom = visreg(md4s,"spatial_rarity", by="dom.rounded", type = "conditional", breaks = 3, overlay = TRUE, plot = FALSE)


p_md4s_dom = visreg(md4s,"spatial_rarity", by="dom.rounded", type = "conditional",
                    overlay = TRUE, rug = FALSE, plot = FALSE)

ggplot(p_md4s_dom$fit, aes(spatial_rarity, visregFit, linetype=factor(dom.rounded), fill=factor(dom.rounded))) +
  geom_point()


#+
geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.5, 
            colour="grey50", linetype=1, size=0.2) +
  geom_line() +
  scale_fill_grey(start=0.5, end=0.8) +
  labs(linetype="Site Dominance", fill="Site Dominance")



visreg(md4s, "spatial_rarity", type = "conditional", gg = TRUE) +
  xlab("Spatial Rarity") +
  ylab("Response Ratio")


visreg(md4s, "dom.rounded", gg = TRUE, color = "black") +
  xlab("Site Level Dominance") +
  ylab("Response Ratio")

visreg(md4s, "dom.rounded", by = "spatial_rarity")



ggplot(DRR4, aes(x=BP.dom.site, y=resp.ratio.site_D4))+
  geom_point() +
  geom_smooth(method = "lm")

sitelabs <- c("CHY (0.31)", "HYS (0.38)", "KNZ (0.39)", "SGS (0.42)", "SBL (0.54)", "SBK (0.67)")
names(sitelabs) <- c("dom.rounded: 0.31", "dom.rounded: 0.384", "dom.rounded: 0.387", "dom.rounded: 0.42", "dom.rounded: 0.544", "dom.rounded: 0.67")

visreg(md4s, "spatial_rarity", type = "conditional", by = "dom.rounded", breaks = 6, gg = TRUE)  +
  xlab("Spatial Rarity") +
  ylab("f(Spatial Rarity)") +
  facet_grid(labeller = labeller(dom.rounded = sitelabs))

ggplot(DRR4, aes(x=spatial_rarity, y=resp.ratio.site_D4))+
  geom_point() +
  geom_abline(intercept = 1.7851, slope = -1.6217)







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
## try visreg interaction plot
visreg(md4t, "temporal_rarity", type = "conditional", by = "BP.dom.site", gg = TRUE, breaks = 3, ylab = "Response Ratio", xlab = "Temporal Rarity", line = list(col = "black")) 

visreg(md4t, "temporal_rarity", type = "conditional", by = "z_precip", gg = TRUE, breaks = 6, ylab = "Response Ratio", xlab = "Temporal Rarity") 

## save visreg interaction plot as an object, then plot with ggplot
p = visreg(md4t,"temporal_rarity", by="BP.dom.site", type = "conditional",
           overlay = TRUE, rug = FALSE, plot = FALSE)

ggplot(p$fit, aes(temporal_rarity, visregFit, linetype=factor(BP.dom.site), fill=factor(BP.dom.site))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr), alpha=0.5, 
              colour="grey50", linetype=1, size=0.2) +
  geom_line() +
  scale_fill_grey(start=0.5, end=0.8) +
  labs(linetype="Site Dominance", fill="Site Dominance")

## try the same for ppt
pp = visreg(md4t,"temporal_rarity", by="z_precip", type = "conditional",
            overlay = TRUE, rug = FALSE, plot = FALSE)

ggplot(pp$fit, aes(temporal_rarity, visregFit, linetype=factor(z_precip), fill=factor(z_precip))) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr, color = factor(z_precip)), alpha=0.25, 
              linetype=1, size=0.2) +
  geom_line(linewidth = 1, aes(color = factor(z_precip))) +
  scale_fill_manual(values = c(pal[6], pal[3], pal[1])) +
  scale_color_manual(values = c(pal[6], pal[3], pal[1])) +
  labs(linetype="Site Precip", fill="Site Precip")



visreg(md4t,"temporal_rarity", by="z_precip",
       overlay = TRUE, rug = FALSE,
       fill=list(col=grey(c(0.2,0.5,0.8), alpha=0.4)),
       #col=list(col=grey(c(0.2,0.5,0.8))),
       line=list(lty=1:3, col="black"))



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
