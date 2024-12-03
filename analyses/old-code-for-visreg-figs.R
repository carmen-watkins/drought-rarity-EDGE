visreg(m1_rd, "percrank", type = "conditional", by = "BP.dom.site", breaks = 6, ylab = "f(Spatial Rarity)", xlab = "Spatial Rarity") 


sitelabs <- c("CHY (0.31)", "HYS (0.38)", "KNZ (0.39)", "SGS (0.42)", "SBL (0.54)", "SBK (0.67)")
names(supp.labs) <- c("OJ", "VC")

visreg(m1_rd, "percrank", type = "conditional", by = "BP.dom.site", breaks = 6, gg = TRUE)  +
  xlab("Spatial Rarity") +
  ylab("f(Spatial Rarity)")

ggsave("figures/Nov2024_postmeeting/rarity_by_site_dom.png", width = 9, height = 3)


visreg(m1_rd, "percrank", type = "conditional", by = "z_precip", breaks = 6)

ggsave("figures/Nov2024_postmeeting/rarity_by_precip.png", width = 9, height = 3)

visreg(m1_rd, "percrank", type = "conditional")
ggsave("figures/Nov2024_postmeeting/rarity_main_effect.png", width = 9, height = 3)

visreg(m1_rd, "percrank", by = "z_precip")

sort(unique(DRR$BP.dom.site))


### explore interactions ####
## temp & precip interaction
visreg2d(m1_rd, "z_temp", "z_precip", plot.type = "image")

## what is the colored scale in this type of plot?

## explore interaction between rank and precip
visreg2d(m1_rd, "z_precip", "percrank", plot.type = "image")
#visreg2d(m1_rd, "z_precip", "percrank", plot.type = "rgl")

## explore interaction between rank and site dominance
visreg2d(m1_rd, "percrank", "BP.dom.site", plot.type = "image")

















## post-drought ####
m1_rpd = lmer(resp.ratio.site_PD ~ percrank + z_temp*z_precip + BP.dom.site + percrank:z_precip + percrank:BP.dom.site + (percrank|site), data = PDRR)

summary(m1_rpd)
anova(m1_rpd)

### explore interactions ####
visreg2d(m1_rpd, "z_temp", "z_precip", plot.type = "image")

# Persistence 
## drought ####
m1_pd = lmer(resp.ratio.site_D ~ persistence.site + z_temp*z_precip + BP.dom.site + persistence.site:z_precip + persistence.site:BP.dom.site + (persistence.site|site), data = DRR)

summary(m1_pd)
anova(m1_pd)

### explore interactions ####
## temp & precip interaction
visreg2d(m1_pd, "z_temp", "z_precip", plot.type = "image")

## what is the colored scale in this type of plot?

## explore interaction between rank and precip
visreg2d(m1_pd, "z_precip", "persistence.site", plot.type = "image")

## explore interaction between rank and site dominance
visreg2d(m1_pd, "persistence.site", "BP.dom.site", plot.type = "image")


## post-drought ####
m1_ppd = lmer(resp.ratio.site_PD ~ persistence.site + z_temp*z_precip + BP.dom.site + persistence.site:z_precip + persistence.site:BP.dom.site + (persistence.site|site), data = PDRR)

summary(m1_ppd)
anova(m1_ppd)

### explore interactions ####
## temp & precip interaction
visreg2d(m1_ppd, "z_temp", "z_precip", plot.type = "image")

## what is the colored scale in this type of plot?

## explore interaction between rank and precip
visreg2d(m1_ppd, "z_precip", "persistence.site", plot.type = "image")









## old attempted visualizations
summary(m1_pd)
summary(m1_rd)

summary(m1_ppd)
summary(m1_rpd)

DRR %>%
  group_by(site, z_temp, z_precip) %>%
  summarise(meanDRR = mean(resp.ratio.site_D)) %>%
  ggplot(aes(x=z_temp, y=z_precip, fill = meanDRR)) +
  geom_tile() +
  scale_fill_viridis() +
  geom_label(aes(label = site))

ggplot(DRR, aes(x=z_precip, y=resp.ratio.site_D, color = z_temp)) +
  geom_point() +
  geom_label(aes(label = site), y=-1) +
  geom_point(data = DRR %>%
               group_by(site, z_precip) %>%
               summarise(meanDRR = mean(resp.ratio.site_D)), aes(x=z_precip, y=meanDRR), size = 4, color = "black")


ggplot(DRR, aes(x=as.factor(signif(z_temp, digits = 3)), y=resp.ratio.site_D)) +
  geom_violin() +
  geom_jitter() +
  geom_boxplot(width = 0.2) 

sort(unique(DRR$MAT.C))
sort(unique(DRR$z_temp))



ggplot(DRR, aes(x=percrank, y=resp.ratio.site_D)) +
  geom_point() +
  facet_wrap(~(mean_ppt), nrow = 1, ncol = 6) +
  scale_x_reverse()


## get output from model
m1_rd

ranef(m1_rd)


?seq()

## param space for 

sort(unique(DRR$z_precip))
sort(unique(DRR$z_temp))


## explore interaction between precip and temp
#dat = expand.grid(seq(from = -1.584, to = 0.781, by = 0.1), seq(from = -0.877, to = 1.478, by = 0.1))

#out = predict(m1_rd, dat)

#ggplot(dat, aes(x = , y = , )) 
#filled.contour()

visreg2d(m1_rd, "z_temp", "z_precip", plot.type = "image")

## explore interaction between rank and precip
visreg2d(m1_rd, "z_precip", "percrank", plot.type = "image")
visreg2d(m1_rd, "z_precip", "percrank", plot.type = "rgl")

## explore interaction between rank and site dominance
visreg2d(m1_rd, "z_precip", "percrank", plot.type = "image")
visreg2d(m1_rd, "z_precip", "percrank", plot.type = "rgl")
