# Modeling 11/14/2024

## updated model structure
## updated data (0-filled subplots)

# Set up ####
library(lmerTest)
library(viridisLite)
library(visreg)
library(rgl)
source("analyses/new_response_ratio_calcs_zero_filled.R") 

## scale variables
site_pred_scaled = site_pred_final %>%
  mutate(mean_temp = mean(MAT.C),
         sd_temp = sd(MAT.C),
         mean_ppt_across = mean(mean_ppt),
         sd_ppt = sd(mean_ppt),
         z_precip = (mean_ppt - mean_ppt_across)/sd_ppt,
         z_temp = (MAT.C - mean_temp)/sd_temp)

edge_RR_preds = left_join(edge_RR, site_pred_scaled, by = "site")

## plot site level MAT and mean precip
ggplot(site_pred_scaled, aes(x=MAT.C, y=mean_ppt, color = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal)

## create df's for modeling 
DRR = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_D))

PDRR = edge_RR_preds %>%
  filter(!is.na(resp.ratio.site_PD))

# Rank ####
## drought ####
m1_rd = lmer(resp.ratio.site_D ~ percrank + z_temp*z_precip + BP.dom.site + percrank:z_precip + percrank:BP.dom.site + (percrank|site), data = DRR)

summary(m1_rd)
anova(m1_rd)

visreg(m1_rd, "percrank", by = "BP.dom.site")
visreg(m1_rd, "percrank", by = "z_precip")


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










