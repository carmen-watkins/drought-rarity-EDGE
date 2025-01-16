# Header ####
## Script name: Plotting Linear model outputs
##
## Purpose of script: Try different ways of visualizing model output to determine final figures & supplementary figures
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

## this script relies on having run linear_models.R

## plot model coefficients
srd = plot_summs(md4s_all)
trd = plot_summs(md4t_all)
srpd = plot_summs(mpd4s_all)
trpd = plot_summs(mpd4t_all)

ggarrange(srd, trd, 
          srpd, trpd, ncol = 2, nrow = 2)


plot_summs(md4s_all, mpd4s_all)

plot_summs(md4t_all, mpd4t_all)


export_summs(md4s_all, mpd4s_all, scale = TRUE)

lm()



## try out jtools package
summ(md4s_all)


effect_plot(md4s_all, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = "Spatial Rarity")


effect_plot(md4t_all, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = "Temporal Rarity", facet.by = site)

ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~site)
## 9 bins not a lot
## skewed towards abund sp

edge_RR %>%
  group_by(site, temporal_rarity) %>%
  summarise(meanDRR = mean(resp.ratio.site_D4, na.rm = T),
            seDRR = calcSE(resp.ratio.site_D4)) %>%
ggplot(aes(x=temporal_rarity, y=meanDRR)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~site) +
  geom_errorbar(aes(ymin = meanDRR - seDRR, ymax = meanDRR + seDRR))

ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~site)


effect_plot(md4s_all, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Response Ratio", x.label = "Spatial Rarity")
effect_plot(md4t_all, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = "Response Ratio", x.label = "Temporal Rarity")




effect_plot(md4s_all, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, partial.residuals = TRUE)

#effect_plot(md4s_all, pred = spatial_rarity, interval = TRUE, rug = TRUE)

effect_plot(md4s_all, pred = site, interval = TRUE, plot.points = TRUE, partial.residuals = TRUE)


effect_plot(md4s_all, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)

effect_plot(md4s_all, pred = site, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)

effect_plot(md4s_all, pred = spatial_rarity:site, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)





plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(md4s_all), xlab = "Spatial Rarity", ylab = "Drought Response Ratio")

intercept = md4s_all$coefficients[1]
rarity_slope = md4s_all$coefficients["spatial_rarity"]


plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(a = intercept, b = rarity_slope, lwd = 3), xlab = "Spatial Rarity", ylab = "Drought Response Ratio", pch = 20)

plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(a = intercept, b = rarity_slope, lwd = 3), xlab = "Spatial Rarity", ylab = "Drought Response Ratio", pch = 20)


HYS_int_slope = md4s_all$coefficients["spatial_rarity:siteHYS"]
SBK_int_slope = md4s_all$coefficients["spatial_rarity:siteSBK"]


KNZ_slope = md4s_all$coefficients["siteKNZ"] ## ah, no siteKNZ coeff because these are all compared to KNZ...
HYS_slope = md4s_all$coefficients["siteHYS"]
CHY_slope = md4s_all$coefficients["siteCHY"]
SGS_slope = md4s_all$coefficients["siteSGS"]
SBK_slope = md4s_all$coefficients["siteSBK"]
SBL_slope = md4s_all$coefficients["siteSBL"]



plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(a = intercept, b=HYS_int_slope), xlab = "Spatial Rarity", ylab = "Drought Response Ratio")


plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(a = intercept, b=SBK_slope), xlab = "Spatial Rarity", ylab = "Drought Response Ratio")

plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(a = intercept, b=SBK_int_slope), xlab = "Spatial Rarity", ylab = "Drought Response Ratio")

## site effects 
plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     xlab = "Spatial Rarity", ylab = "Drought Response Ratio")
abline(a = intercept, b=KNZ_slope)
abline(a = intercept, b=HYS_slope)
abline(a = intercept, b=CHY_slope)
abline(a = intercept, b=SGS_slope)
abline(a = intercept, b=SBK_slope)
abline(a = intercept, b=SBL_slope)



plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(md4s_add))


plot(x = edge_RR$spatial_rarity, y = edge_RR$resp.ratio.site_D4,
     abline(md4s_int))

ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  #geom_smooth(aes(color = site), method = "lm", alpha = 0.5, linewidth = 2) +
  geom_abline(md4s_all) +
  #geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Relative MAP") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))