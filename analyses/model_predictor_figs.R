## Create model predictor figures

# Set up ####
library(ggpubr)

source("analyses/color_palettes.R")
theme_set(theme_classic())
pal <- wes_palette("Royal3")

site_pred_scaled$site = factor(site_pred_scaled$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Make Figure ####
a = ggplot(site_pred_scaled, aes(x=site, y=mean_ppt, color = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 15)) +
  labs(color = "Site") +
  xlab(" ") +
  ylab("Mean Growing Season Precip")

b = ggplot(site_pred_scaled, aes(x=site, y=MAT.C, color = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 15)) +
  labs(color = "Site") +
  xlab("Site") +
  ylab("Mean Annual Temperature")

c = ggplot(site_pred_scaled, aes(x=site, y=dom.rounded, color = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 15)) +
  labs(color = "Site") +
  xlab(" ") +
  ylab("Berger-Parker Dominance")

ggarrange(a, b, c, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right", labels = "AUTO")
ggsave("figures/Dec2024/site_predictors.png", width = 11, height = 4)

rm(a, b, c)