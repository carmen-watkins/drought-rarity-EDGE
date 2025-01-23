## Q3 does the effect post-drought fade basically

## Explore first and final 2 years of post-drought period as separate response ratios.
mmtpi = lmer(resp.ratio.site_PDfirst ~ temporal_rarity + (1|site), data = edge_RR)
mmtpf = lmer(resp.ratio.site_PDfinal ~ temporal_rarity + (1|site), data = edge_RR)

summary(mmtpi)
summary(mmtpf)

mmtpi_tab = as.data.frame(summary(mmtpi)$coeff) %>%
  mutate(rarity = "Temporal",
         period = "Initial")
mmtpf_tab = as.data.frame(summary(mmtpf)$coeff) %>%
  mutate(rarity = "Temporal",
         period = "Final")

confint(mmtpi)
confint(mmtpf)

mmspi = lmer(resp.ratio.site_PDfirst ~ spatial_rarity + (1|site), data = edge_RR)
mmspf = lmer(resp.ratio.site_PDfinal ~ spatial_rarity + (1|site), data = edge_RR)

summary(mmspi)
summary(mmspf)

mmspi_tab = as.data.frame(summary(mmspi)$coeff) %>%
  mutate(rarity = "Spatial",
         period = "Initial")
mmspf_tab = as.data.frame(summary(mmspf)$coeff) %>%
  mutate(rarity = "Spatial",
         period = "Final")

pd_sep_coeff_tab = rbind(mmspi_tab, mmspf_tab, mmtpi_tab, mmtpf_tab) %>%
  rownames_to_column(var = "type") %>%
  select(rarity, period, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`)

write.csv(pd_sep_coeff_tab, "tables/post_drought_separated_coeff_table.csv")

# Create tables
### decided to use type II Anovas - for when data is unbalanced and DON'T want to consider interactions
mmspi_tab = as.data.frame(Anova(mmspi, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought Initial",
         rarity = "Spatial")

mmspf_tab = as.data.frame(Anova(mmspf, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought Final",
         rarity = "Spatial")

mmtpi_tab = as.data.frame(Anova(mmtpi, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought Initial",
         rarity = "Temporal")

mmtpf_tab = as.data.frame(Anova(mmtpf, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought Final",
         rarity = "Temporal")

anova_df_pd = rbind(mmspi_tab, mmspf_tab, mmtpi_tab, mmtpf_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` )

#write.csv(anova_df_pd, "tables/mixed_mod_anova_table_pd_separated.csv")

# Plot ####
spi = effect_plot(mmspi, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Post-Drought Response Ratio", x.label = "Spatial Rarity", 
                  colors = "#909090", 
                  line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  ggtitle("Initial")

spf = effect_plot(mmspf, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, x.label = "Spatial Rarity", 
                  colors = "#909090", 
                  line.colors = "black") +
  ylab(NULL) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  ggtitle("Final")

tpi = effect_plot(mmtpi, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, x.label = "Temporal Rarity", 
            colors = "#909090", 
            line.colors = "black") +
  theme_classic() +
  ylab(NULL) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  ggtitle("Initial")

tpf = effect_plot(mmtpf, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, x.label = "Temporal Rarity", 
            colors = "#909090", 
            line.colors = "black") +
  ylab(NULL) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  ggtitle("Final")

ggarrange(spi, spf, tpi, tpf, ncol = 4, nrow = 1)

ggsave("figures/Jan2025/pd_separated_mmfig.png", width = 10, height = 3)

# Create DFs of Model ####
## spatial, drought ####
sites = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")

## spatial, first ####
mod_pdi = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfirst ~ spatial_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought Initial")
  
  ## append
  mod_pdi = rbind(mod_pdi, tmp) %>%
    filter(!is.na(term))
  
}

## spatial, final ####
mod_pdf = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfinal ~ spatial_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought Final")
  
  ## append
  mod_pdf = rbind(mod_pdf, tmp) %>%
    filter(!is.na(term))
  
}


## temporal, first ####
modt_pdi = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfirst ~ temporal_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought Initial")
  
  ## append
  modt_pdi = rbind(modt_pdi, tmp) %>%
    filter(!is.na(term))
  
}

modt_pdf = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfinal ~ temporal_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought Final")
  
  ## append
  modt_pdf = rbind(modt_pdf, tmp) %>%
    filter(!is.na(term))
  
}



## combine ####
sp_mods_pdsplit = rbind(mod_pdi, mod_pdf) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmp_mods_pdsplit = rbind(modt_pdi, modt_pdf) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))
  
pa = tmp_mods_pdsplit %>%
    filter(term == "temporal_rarity") %>%
  mutate(period = fct_relevel(period, "Post-Drought Initial", "Post-Drought Final")) %>%
  
    ggplot(aes(x = estimate, y = site, shape = period)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_point(aes(fill = site, shape = period), colour = "black", size = 3.5) +
    scale_fill_manual(values = pal) +
    geom_vline(xintercept = 0, lty = 2) +
    labs(x = "Slope",
         y = "Temporal Rarity", 
         shape = "") +
  scale_shape_manual(values = c(21, 13)) +
    labs(fill = "Site") +
  coord_cartesian(xlim = c(-1.5, 2.1)) +
  
    #theme(axis.text.y = element_blank(),
     #     axis.ticks.y = element_blank()) +
    #coord_cartesian(xlim = c(-0.8, 2.1)) +
    #theme(axis.text.x=element_text(size=12)) +
    #theme(#axis.text.y=element_text(size=12),
    #  axis.title=element_text(size=13)) +
    theme(text = element_text(size = 13))# +
#  ggtitle("Temporal Rarity")

pb = tmp_mods_pdsplit %>%
  filter(term == "(Intercept)") %>%
  mutate(period = fct_relevel(period, "Post-Drought Initial", "Post-Drought Final")) %>%
  
  ggplot(aes(x = estimate, y = site, shape = period)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site, shape = period), colour = "black", size = 3.5) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept",
       y = "") +
  scale_shape_manual(values = c(21, 13)) +
  labs(fill = "Site") +
  #facet_wrap(~period) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  #coord_cartesian(xlim = c(-0.8, 2.1)) +
  #theme(axis.text.x=element_text(size=12)) +
  #theme(#axis.text.y=element_text(size=12),
  #  axis.title=element_text(size=13)) +
  theme(text = element_text(size = 13))

pc = sp_mods_pdsplit %>%
  filter(term == "spatial_rarity") %>%
  mutate(period = fct_relevel(period, "Post-Drought Initial", "Post-Drought Final")) %>%
  
  ggplot(aes(x = estimate, y = site, shape = period)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site, shape = period), colour = "black", size = 3.5) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope",
       y = "Spatial Rarity") +
  scale_shape_manual(values = c(21, 13)) +
  labs(fill = "Site") +
  #theme(axis.text.y = element_blank(),
   #     axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-1.5, 2.1)) +
  #theme(axis.text.x=element_text(size=12)) +
  #theme(#axis.text.y=element_text(size=12),
  #  axis.title=element_text(size=13)) +
  theme(text = element_text(size = 13))

pd = sp_mods_pdsplit %>%
  filter(term == "(Intercept)") %>%
  mutate(period = fct_relevel(period, "Post-Drought Initial", "Post-Drought Final")) %>%
  ggplot(aes(x = estimate, y = site, shape = period)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site, shape = period), colour = "black", size = 3.5) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept",
       y = "", 
       shape = "") +
  scale_shape_manual(values = c(21, 13)) +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  #coord_cartesian(xlim = c(-0.8, 2.1)) +
  #theme(axis.text.x=element_text(size=12)) +
  #theme(#axis.text.y=element_text(size=12),
  #  axis.title=element_text(size=13)) +
  theme(text = element_text(size = 13))

ggarrange(pc, pd, pa, pb,  common.legend = TRUE, legend = "bottom")  

ggsave("figures/Jan2025/post_drought_separated.png", width = 8, height = 7)
  
