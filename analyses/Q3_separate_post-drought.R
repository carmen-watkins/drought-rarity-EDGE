## Q3 does the effect post-drought fade basically

## Explore first and final 2 years of post-drought period as separate response ratios.

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

ggarrange(pa, pb, pc, pd, common.legend = TRUE, legend = "bottom")  

ggsave("figures/Jan2025/post_drought_separated.png", width = 8, height = 7)
  
