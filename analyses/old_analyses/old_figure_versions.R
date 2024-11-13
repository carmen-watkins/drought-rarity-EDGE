## OLD FIGURE VERSIONS

ggplot(edge_RR_cats, aes(x=drought.RR, y=recovery.RR, color = rarity_cat)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2.5)+
  facet_wrap(~MAP_level, nrow = 1, ncol = 3) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  scale_color_manual(values = c("#5D69B1", "#CC61B0", "#99C945","#E58606")) +
  labs(color = "Rarity") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.22, 1.22)) +
  annotate(geom="text", x=-0.75, y=1.2, label="low resistance, high recovery",
           color="black", size = 3.5) +
  annotate(geom="text", x=-0.75, y=-1.2, label="low resistance, low recovery",
           color="black", size = 3.5) +
  annotate(geom="text", x=0.75, y=1.2, label="high resistance, high recovery",
           color="black", size = 3.5) +
  annotate(geom="text", x=0.75, y=-1.2, label="high resistance, low recovery",
           color="black", size = 3.5) +
  theme(text = element_text(size = 15))

ggsave("preliminary_figs/oct_2024/figure4_color_cats.png", width = 7, height = 6)

## fig 4 alt views ####  
edge_FG_cats$rarity_cat = as.factor(edge_FG_cats$rarity_cat)

edge_FG_cats = edge_FG_cats %>%
  mutate(rarity_cat = fct_relevel(rarity_cat, "Transient, Abundant", "Transient, Scarce", "Core, Abundant", "Core, Scarce"))

### site ####
ggplot(edge_FG_cats, aes(x=drought.RR, y=recovery.RR, color = site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2)+
  facet_wrap(~rarity_cat, nrow = 3, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  #scale_color_manual(values = c("#5D69B1", "#52BCA3", "#99C945","#E58606")) +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.02)+
  scale_color_manual(values = pal) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.25, 1.25
  )) +
  annotate(geom="text", x=-0.75, y=1.2, label="-D+R",
           color="black", size = 3) +
  annotate(geom="text", x=-0.75, y=-1.2, label="-D-R",
           color="black", size = 3) +
  annotate(geom="text", x=0.75, y=1.2, label="+D+R",
           color="black", size = 3) +
  annotate(geom="text", x=0.75, y=-1.2, label="+D-R",
           color="black", size = 3) +
  theme(text = element_text(size = 15))

ggsave("preliminary_figs/oct_2024/post_lab_feedback/figure4_cats_colorsite.png", width = 7, height = 6)

# Figure S5 ####
rankD3 <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

rankR3 <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persD3 <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persR3 <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(rankD3, persD3, rankR3, persR3, 
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

ggsave("preliminary_figs/oct_2024/post_lab_feedback/figs5_site_RR_diffs.png", width = 10, height = 8.5)

# Old Fig Versions ####
## keep for posterity
## Fig 1 attempt ####
edge_FG_long = edge_FG %>%
  pivot_longer(cols = c("persistence.site", "percrank"), names_to = "rarity_type", values_to = "rarity_score") %>%
  mutate(rarity_type = ifelse(rarity_type == "persistence.site", "Temporal", "Spatial"))

d = ggplot(edge_FG_long, aes(x=rarity_score, y=drought.RR)) +
  geom_point(aes(fill=rarity_type), 
             colour="black",pch=21, size=1.5, alpha = 0.25) + 
  scale_fill_manual(values = c("black", "white")) +
  geom_smooth(method = "lm", alpha = 0.1, linewidth = 2, aes(color = rarity_type)) +
  scale_color_manual(values = c("#202020", "#a3a3a3")) +
  xlab("Rarity") + 
  ylab("Drought Response Ratio") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_color_manual(values = c("#E58606", "#5D69B1"))+
  scale_shape_manual(values = c(19, 1)) +
  labs(fill = "Rarity Type", color = "Rarity Type") +
  theme(text = element_text(size = 15))

pd = ggplot(edge_FG_long, aes(x=rarity_score, y=recovery.RR)) +
  geom_point(aes(fill=rarity_type), 
             colour="black",pch=21, size=1.5, alpha = 0.25) + 
  scale_fill_manual(values = c("black", "white")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.1, linewidth = 2, aes(color = rarity_type)) +
  scale_color_manual(values = c("#080808", "#a3a3a3")) +
  xlab("Rarity") + 
  ylab("Post-Drought Response Ratio") +
  
  #scale_color_manual(values = c("#E58606", "#5D69B1"))+
  scale_shape_manual(values = c(19, 1)) +
  labs(fill = "Rarity Type", color = "Rarity Type") +
  theme(text = element_text(size = 15))

ggarrange(d, pd, common.legend = TRUE, labels = "AUTO", legend = "bottom")
ggsave("preliminary_figs/oct_2024/fig2rarity_responses_nosite.tiff", width = 8, height = 4.5)


## Figure 1 - not needed ####
ggplot(edge_FG_cats, aes(x=percrank, y=persistence.site))+
  # scale_color_manual(values = c("#5D69B1", "#CC61B0", "#99C945","#E58606")) +
  geom_point(size = 2.5, alpha = 0.65, color = "#898989") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab(NULL) +
  ylab(NULL) +
  labs(color = "Rarity") +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0.5) +
  #coord_cartesian(ylim = c(-0.05, 1.05)) +
  # annotate("text", x = 0.15, y = 1.05, label = "Core, Scarce", size = 4) +
  #annotate("text", x = 0.15, y = -0.05, label = "Transient, Scarce", size = 4) +
  #annotate("text", x = 0.75, y = 1.05, label = "Core, Abundant", size = 4) +
  # annotate("text", x = 0.75, y = -0.05, label = "Transient, Abundant", size = 4) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 13)) +
  scale_x_reverse() +
  scale_y_reverse() 

#ggsave("preliminary_figs/oct_2024/post_lab_feedback/figure1.tiff", width = 4.2, height = 4)

## panel B
str(edge_FG_cats)
edge_FG_cats$rarity_cat = as.factor(edge_FG_cats$rarity_cat)
edge_FG_cats = edge_FG_cats %>%
  mutate(rarity_cat = fct_relevel(rarity_cat, "Core, Scarce", "Core, Abundant", "Transient, Scarce", "Transient, Abundant"))

ggplot(edge_FG_cats, aes(x=drought.RR, y=recovery.RR, color = site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2.5)+
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  # scale_color_manual(values = c("#CC61B0", "#5D69B1","#E58606", "#99C945")) +
  scale_color_manual(values = pal) +
  labs(color = "Rarity") +
  theme_bw() +
  geom_smooth(method = "lm", alpha = .1, linewidth = 2) +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.22, 1.22)) +
  annotate(geom="text", x=-0.75, y=1.2, label="low resist, high recov",
           color="black", size = 2.5) +
  annotate(geom="text", x=-0.75, y=-1.2, label="low resist, low recov",
           color="black", size = 2.5) +
  annotate(geom="text", x=0.75, y=1.2, label="high resist, high recov",
           color="black", size = 2.5) +
  annotate(geom="text", x=0.75, y=-1.2, label="high resist, low recov",
           color="black", size = 2.5) +
  theme(text = element_text(size = 13)) +
  theme(legend.position = "bottom") +
  facet_wrap(~rarity_cat)

## Figure S5 ####
### Persist by Site ####
ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = persistence.site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 1, ncol = 6) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_gradientn(
    colors = c("#E3B710", "#DCCB4E", "#BDC881", "#A2A475", "#81A88D", "#00A08A", "#0B775E", "#175149")) +
  labs(color = "Persistence")  +
  theme(legend.position="bottom") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white"))

ggsave("preliminary_figs/june_2024/figureS5.tiff", width = 9, height = 2)
