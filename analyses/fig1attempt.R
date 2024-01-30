

## Figure 1: drought response ratios individually?
#ggplot(response.ratio.tog, aes(x=as.factor(resp.ratio.drought), y=filler.variable)) +
 # geom_bar(stat = 'identity') +
  #facet_wrap(~site) +
  #geom_vline(xintercept = 0, color = "red")

ggplot(response.ratio.tog, aes(x=resp.ratio.drought)) +
  geom_histogram() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  xlab("Drought Response Ratio")

ggplot(response.ratio.tog, aes(x=resp.ratio.recov)) +
  geom_histogram() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  xlab("Recovery Response Ratio")
