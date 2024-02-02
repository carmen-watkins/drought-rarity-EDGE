library(lmerTest)

## rank & persistence in same model
## drought
FG_model<-lm(drought.RR~persistence.site*FunctionalGroup*percrank*site, data = edge_FG)

summary(FG_model)
anova(FG_model)

## recovery
FG_model_recov<-lm(recovery.RR~persistence.site*FunctionalGroup*percrank*site, data = edge_FG)

summary(FG_model_recov)
anova(FG_model_recov)


## rank only in models
FG_modelR<-lm(recovery.RR~persistence.site*FunctionalGroup*percrank*site, data = edge_FG[edge_FG$FunctionalGroup != "tree" & !is.na(edge_FG$FunctionalGroup),])

summary(FG_modelR)
anova(FG_modelR)


FG_model_recovR<-lm(resp.ratio.site~FunctionalGroup*percrank, data = edge_FG_recov[edge_FG_recov$FunctionalGroup != "tree" & !is.na(edge_FG_recov$FunctionalGroup),])

summary(FG_model_recovR)



## site as random effect?
