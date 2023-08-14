
colnames(RR.slope.rich)

map <- lm(slope.drought.rank~MAP.mm, data = RR.slope.rich)
summary(map)

rich.model <- lm(slope.drought.rank~SR, data = RR.slope.rich)
summary(rich.model)


map2 <- lm(slope.recov.rank~MAP.mm, data = RR.slope.rich)
summary(map2)

rich.model2 <- lm(slope.recov.rank~SR, data = RR.slope.rich)
summary(rich.model2)


map3 <- lm(slope.drought.persistence~MAP.mm, data = RR.slope.rich)
summary(map3)

rich.model3 <- lm(slope.drought.persistence~SR, data = RR.slope.rich)
summary(rich.model3)


map4 <- lm(slope.recov.persistence~MAP.mm, data = RR.slope.rich)
summary(map4)

rich.model4 <- lm(slope.recov.persistence~SR, data = RR.slope.rich)
summary(rich.model4)
