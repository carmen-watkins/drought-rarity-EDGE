
colnames(response.ratio.tog)

mKNZ <- lm(resp.ratio.recov ~ resp.ratio.drought, data = response.ratio.tog[response.ratio.tog$site == "KNZ",])
summary(mKNZ)

mHYS <- lm(resp.ratio.recov ~ resp.ratio.drought, data = response.ratio.tog[response.ratio.tog$site == "HYS",])
summary(mHYS)

mCHY <- lm(resp.ratio.recov ~ resp.ratio.drought, data = response.ratio.tog[response.ratio.tog$site == "CHY",])
summary(mCHY)

mSGS <- lm(resp.ratio.recov ~ resp.ratio.drought, data = response.ratio.tog[response.ratio.tog$site == "SGS",])
summary(mSGS)

mSEV_blue <- lm(resp.ratio.recov ~ resp.ratio.drought, data = response.ratio.tog[response.ratio.tog$site == "SEV_blue",])
summary(mSEV_blue)

mSEV_black <- lm(resp.ratio.recov ~ resp.ratio.drought, data = response.ratio.tog[response.ratio.tog$site == "SEV_black",])
summary(mSEV_black)
