####Get descriptives for both datasets####
m_h = read.csv("Maxwell Huff Scored.csv")
m_b = read.csv("maxwell_buchanan scored.csv")

summary(m_h)
summary(m_b)

##Get mean manually coded
mean(m_h$Manual_Scored, na.rm = T)
mean(m_b$manually_coded, na.rm = T)

##Now get means for each scoring criteria
apply(m_h[ , 8:13], 2, mean)
apply(m_b[ , 8:13], 2, mean)

####Let's run some stats!####
library(data.table)
library(ez)

options(scipen = 999)

m_h$Subject = as.character(m_h$Subject)
m_b$Sub.ID = as.character(m_b$Sub.ID)

##Melt the things
#Maxwell and Huff data
long.mh = melt(m_h)

colnames(long.mh)[6] = "Score_Type"
colnames(long.mh)[7] = "Score"

#Maxwell & Buchanan data
long.mb = melt(m_b)

colnames(long.mb)[6] = "Score_Type"
colnames(long.mb)[7] = "Score"

##remove missing
nomiss_mh = na.omit(long.mh).
nomiss_mb = na.omit(long.mb) 

##Anova
#Maxwell Huff
model1 = ezANOVA(nomiss_mh,
                 wid = Subject,
                 within = Score_Type,
                 dv = Score,
                 type = 3,
                 detailed = T)
model1

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

##Maxwell Buchanan
model2 = ezANOVA(nomiss_mb,
                 wid = Sub.ID,
                 within = Score_Type,
                 dv = Score,
                 type = 3,
                 detailed = T)
model2

model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

##Doing the post hoc things
#Start w/ Maxwell Huff dataset
#get 95%CI
tap1 = tapply(nomiss_mh$Score,
              nomiss_mh$Score_Type, sd)

se1 = tap1 / sqrt(length(unique(nomiss_mh$Subject)))

se1 * 1.96

library(reshape)

mh_posthoc = cast(long.mh, Subject ~ Score_Type, mean, na.rm = T)

##Now do the t-test comparisons
#Manual
t.test(mh_posthoc$Manual_Scored, mh_posthoc$lev0, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$Manual_Scored, mh_posthoc$lev1, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$Manual_Scored, mh_posthoc$lev2, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$Manual_Scored, mh_posthoc$lev3, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$Manual_Scored, mh_posthoc$lev4, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$Manual_Scored, mh_posthoc$lev5, paired = F, p.adjust.methods = "bonferroni")

#0
t.test(mh_posthoc$lev0, mh_posthoc$lev1, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev0, mh_posthoc$lev2, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev0, mh_posthoc$lev3, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev0, mh_posthoc$lev4, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev0, mh_posthoc$lev5, paired = F, p.adjust.methods = "bonferroni")

#1
t.test(mh_posthoc$lev1, mh_posthoc$lev2, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev1, mh_posthoc$lev3, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev1, mh_posthoc$lev4, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev1, mh_posthoc$lev5, paired = F, p.adjust.methods = "bonferroni")

#2
t.test(mh_posthoc$lev2, mh_posthoc$lev3, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev2, mh_posthoc$lev4, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev2, mh_posthoc$lev5, paired = F, p.adjust.methods = "bonferroni")

#3
t.test(mh_posthoc$lev3, mh_posthoc$lev4, paired = F, p.adjust.methods = "bonferroni")
t.test(mh_posthoc$lev3, mh_posthoc$lev5, paired = F, p.adjust.methods = "bonferroni")

#4
t.test(mh_posthoc$lev4, mh_posthoc$lev5, paired = F, p.adjust.methods = "bonferroni")

##Now means and sds for effect size (cohens d)
apply(mh_posthoc[, c(4:9)], 2, mean)
apply(mh_posthoc[, c(4:9)], 2, sd)

##Now the Maxwell Buchanan dataset
tap1 = tapply(nomiss_mb$Score,
              nomiss_mb$Score_Type, sd)

se1 = tap1 / sqrt(length(unique(nomiss_mb$Sub.ID)))

se1 * 1.96

mb_posthoc = cast(long.mb, Sub.ID ~ Score_Type, mean, na.rm = T)

##Now do the t-test comparisons
#Manual
t.test(mb_posthoc$manually_coded, mb_posthoc$scored0, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$manually_coded, mb_posthoc$scored1, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$manually_coded, mb_posthoc$scored2, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$manually_coded, mb_posthoc$scored3, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$manually_coded, mb_posthoc$scored4, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$manually_coded, mb_posthoc$scored5, paired = F, p.adjust.methods = "bonferroni")

#0
t.test(mb_posthoc$scored0, mb_posthoc$scored1, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored0, mb_posthoc$scored2, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored0, mb_posthoc$scored3, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored0, mb_posthoc$scored4, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored0, mb_posthoc$scored5, paired = F, p.adjust.methods = "bonferroni")

#1
t.test(mb_posthoc$scored1, mb_posthoc$scored2, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored1, mb_posthoc$scored3, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored1, mb_posthoc$scored4, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored1, mb_posthoc$scored5, paired = F, p.adjust.methods = "bonferroni")

#2
t.test(mb_posthoc$scored2, mb_posthoc$scored3, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored2, mb_posthoc$scored4, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored2, mb_posthoc$scored5, paired = F, p.adjust.methods = "bonferroni")

#3
t.test(mb_posthoc$scored3, mb_posthoc$scored4, paired = F, p.adjust.methods = "bonferroni")
t.test(mb_posthoc$scored3, mb_posthoc$scored5, paired = F, p.adjust.methods = "bonferroni")

#4
t.test(mb_posthoc$scored4, mb_posthoc$scored5, paired = F, p.adjust.methods = "bonferroni")

##Now means and sds for effect size (cohens d)
apply(mb_posthoc[, c(4:9)], 2, mean)
apply(mb_posthoc[, c(4:9)], 2, sd)

####And now do kappas####
library(psych)

cohen.kappa(m_b[ , c(6, 8:13)]) #M & B

cohen.kappa(m_h[ , c(6, 8:13)]) #M & H
