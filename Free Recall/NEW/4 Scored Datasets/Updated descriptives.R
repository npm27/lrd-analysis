dat = read.csv("Multi 8_16.csv")

##Drop the ID columns
dat = dat[ , -c(7,8,10,11,13,14,16,17,19,20,22,23)]

##subset out the list types
unrel = subset(dat,
               dat$list_type == "Unrel")
cat = subset(dat,
             dat$list_type == "Cat")
adhoc = subset(dat,
               dat$list_type == "Ad_hoc")

##load libraries
library(psych)
library(ez)
library(reshape)

options(scipen = 999)

##Get means
apply(unrel[ , c(7:12)], 2, mean, na.rm = T) * 100
apply(cat[ , c(7:12)], 2, mean, na.rm = T) * 100
apply(adhoc[ , c(7:12)], 2, mean, na.rm = T) * 100

##Get 95% CI's
#Adhoc
adhoc.long = melt(adhoc, measure.vars = c("Human", "LEV0", "LEV1", "LEV2", "LEV3", "LEV4", "LEV5"))
colnames(adhoc.long)[6:7] = c("Score_Type", "Score")
adhoc.wide = cast(adhoc.long, Username ~ Score_Type, mean, na.rm = T)

((apply(adhoc.wide[ , c(2:8)], 2, sd) / sqrt(length(unique(adhoc.wide$Username)))) * 1.96) * 100

#CAT
cat.long = melt(cat, measure.vars = c("Human", "LEV0", "LEV1", "LEV2", "LEV3", "LEV4", "LEV5"))
colnames(cat.long)[6:7] = c("Score_Type", "Score")
cat.wide = cast(cat.long, Username ~ Score_Type, mean, na.rm = T)

((apply(cat.wide[ , c(2:8)], 2, sd) / sqrt(length(unique(cat.wide$Username)))) * 1.96) * 100

#Unrel
unrel.long = melt(unrel, measure.vars = c("Human", "LEV0", "LEV1", "LEV2", "LEV3", "LEV4", "LEV5"))
colnames(unrel.long)[6:7] = c("Score_Type", "Score")
unrel.wide = cast(unrel.long, Username ~ Score_Type, mean, na.rm = T)

((apply(unrel.wide[ , c(2:8)], 2, sd) / sqrt(length(unique(unrel.wide$Username)))) * 1.96) * 100

##Fix score scale
adhoc.long$Score = adhoc.long$Score * 100
unrel.long$Score = unrel.long$Score * 100
cat.long$Score = cat.long$Score * 100

cat2 = na.omit(cat.long)
unrel2 = na.omit(unrel.long)
adhoc2 = na.omit(adhoc.long)

####Run the ANOVAs####
##Adhoc
model1 = ezANOVA(adhoc2,
                 wid = Username,
                 between = Score_Type,
                 dv = Score,
                 detailed = T,
                 type = 3)

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd

model1

##categorical
model2 = ezANOVA(cat2,
                 wid = Username,
                 between = Score_Type,
                 dv = Score,
                 detailed = T,
                 type = 3)

model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd

model2

##unrelated
model3 = ezANOVA(unrel2,
                 wid = Username,
                 between = Score_Type,
                 dv = Score,
                 detailed = T,
                 type = 3)

model3$ANOVA$MSE = model3$ANOVA$SSd/model3$ANOVA$DFd

model3
