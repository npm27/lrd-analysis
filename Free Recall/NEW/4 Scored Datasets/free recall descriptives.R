####set up####
##load datasets
unrel = read.csv("Unrel_final.csv")
cat = read.csv("cat_final.csv")
adhoc = read.csv("adhoc_final.csv")

##load libraries
library(psych)
library(ez)
library(reshape)

options(scipen = 999)

####Descriptives####
##Get means
apply(unrel[ , c(6:12)], 2, mean, na.rm = T)
apply(cat[ , c(5:11)], 2, mean, na.rm = T)
apply(adhoc[ , c(5:11)], 2, mean, na.rm = T)

##Get 95% CI's
#Adhoc
adhoc.long = melt(adhoc, measure.vars = c("Human", "Lev0", "Lev1", "Lev2", "Lev3", "Lev4", "Lev5"))
colnames(adhoc.long)[5:6] = c("Score_Type", "Score")
adhoc.wide = cast(adhoc.long, Username ~ Score_Type, mean, na.rm = T)

(apply(adhoc.wide[ , c(2:8)], 2, sd) / sqrt(length(unique(adhoc.wide$Username)))) * 1.96

#Cat
cat.long = melt(cat, measure.vars = c("Human", "Lev0", "Lev1", "Lev2", "Lev3", "Lev4", "Lev5"))
colnames(cat.long)[5:6] = c("Score_Type", "Score")
cat.wide = cast(cat.long, Username ~ Score_Type, mean, na.rm = T)

(apply(cat.wide[ , c(2:8)], 2, sd) / sqrt(length(unique(cat.wide$Username)))) * 1.96

##Unrelated
unrel.long = melt(unrel, measure.vars = c("Human", "Lev0", "Lev1", "Lev2", "Lev3", "Lev4", "Lev5"))
colnames(unrel.long)[6:7] = c("Score_Type", "Score")
unrel.wide = cast(unrel.long, Username ~ Score_Type, mean, na.rm = T)

(apply(unrel.wide[ , c(2:8)], 2, sd) / sqrt(length(unique(unrel.wide$Username)))) * 1.96

##Fix score scale
adhoc.long$Score = adhoc.long$Score * 100
unrel.long$Score = unrel.long$Score * 100
cat.long$Score = cat.long$Score * 100

cat2 = na.omit(cat.long)
unrel2 = na.omit(unrel.long)
adhoc2 = na.omit(adhoc.long)

####Run the ANOVA####
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

####Make table 12####
##Start w/ adhoc
apply(adhoc.wide[ , 2:length(adhoc.wide)], 2, mean)
apply(adhoc.wide[ , 2:length(adhoc.wide)], 2, sd)

##Now do categorical
apply(cat.wide[ , 2:length(cat.wide)], 2, mean)
apply(cat.wide[ , 2:length(cat.wide)], 2, sd)

##And finally the unrelated list
apply(unrel.wide[ , 2:length(unrel.wide)], 2, mean)
apply(unrel.wide[ , 2:length(unrel.wide)], 2, sd)

####Kappas####
cohen.kappa(unrel[ , c(6:12)]) #Unrelated

cohen.kappa(adhoc[ , c(5:11)]) #Adhoc

cohen.kappa(cat[ , c(5:11)]) #categorical

