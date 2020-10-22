####setup####
dat = read.csv("Maxwell Huff Scored.csv")

library(caret)

summary(dat)

####sensitivity analysis####
#set up
lev0 = factor(dat$lev0)
lev1 = factor(dat$lev1)
lev2 = factor(dat$lev2)
lev3 = factor(dat$lev3)
lev4 = factor(dat$lev4)
lev5 = factor(dat$lev5)

ref = factor(dat$Manual_Scored)

#start with 0
table(lev0, ref)
sensitivity(lev0, ref)
specificity(lev0, ref)

#1
table(lev1, ref)
sensitivity(lev1, ref)
specificity(lev1, ref)

#2
table(lev2, ref)
sensitivity(lev2, ref)
specificity(lev2, ref)

#3
table(lev3, ref)
sensitivity(lev3, ref)
specificity(lev3, ref)

#4
table(lev4, ref)
sensitivity(lev4, ref)
specificity(lev4, ref)

#5
table(lev5, ref)
sensitivity(lev5, ref)
specificity(lev5, ref)