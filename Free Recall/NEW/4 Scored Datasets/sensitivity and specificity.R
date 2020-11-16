dat = read.csv("Unrel_final.csv")

library(caret)

ref = factor(dat$Human)

lev0 = factor(dat$Lev0)
lev1 = factor(dat$Lev1)
lev2 = factor(dat$Lev2)
lev3 = factor(dat$Lev3)
lev4 = factor(dat$Lev4)
lev5 = factor(dat$Lev5)

#0
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

