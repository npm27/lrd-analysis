##sensitivity and specificity
#set up
library(caret)

dat = read.csv("scored_combined.csv")

ref1 = factor(dat$Corr1)
ref2 = factor(dat$Corr2)

lev0 = factor(dat$lrd_cor_0)
lev1 = factor(dat$lrd_cor_1)
lev2 = factor(dat$lrd_cor_2)
lev3 = factor(dat$lrd_cor_3)
lev4 = factor(dat$lrd_cor_4)
lev5 = factor(dat$lrd_cor_5)

#Compare the two human raters
table(ref1, ref2)
sensitivity(ref1, ref2)
specificity(ref1, ref2)

#Now compare all the lrd scorings to the first rater
#0
table(lev0, ref1)
sensitivity(lev0, ref1)
specificity(lev0, ref1)

#1
table(lev1, ref1)
sensitivity(lev1, ref1)
specificity(lev1, ref1)

#2
table(lev2, ref1)
sensitivity(lev2, ref1)
specificity(lev2, ref1)

#3
table(lev3, ref1)
sensitivity(lev3, ref1)
specificity(lev3, ref1)

#4
table(lev4, ref1)
sensitivity(lev4, ref1)
specificity(lev4, ref1)

#5
table(lev5, ref1)
sensitivity(lev5, ref1)
specificity(lev5, ref1)

#Now compare all the lrd scorings to the second rater
#0
table(lev0, ref2)
sensitivity(lev0, ref2)
specificity(lev0, ref2)

#1
table(lev1, ref2)
sensitivity(lev1, ref2)
specificity(lev1, ref2)

#2
table(lev2, ref2)
sensitivity(lev2, ref2)
specificity(lev2, ref2)

#3
table(lev3, ref2)
sensitivity(lev3, ref2)
specificity(lev3, ref2)

#4
table(lev4, ref2)
sensitivity(lev4, ref2)
specificity(lev4, ref2)

#5
table(lev5, ref2)
sensitivity(lev5, ref2)
specificity(lev5, ref2)
