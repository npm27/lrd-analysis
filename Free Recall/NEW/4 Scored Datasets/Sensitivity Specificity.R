library(caret)

dat = read.csv("Multi 8_16.csv")

##subset out the list types
unrel = subset(dat,
               dat$list_type == "Unrel")
cat = subset(dat,
             dat$list_type == "Cat")
adhoc = subset(dat,
               dat$list_type == "Ad_hoc")

#Let's do unrel first
ref = factor(unrel$Human)

lev0 = factor(unrel$LEV0)
lev1 = factor(unrel$LEV1)
lev2 = factor(unrel$LEV2)
lev3 = factor(unrel$LEV3)
lev4 = factor(unrel$LEV4)
lev5 = factor(unrel$LEV5)

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

#Now for Cat
ref = factor(cat$Human)

lev0 = factor(cat$LEV0)
lev1 = factor(cat$LEV1)
lev2 = factor(cat$LEV2)
lev3 = factor(cat$LEV3)
lev4 = factor(cat$LEV4)
lev5 = factor(cat$LEV5)

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

##Now adhoc
ref = factor(adhoc$Human)

lev0 = factor(adhoc$LEV0)
lev1 = factor(adhoc$LEV1)
lev2 = factor(adhoc$LEV2)
lev3 = factor(adhoc$LEV3)
lev4 = factor(adhoc$LEV4)
lev5 = factor(adhoc$LEV5)

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

