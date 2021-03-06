####Score the Maxwell Huff data####
##First need to add a trial number to this dataset
#Some of the data is missing trials, so this will be a bit tricky
dat = read.csv("Maxwell_Huff output.csv")

table(dat$Experiment)

A = subset(dat,
           dat$Experiment == "JOL A")
B = subset(dat,
           dat$Experiment == "JOL B")
C = subset(dat,
           dat$Experiment == "JOL C")
D = subset(dat,
           dat$Experiment == "JOL D")

##Figure out which subjects are weird
table(A$Subject) #117, 121, 125, 129
table(B$Subject) #114, 118, 122, 126
table(C$Subject) #116, 119, 123, 127
table(D$Subject) #100, 120, 124, 128

##Get all the normal subjects in their own DF
A.normal = subset(A,
                  A$Subject < 117)
B.normal = subset(B,
                  B$Subject < 114)
C.normal = subset(C,
                  C$Subject < 116)
D.normal = subset(D,
                  D$Subject < 120)
D.normal = subset(D.normal,
                  D.normal$Subject != 100)

##Now get all the weird ones in a DF
A.weird = subset(A,
                 A$Subject > 116)
B.weird = subset(B,
                 B$Subject > 113)
C.weird = subset(C,
                 C$Subject > 115)
D.weird = subset(D,
                 D$Subject > 119)

D.weird2 = subset(D,
                  D$Subject == 100)

##Add trial num -- going to do this in Excel

##Okay, first write all the weird dudes to .csv -- let's open them up in excel and see what's up
#write.csv(A.weird, file = "A weird.csv", row.names = F)
#write.csv(B.weird, file = "B weird.csv", row.names = F)
#write.csv(C.weird, file = "C weird.csv", row.names = F)
#write.csv(D.weird, file = "D weird.csv", row.names = F)

##now Write the normal ones to .csv -- need to figure out which pairs the weird ones are missing
#write.csv(A.normal, file = "A normal.csv", row.names = F)
#write.csv(B.normal, file = "B normal.csv", row.names = F)
#write.csv(C.normal, file = "C normal.csv", row.names = F)
#write.csv(D.normal, file = "D normal.csv", row.names = F)

#write.csv(D.weird2, file = "D weird2.csv", row.names = F)

##Now read the fixed weird files back in
#A2 = rbind(read.csv("A normal.csv"), read.csv("A weird.csv"))
#B2 = rbind(read.csv("B normal.csv"), read.csv("B weird.csv"))
#C2 = rbind(read.csv("C normal.csv"), read.csv("C weird.csv"))
#D2 = rbind(read.csv("D normal.csv"), read.csv("D weird.csv"), read.csv("D weird2.csv"))

##Okay! Now use lrd to score each thing separately!
library(lrd)

#scoredD = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_num",
#                            id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

#Got a duplicate trial ID error....

##Okay, let's try just the normal data for now
#D3 = read.csv("D normal.csv")

#scoredD = prop_correct_cued(D3, responses = "Response", key = "Target", key.trial = "Trial_num",
          #                 id = "Subject", id.trial = "Trial_num", cutoff = 0, flag = FALSE)

#Got it again.

#Okay, let's try just the weird data
A4 = read.csv("A weird.csv")

A4$Target = tolower(A4$Target)
A4$Response = tolower(A4$Response)

scoredA = prop_correct_cued(A4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

#This works! Let's try the other weird datasets
B4 = read.csv("B weird.csv")
C4 = read.csv("C weird.csv")
D4 = read.csv("D weird.csv")
D4.2 = read.csv("D weird2.csv")

B4$Target = tolower(B4$Target)
C4$Target = tolower(C4$Target)
D4$Target = tolower(D4$Target)
D4.2$Target = tolower(D4.2$Target)

B4$Response = tolower(B4$Response)
C4$Response = tolower(C4$Response)
D4$Response = tolower(D4$Response)
D4.2$Response = tolower(D4.2$Response)

scoredB = prop_correct_cued(B4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

scoredC = prop_correct_cued(C4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

scoredD = prop_correct_cued(D4, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 0, flag = FALSE)

scoredD.2 = prop_correct_cued(D4.2, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 0, flag = FALSE)

####Okay, these all work. So that means that the "normal data" has something weird going on with the trial IDs
#table(A3$Target) #"Cook" doesn't belong
#table(B3$Target) #"Sarh" doesn't belong. Neither does "Firh"
#table(C3$Target) #Early is in here 8 times. That could be throwing things off. Also baby is in here 17 times
#table(D3$Target) #Got early in here again. And cake. And baby needs fixing.

##Okay, I think I fixed the data in Excel, so let's read these files in again!
A2 = read.csv("A normal.csv")
B2 = read.csv("B normal.csv")
C2 = read.csv("C normal.csv")
D2 = read.csv("D normal.csv")

A2$Target = tolower(A2$Target)
B2$Target = tolower(B2$Target)
C2$Target = tolower(C2$Target)
D2$Target = tolower(D2$Target)

A2$Response = tolower(A2$Response)
B2$Response = tolower(B2$Response)
C2$Response = tolower(C2$Response)
D2$Response = tolower(D2$Response)

#Scoring time! Fingers crossed that it works now.
scoredA2 = prop_correct_cued(A2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

scoredB2 = prop_correct_cued(B2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

scoredD2 = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

#Okay, A, B, and D work! C is still my problem child. Let's take another look at it.
table(C2$Target) #Don't see anything too weird. Could just be an ordering thing

#And I forgot to reorder things in Excel. Fixed and now it should work.

#C2 = read.csv("C normal.csv")

#scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
      #         id = "Subject", id.trial = "Trial_Num", cutoff = 0, flag = FALSE)

##Alright, now that I have all the various scored outputs, time to piece them together
outA1 = scoredA$DF_Scored
outA2 = scoredA2$DF_Scored

outB1 = scoredB$DF_Scored
outB2 = scoredB2$DF_Scored

outC1 = scoredC$DF_Scored
outC2 = scoredC2$DF_Scored

outD1 = scoredD$DF_Scored
outD2 = scoredD2$DF_Scored
outD3 = scoredD.2$DF_Scored

combined = rbind(outA1, outA2, outB1, outB2, outC1, outC2, outD1, outD2, outD3)

##The above used a lev of 0 -- Now we need to use a lev of 1
scoredA = prop_correct_cued(A4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

scoredB = prop_correct_cued(B4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

scoredC = prop_correct_cued(C4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

scoredD = prop_correct_cued(D4, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 1, flag = FALSE)

scoredD.2 = prop_correct_cued(D4.2, responses = "Response", key = "Target", key.trial = "Trial_num",
                              id = "Subject", id.trial = "Trial_num", cutoff = 1, flag = FALSE)

scoredA2 = prop_correct_cued(A2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

scoredB2 = prop_correct_cued(B2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

scoredD2 = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 1, flag = FALSE)

outA1 = scoredA$DF_Scored
outA2 = scoredA2$DF_Scored

outB1 = scoredB$DF_Scored
outB2 = scoredB2$DF_Scored

outC1 = scoredC$DF_Scored
outC2 = scoredC2$DF_Scored

outD1 = scoredD$DF_Scored
outD2 = scoredD2$DF_Scored
outD3 = scoredD.2$DF_Scored

combined2 = rbind(outA1, outA2, outB1, outB2, outC1, outC2, outD1, outD2, outD3)

##Now lev of 2
scoredA = prop_correct_cued(A4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

scoredB = prop_correct_cued(B4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

scoredC = prop_correct_cued(C4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

scoredD = prop_correct_cued(D4, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 2, flag = FALSE)

scoredD.2 = prop_correct_cued(D4.2, responses = "Response", key = "Target", key.trial = "Trial_num",
                              id = "Subject", id.trial = "Trial_num", cutoff = 2, flag = FALSE)

scoredA2 = prop_correct_cued(A2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

scoredB2 = prop_correct_cued(B2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

scoredD2 = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 2, flag = FALSE)

outA1 = scoredA$DF_Scored
outA2 = scoredA2$DF_Scored

outB1 = scoredB$DF_Scored
outB2 = scoredB2$DF_Scored

outC1 = scoredC$DF_Scored
outC2 = scoredC2$DF_Scored

outD1 = scoredD$DF_Scored
outD2 = scoredD2$DF_Scored
outD3 = scoredD.2$DF_Scored

combined3 = rbind(outA1, outA2, outB1, outB2, outC1, outC2, outD1, outD2, outD3)

##Now lev of 3
scoredA = prop_correct_cued(A4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

scoredB = prop_correct_cued(B4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

scoredC = prop_correct_cued(C4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

scoredD = prop_correct_cued(D4, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 3, flag = FALSE)

scoredD.2 = prop_correct_cued(D4.2, responses = "Response", key = "Target", key.trial = "Trial_num",
                              id = "Subject", id.trial = "Trial_num", cutoff = 3, flag = FALSE)

scoredA2 = prop_correct_cued(A2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

scoredB2 = prop_correct_cued(B2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

scoredD2 = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 3, flag = FALSE)

outA1 = scoredA$DF_Scored
outA2 = scoredA2$DF_Scored

outB1 = scoredB$DF_Scored
outB2 = scoredB2$DF_Scored

outC1 = scoredC$DF_Scored
outC2 = scoredC2$DF_Scored

outD1 = scoredD$DF_Scored
outD2 = scoredD2$DF_Scored
outD3 = scoredD.2$DF_Scored

combined4 = rbind(outA1, outA2, outB1, outB2, outC1, outC2, outD1, outD2, outD3)

##Now lev of 4
scoredA = prop_correct_cued(A4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

scoredB = prop_correct_cued(B4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

scoredC = prop_correct_cued(C4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

scoredD = prop_correct_cued(D4, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 4, flag = FALSE)

scoredD.2 = prop_correct_cued(D4.2, responses = "Response", key = "Target", key.trial = "Trial_num",
                              id = "Subject", id.trial = "Trial_num", cutoff = 4, flag = FALSE)

scoredA2 = prop_correct_cued(A2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

scoredB2 = prop_correct_cued(B2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

scoredD2 = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 4, flag = FALSE)

outA1 = scoredA$DF_Scored
outA2 = scoredA2$DF_Scored

outB1 = scoredB$DF_Scored
outB2 = scoredB2$DF_Scored

outC1 = scoredC$DF_Scored
outC2 = scoredC2$DF_Scored

outD1 = scoredD$DF_Scored
outD2 = scoredD2$DF_Scored
outD3 = scoredD.2$DF_Scored

combined5 = rbind(outA1, outA2, outB1, outB2, outC1, outC2, outD1, outD2, outD3)

##And finally a lev of 5
scoredA = prop_correct_cued(A4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

scoredB = prop_correct_cued(B4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

scoredC = prop_correct_cued(C4, responses = "Response", key = "Target", key.trial = "Trial_Num",
                            id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

scoredD = prop_correct_cued(D4, responses = "Response", key = "Target", key.trial = "Trial_num",
                            id = "Subject", id.trial = "Trial_num", cutoff = 5, flag = FALSE)

scoredD.2 = prop_correct_cued(D4.2, responses = "Response", key = "Target", key.trial = "Trial_num",
                              id = "Subject", id.trial = "Trial_num", cutoff = 5, flag = FALSE)

scoredA2 = prop_correct_cued(A2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

scoredB2 = prop_correct_cued(B2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

scoredC2 = prop_correct_cued(C2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

scoredD2 = prop_correct_cued(D2, responses = "Response", key = "Target", key.trial = "Trial_Num",
                             id = "Subject", id.trial = "Trial_Num", cutoff = 5, flag = FALSE)

outA1 = scoredA$DF_Scored
outA2 = scoredA2$DF_Scored

outB1 = scoredB$DF_Scored
outB2 = scoredB2$DF_Scored

outC1 = scoredC$DF_Scored
outC2 = scoredC2$DF_Scored

outD1 = scoredD$DF_Scored
outD2 = scoredD2$DF_Scored
outD3 = scoredD.2$DF_Scored

combined6 = rbind(outA1, outA2, outB1, outB2, outC1, outC2, outD1, outD2, outD3)

##Okay, put all the things together
colnames(D4)[7] = "Trial_Num"
colnames(D4.2)[7] = "Trial_Num"

manual_scoring = rbind(A2, B2, C2, D2, A4, B4, C4, D4, D4.2)

#Now need to get this in the same order as the lrd scored data
manual_scoring = manual_scoring[order(manual_scoring$Target), ]
manual_scoring = manual_scoring[order(manual_scoring$Subject), ]

combined = combined[order(combined$Target), ]
combined = combined[order(combined$Sub.ID), ]

combined2 = combined2[order(combined2$Target), ]
combined2 = combined2[order(combined2$Sub.ID), ]

combined3 = combined3[order(combined3$Target), ]
combined3 = combined3[order(combined3$Sub.ID), ]

combined4 = combined4[order(combined4$Target), ]
combined4 = combined4[order(combined4$Sub.ID), ]

combined5 = combined5[order(combined5$Target), ]
combined5 = combined5[order(combined5$Sub.ID), ]

combined6 = combined6[order(combined6$Target), ]
combined6 = combined6[order(combined6$Sub.ID), ]

##Make final dataset
final = manual_scoring

final$lev0 = combined$Scored
final$lev1 = combined2$Scored
final$lev2 = combined3$Scored
final$lev3 = combined4$Scored
final$lev4 = combined5$Scored
final$lev5 = combined6$Scored

#Write to .csv
#write.csv(final, file = "Maxwell Huff Scored.csv", row.names = F)
