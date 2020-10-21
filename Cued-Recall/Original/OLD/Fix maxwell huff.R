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

##Add trial num to normal ones
A.normal$Trial = rep(1:160)
B.normal$Trial = rep(1:160)
C.normal$Trial = rep(1:160)
D.normal$Trial = rep(1:160)

##Okay, now write all the weird dudes to .csv -- let's open them up in excel and see what's up
#write.csv(A.weird, file = "A weird.csv", row.names = F)
#write.csv(B.weird, file = "B weird.csv", row.names = F)
#write.csv(C.weird, file = "C weird.csv", row.names = F)
#write.csv(D.weird, file = "D weird.csv", row.names = F)

##Now read the fixed weird files back in

##Combine!

##Write final output to .csv
