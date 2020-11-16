####Score the Huff et al. (2018) data####
library(RecordLinkage)

##load in cleaned data
dat = read.csv("2 Processed Datasets/Huff et al cleaned.csv")

##load libraries
library(readxl)
library(lrd)

##Read in answer keys
ver.A = read_excel("1 Raw Datasets/Huff et al Key.xlsx", sheet = "Version A")
ver.B = read_excel("1 Raw Datasets/Huff et al Key.xlsx", sheet = "Version B")
ver.C = read_excel("1 Raw Datasets/Huff et al Key.xlsx", sheet = "Version C")
ver.D = read_excel("1 Raw Datasets/Huff et al Key.xlsx", sheet = "Version D")
ver.E = read_excel("1 Raw Datasets/Huff et al Key.xlsx", sheet = "Version E")
ver.F = read_excel("1 Raw Datasets/Huff et al Key.xlsx", sheet = "Version F")

##Remove spaces from answer key
ver.A = apply(ver.A, 2, function(x)gsub(' ', '', x))
ver.A = as.data.frame(ver.A)

ver.B = apply(ver.B, 2, function(x)gsub(' ', '', x))
ver.B = as.data.frame(ver.B)

ver.C = apply(ver.C, 2, function(x)gsub(' ', '', x))
ver.C = as.data.frame(ver.C)

ver.D = apply(ver.D, 2, function(x)gsub(' ', '', x))
ver.D = as.data.frame(ver.D)

ver.E = apply(ver.E, 2, function(x)gsub(' ', '', x))
ver.E = as.data.frame(ver.E)

ver.F = apply(ver.F, 2, function(x)gsub(' ', '', x))
ver.F = as.data.frame(ver.F)

##Lowercase everything
ver.A = data.frame(lapply(ver.A, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.B = data.frame(lapply(ver.B, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.C = data.frame(lapply(ver.C, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.D = data.frame(lapply(ver.D, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))


ver.E = data.frame(lapply(ver.E, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

ver.F = data.frame(lapply(ver.F, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

##subset responses by version
datA = subset(dat,
              dat$Version == "A")
datB = subset(dat,
              dat$Version == "B")
datC = subset(dat,
              dat$Version == "C")
datD = subset(dat,
              dat$Version == "D")
datE = subset(dat,
              dat$Version == "E")
datF = subset(dat,
              dat$Version == "F")

##now need to subset out each list type
##A
A1 = subset(datA,
            datA$List_Type == "Cat_Recall_L1")
A2 = subset(datA,
            datA$List_Type == "AdHoc_Recall_L2")
A3 = subset(datA,
            datA$List_Type == "Unrel_Recall_L3")
A4 = subset(datA,
            datA$List_Type == "Unrel_Recall_L4")
A5 = subset(datA,
            datA$List_Type == "Cat_Recall_L5")
A6 = subset(datA,
            datA$List_Type == "AdHoc_Recall_L6")

##B
B1 = subset(datB,
            datB$List_Type == "Unrel_Recall_L1")
B2 = subset(datB,
            datB$List_Type == "AdHoc_Recall_L2")
B3 = subset(datB,
            datB$List_Type == "AdHoc_Recall_L3")
B4 = subset(datB,
            datB$List_Type == "Cat_Recall_L4")
B5 = subset(datB,
            datB$List_Type == "Cat_Recall_L5")
B6 = subset(datB,
            datB$List_Type == "Unrel_Recall_L6")

##C
C1 = subset(datC,
            datC$List_Type == "Cat_Recall_L1")
C2 = subset(datC,
            datC$List_Type == "AdHoc_Recall_L2")
C3 = subset(datC,
            datC$List_Type == "Unrel_Recall_L3")
C4 = subset(datC,
            datC$List_Type == "Unrel_Recall_L4")
C5 = subset(datC,
            datC$List_Type == "Cat_Recall_L5")
C6 = subset(datC,
            datC$List_Type == "AdHoc_Recall_L6")

##D
D1 = subset(datD,
            datD$List_Type == "Unrel_Recall_L1")
D2 = subset(datD,
            datD$List_Type == "AdHoc_Recall_L2")
D3 = subset(datD,
            datD$List_Type == "AdHoc_Recall_L3")
D4 = subset(datD,
            datD$List_Type == "Cat_Recall_L4")
D5 = subset(datD,
            datD$List_Type == "Cat_Recall_L5")
D6 = subset(datD,
            datD$List_Type == "Unrel_Recall_L6")

##E
E1 = subset(datE,
            datE$List_Type == "Cat_Recall_L1")
E2 = subset(datE,
            datE$List_Type == "AdHoc_Recall_L2")
E3 = subset(datE,
            datE$List_Type == "Unrel_Recall_L3")
E4 = subset(datE,
            datE$List_Type == "Unrel_Recall_L4")
E5 = subset(datE,
            datE$List_Type == "Cat_Recall_L5")
E6 = subset(datE,
            datE$List_Type == "AdHoc_Recall_L6")

##F
F1 = subset(datF,
            datF$List_Type == "Unrel_Recall_L1")
F2 = subset(datF,
            datF$List_Type == "AdHoc_Recall_L2")
F3 = subset(datF,
            datF$List_Type == "AdHoc_Recall_L3")
F4 = subset(datF,
            datF$List_Type == "Cat_Recall_L4")
F5 = subset(datF,
            datF$List_Type == "Cat_Recall_L5")
F6 = subset(datF,
            datF$List_Type == "Unrel_Recall_L6")

##Remove blank responses
A4 = A4[-15, ]

##Remove question marks
E6 = E6[-112, ]

####Get data in long format####
##Version A
A1 = arrange_data(A1, responses = "Response", sep = " ", id = "Username")
A2 = arrange_data(A2, responses = "Response", sep = " ", id = "Username")
A3 = arrange_data(A3, responses = "Response", sep = " ", id = "Username")
A4 = arrange_data(A4, responses = "Response", sep = " ", id = "Username")
A5 = arrange_data(A5, responses = "Response", sep = " ", id = "Username")
A6 = arrange_data(A6, responses = "Response", sep = " ", id = "Username")

##Version B
B1 = arrange_data(B1, responses = "Response", sep = " ", id = "Username")
B2 = arrange_data(B2, responses = "Response", sep = " ", id = "Username")
B3 = arrange_data(B3, responses = "Response", sep = " ", id = "Username")
B4 = arrange_data(B4, responses = "Response", sep = " ", id = "Username")
B5 = arrange_data(B5, responses = "Response", sep = " ", id = "Username")
B6 = arrange_data(B6, responses = "Response", sep = " ", id = "Username")

##Version C
C1 = arrange_data(C1, responses = "Response", sep = " ", id = "Username")
C2 = arrange_data(C2, responses = "Response", sep = " ", id = "Username")
C3 = arrange_data(C3, responses = "Response", sep = " ", id = "Username")
C4 = arrange_data(C4, responses = "Response", sep = " ", id = "Username")
C5 = arrange_data(C5, responses = "Response", sep = " ", id = "Username")
C6 = arrange_data(C6, responses = "Response", sep = " ", id = "Username")

##Version D
D1 = arrange_data(D1, responses = "Response", sep = " ", id = "Username")
D2 = arrange_data(D2, responses = "Response", sep = " ", id = "Username")
D3 = arrange_data(D3, responses = "Response", sep = " ", id = "Username")
D4 = arrange_data(D4, responses = "Response", sep = " ", id = "Username")
D5 = arrange_data(D5, responses = "Response", sep = " ", id = "Username")
D6 = arrange_data(D6, responses = "Response", sep = " ", id = "Username")

##Version E
E1 = arrange_data(E1, responses = "Response", sep = " ", id = "Username")
E2 = arrange_data(E2, responses = "Response", sep = " ", id = "Username")
E3 = arrange_data(E3, responses = "Response", sep = " ", id = "Username")
E4 = arrange_data(E4, responses = "Response", sep = " ", id = "Username")
E5 = arrange_data(E5, responses = "Response", sep = " ", id = "Username")
E6 = arrange_data(E6, responses = "Response", sep = " ", id = "Username")

##Version E
F1 = arrange_data(F1, responses = "Response", sep = " ", id = "Username")
F2 = arrange_data(F2, responses = "Response", sep = " ", id = "Username")
F3 = arrange_data(F3, responses = "Response", sep = " ", id = "Username")
F4 = arrange_data(F4, responses = "Response", sep = " ", id = "Username")
F5 = arrange_data(F5, responses = "Response", sep = " ", id = "Username")
F6 = arrange_data(F6, responses = "Response", sep = " ", id = "Username")

####Okay now we can score the stuff####
##First batch of scoring will use a cutoff of 0

##Start with version A
scoredA1 = prop_correct_free(A1, responses = "response", key = ver.A$Cat_Recall_L1, id = "Sub.ID", cutoff = 5)
scoredA2 = prop_correct_free(A2, responses = "response", key = ver.A$AdHoc_Recall_L2, id = "Sub.ID", cutoff = 5)
scoredA3 = prop_correct_free(A3, responses = "response", key = ver.A$Unrel_Recall_L3, id = "Sub.ID", cutoff = 5)
scoredA4 = prop_correct_free(A4, responses = "response", key = ver.A$Unrel_Recall_L4, id = "Sub.ID", cutoff = 5)
scoredA5 = prop_correct_free(A5, responses = "response", key = ver.A$Cat_Recall_L5, id = "Sub.ID", cutoff = 5)
scoredA6 = prop_correct_free(A6, responses = "response", key = ver.A$AdHoc_Recall_L6, id = "Sub.ID", cutoff = 5)

##Now version B
scoredB1 = prop_correct_free(B1, responses = "response", key = ver.D$Unrel_Recall_L1, id = "Sub.ID", cutoff = 5) #ver.B key didn't work for some reason
scoredB2 = prop_correct_free(B2, responses = "response", key = ver.B$AdHoc_Recall_L2, id = "Sub.ID", cutoff = 5)
scoredB3 = prop_correct_free(B3, responses = "response", key = ver.B$AdHoc_Recall_L3, id = "Sub.ID", cutoff = 5)
scoredB4 = prop_correct_free(B4, responses = "response", key = ver.B$Cat_Recall_L4, id = "Sub.ID", cutoff = 5)
scoredB5 = prop_correct_free(B5, responses = "response", key = ver.B$Cat_Recall_L5, id = "Sub.ID", cutoff = 5)
scoredB6 = prop_correct_free(B6, responses = "response", key = ver.B$Unrel_Recall_L6, id = "Sub.ID", cutoff = 5)

##Now version C
scoredC1 = prop_correct_free(C1, responses = "response", key = ver.C$Cat_Recall_L1, id = "Sub.ID", cutoff = 5)
scoredC2 = prop_correct_free(C2, responses = "response", key = ver.C$AdHoc_Recall_L2, id = "Sub.ID", cutoff = 5)
scoredC3 = prop_correct_free(C3, responses = "response", key = ver.C$Unrel_Recall_L3, id = "Sub.ID", cutoff = 5)
scoredC4 = prop_correct_free(C4, responses = "response", key = ver.C$Unrel_Recall_L4, id = "Sub.ID", cutoff = 5)
scoredC5 = prop_correct_free(C5, responses = "response", key = ver.C$Cat_Recall_L5, id = "Sub.ID", cutoff = 5)
scoredC6 = prop_correct_free(C6, responses = "response", key = ver.C$AdHoc_Recall_L6, id = "Sub.ID", cutoff = 5)

##Version D
scoredD1 = prop_correct_free(D1, responses = "response", key = ver.D$Unrel_Recall_L1, id = "Sub.ID", cutoff = 5)
scoredD2 = prop_correct_free(D2, responses = "response", key = ver.D$AdHoc_Recall_L2, id = "Sub.ID", cutoff = 5)
scoredD3 = prop_correct_free(D3, responses = "response", key = ver.D$AdHoc_Recall_L3, id = "Sub.ID", cutoff = 5)
scoredD4 = prop_correct_free(D4, responses = "response", key = ver.D$Cat_Recall_L4, id = "Sub.ID", cutoff = 5)
scoredD5 = prop_correct_free(D5, responses = "response", key = ver.D$Cat_Recall_L5, id = "Sub.ID", cutoff = 5)
scoredD6 = prop_correct_free(D6, responses = "response", key = ver.D$Unrel_Recall_L6, id = "Sub.ID", cutoff = 5)

##Version E
scoredE1 = prop_correct_free(E1, responses = "response", key = ver.E$Cat_Recall_L1, id = "Sub.ID", cutoff = 5)
scoredE2 = prop_correct_free(E2, responses = "response", key = ver.E$AdHoc_Recall_L2, id = "Sub.ID", cutoff = 5)
scoredE3 = prop_correct_free(E3, responses = "response", key = ver.E$Unrel_Recall_L3, id = "Sub.ID", cutoff = 5)
scoredE4 = prop_correct_free(E4, responses = "response", key = ver.E$Unrel_Recall_L4, id = "Sub.ID", cutoff = 5)
scoredE5 = prop_correct_free(E5, responses = "response", key = ver.E$Cat_Recall_L5, id = "Sub.ID", cutoff = 5)
scoredE6 = prop_correct_free(E6, responses = "response", key = ver.C$AdHoc_Recall_L6, id = "Sub.ID", cutoff = 5)

##Now version F
scoredF1 = prop_correct_free(F1, responses = "response", key = ver.D$Unrel_Recall_L1, id = "Sub.ID", cutoff = 5)
scoredF2 = prop_correct_free(F2, responses = "response", key = ver.F$AdHoc_Recall_L2, id = "Sub.ID", cutoff = 5)
scoredF3 = prop_correct_free(F3, responses = "response", key = ver.F$AdHoc_Recall_L3, id = "Sub.ID", cutoff = 5)
scoredF4 = prop_correct_free(F4, responses = "response", key = ver.F$Cat_Recall_L4, id = "Sub.ID", cutoff = 5)
scoredF5 = prop_correct_free(F5, responses = "response", key = ver.F$Cat_Recall_L5, id = "Sub.ID", cutoff = 5)
scoredF6 = prop_correct_free(F6, responses = "response", key = ver.F$Unrel_Recall_L6, id = "Sub.ID", cutoff = 5)

####Okay, now to combine scored output####
Unrel = rbind(scoredA3$DF_Scored$Scored, scoredA4$DF_Scored, scoredB1$DF_Scored, scoredB6$DF_Scored, scoredC3$DF_Scored, scoredC4$DF_Scored,
              scoredD1$DF_Scored, scoredD6$DF_Scored, scoredE3$DF_Scored, scoredE4$DF_Scored, scoredF1$DF_Scored, scoredF6$DF_Scored)

Ad_hoc = rbind(scoredA2$DF_Scored, scoredA6$DF_Scored, scoredB2$DF_Scored, scoredB3$DF_Scored, scoredC2$DF_Scored, scoredC6$DF_Scored,
               scoredD2$DF_Scored, scoredD3$DF_Scored, scoredE2$DF_Scored, scoredE6$DF_Scored, scoredF2$DF_Scored, scoredF3$DF_Scored)

Cat = rbind(scoredA1$DF_Scored, scoredA5$DF_Scored, scoredB4$DF_Scored, scoredB5$DF_Scored, scoredC1$DF_Scored, scoredC5$DF_Scored,
            scoredD4$DF_Scored, scoredD5$DF_Scored, scoredE1$DF_Scored, scoredE5$DF_Scored, scoredF4$DF_Scored, scoredF5$DF_Scored)

####Write trial level data to .csv####
write.csv(Cat, file = "4 Scored Datasets/Cat/CAT5.csv", row.names = F)
write.csv(Ad_hoc, file = "4 Scored Datasets/Adhoc/ADHOC5.csv", row.names = F)
write.csv(Unrel, file = "4 Scored Datasets/Unrel/UNREL5.csv", row.names = F)
