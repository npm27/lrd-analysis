####Score the Huff et al. (2018) data####
##This script scores the Huff et al. data using the multi list free recall functions

##Quick note: I'll be scoring this split up by version (A-F) but could score this combined.
##Would just need unique list numbers for the A keys, B keys, etc.
##(i.e., A keys would be lists 1-6, B keys lists 7-12, etc.)

#Read in the cleaned data
dat = read.csv("2 Processed Datasets/Huff et al cleaned.csv")

##load libraries
library(readxl)
library(lrd)
library(reshape)

##Load in the answer keys
##Read in answer keys
ver.A = read_excel("4 Scored Datasets/Huff et al Key.xlsx", sheet = "Version A")
ver.B = read_excel("4 Scored Datasets/Huff et al Key.xlsx", sheet = "Version B")
ver.C = read_excel("4 Scored Datasets/Huff et al Key.xlsx", sheet = "Version C")
ver.D = read_excel("4 Scored Datasets/Huff et al Key.xlsx", sheet = "Version D")
ver.E = read_excel("4 Scored Datasets/Huff et al Key.xlsx", sheet = "Version E")
ver.F = read_excel("4 Scored Datasets/Huff et al Key.xlsx", sheet = "Version F")

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

##add list numbers
datA$List_number = rep(1:6)
datB$List_number = rep(1:6)
datC$List_number = rep(1:6)
datD$List_number = rep(1:6)
datE$List_number = rep(1:6)
datF$List_number = rep(1:6)

##Get responses in long format
datA = arrange_data(datA, responses = "Response", sep = " ", id = "Username", repeated = c("List_number", "List_Type"))
datB = arrange_data(datB, responses = "Response", sep = " ", id = "Username", repeated = c("List_number", "List_Type"))
datC = arrange_data(datC, responses = "Response", sep = " ", id = "Username", repeated = c("List_number", "List_Type"))
datD = arrange_data(datD, responses = "Response", sep = " ", id = "Username", repeated = c("List_number", "List_Type"))
datE = arrange_data(datE, responses = "Response", sep = " ", id = "Username", repeated = c("List_number", "List_Type"))
datF = arrange_data(datF, responses = "Response", sep = " ", id = "Username", repeated = c("List_number", "List_Type"))

##Get answer keys in long format
#Start w/ A
ver.A$position = 1:nrow(ver.A)
LongA = melt(ver.A,
             measured = colnames(ver.A),
             id = "position")

#fix columns
colnames(LongA) = c("position", "List.name", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers
LongA$list_number = as.numeric(LongA$List.name)

##Version B
ver.B$position = 1:nrow(ver.B)
LongB = melt(ver.B,
             measured = colnames(ver.B),
             id = "position")

#fix columns
colnames(LongB) = c("position", "List.name", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers
LongB$list_number = as.numeric(LongB$List.name)

#REMOVE BLANKS
datB = datB[-c(120, 121, 122), ]

##vERSION c
ver.C$position = 1:nrow(ver.C)
LongC = melt(ver.C,
             measured = colnames(ver.C),
             id = "position")

#fix columns
colnames(LongC) = c("position", "List.name", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers
LongC$list_number = as.numeric(LongC$List.name)

##vERSION d
ver.D$position = 1:nrow(ver.D)
LongD = melt(ver.D,
             measured = colnames(ver.D),
             id = "position")

#fix columns
colnames(LongD) = c("position", "List.name", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers
LongD$list_number = as.numeric(LongD$List.name)

##version e
ver.E$position = 1:nrow(ver.E)
LongE = melt(ver.E,
             measured = colnames(ver.E),
             id = "position")

#fix columns
colnames(LongE) = c("position", "List.name", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers
LongE$list_number = as.numeric(LongE$List.name)

##Version F
ver.F$position = 1:nrow(ver.F)
LongF = melt(ver.F,
             measured = colnames(ver.F),
             id = "position")

#fix columns
colnames(LongF) = c("position", "List.name", "Answer")

#match list id to participant data, which is only numbers
#list IDs can be characters or numbers
LongF$list_number = as.numeric(LongF$List.name)

#remove NAs from the B and F keys
LongB = na.omit(LongB)
LongF = na.omit(LongF)

##Okay, ready to score! Scoring will be iterative using a distances from 0 - 5
##Start w/ version A
##Lev 0
Scored_A_0 = prop_correct_multiple(data = datA,
                      responses = "response",
                      key = LongA$Answer,
                      key.trial = LongA$list_number,
                      id = "Sub.ID",
                      id.trial = "List_number",
                      group.by = "List_Type",
                      cutoff = 0,
                      flag = FALSE)
#Lev1
Scored_A_1 = prop_correct_multiple(data = datA,
                                   responses = "response",
                                   key = LongA$Answer,
                                   key.trial = LongA$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 1,
                                   flag = FALSE)
#lev2
Scored_A_2 = prop_correct_multiple(data = datA,
                                   responses = "response",
                                   key = LongA$Answer,
                                   key.trial = LongA$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 2,
                                   flag = FALSE)
#lev3
Scored_A_3 = prop_correct_multiple(data = datA,
                                   responses = "response",
                                   key = LongA$Answer,
                                   key.trial = LongA$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 3,
                                   flag = FALSE)
#lev4
Scored_A_4 = prop_correct_multiple(data = datA,
                                   responses = "response",
                                   key = LongA$Answer,
                                   key.trial = LongA$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 4,
                                   flag = FALSE)
#lev5
Scored_A_5 = prop_correct_multiple(data = datA,
                                   responses = "response",
                                   key = LongA$Answer,
                                   key.trial = LongA$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 5,
                                   flag = FALSE)
#Now do version B
##Lev 0
Scored_B_0 = prop_correct_multiple(data = datB,
                                   responses = "response",
                                   key = LongB$Answer,
                                   key.trial = LongB$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 0,
                                   flag = FALSE)
#Lev1
Scored_B_1 = prop_correct_multiple(data = datB,
                                   responses = "response",
                                   key = LongB$Answer,
                                   key.trial = LongB$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 1,
                                   flag = FALSE)
#lev2
Scored_B_2 = prop_correct_multiple(data = datB,
                                   responses = "response",
                                   key = LongB$Answer,
                                   key.trial = LongB$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 2,
                                   flag = FALSE)
#lev3
Scored_B_3 = prop_correct_multiple(data = datB,
                                   responses = "response",
                                   key = LongB$Answer,
                                   key.trial = LongB$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 3,
                                   flag = FALSE)
#lev4
Scored_B_4 = prop_correct_multiple(data = datB,
                                   responses = "response",
                                   key = LongB$Answer,
                                   key.trial = LongB$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 4,
                                   flag = FALSE)
#lev5
Scored_B_5 = prop_correct_multiple(data = datB,
                                   responses = "response",
                                   key = LongB$Answer,
                                   key.trial = LongB$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 5,
                                   flag = FALSE)

##VERSION C
##Lev 0
Scored_C_0 = prop_correct_multiple(data = datC,
                                   responses = "response",
                                   key = LongC$Answer,
                                   key.trial = LongC$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 0,
                                   flag = FALSE)
#Lev1
Scored_C_1 = prop_correct_multiple(data = datC,
                                   responses = "response",
                                   key = LongC$Answer,
                                   key.trial = LongC$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 1,
                                   flag = FALSE)
#lev2
Scored_C_2 = prop_correct_multiple(data = datC,
                                   responses = "response",
                                   key = LongC$Answer,
                                   key.trial = LongC$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 2,
                                   flag = FALSE)
#lev3
Scored_C_3 = prop_correct_multiple(data = datC,
                                   responses = "response",
                                   key = LongC$Answer,
                                   key.trial = LongC$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 3,
                                   flag = FALSE)
#lev4
Scored_C_4 = prop_correct_multiple(data = datC,
                                   responses = "response",
                                   key = LongC$Answer,
                                   key.trial = LongC$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 4,
                                   flag = FALSE)
#lev5
Scored_C_5 = prop_correct_multiple(data = datC,
                                   responses = "response",
                                   key = LongC$Answer,
                                   key.trial = LongC$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 5,
                                   flag = FALSE)

#Now for D
##Lev 0
Scored_D_0 = prop_correct_multiple(data = datD,
                                   responses = "response",
                                   key = LongD$Answer,
                                   key.trial = LongD$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 0,
                                   flag = FALSE)
#Lev1
Scored_D_1 = prop_correct_multiple(data = datD,
                                   responses = "response",
                                   key = LongD$Answer,
                                   key.trial = LongD$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 1,
                                   flag = FALSE)
#lev2
Scored_D_2 = prop_correct_multiple(data = datD,
                                   responses = "response",
                                   key = LongD$Answer,
                                   key.trial = LongD$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 2,
                                   flag = FALSE)
#lev3
Scored_D_3 = prop_correct_multiple(data = datD,
                                   responses = "response",
                                   key = LongD$Answer,
                                   key.trial = LongD$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 3,
                                   flag = FALSE)
#lev4
Scored_D_4 = prop_correct_multiple(data = datD,
                                   responses = "response",
                                   key = LongD$Answer,
                                   key.trial = LongD$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 4,
                                   flag = FALSE)
#lev5
Scored_D_5 = prop_correct_multiple(data = datD,
                                   responses = "response",
                                   key = LongD$Answer,
                                   key.trial = LongD$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 5,
                                   flag = FALSE)

##Time for E!
##Lev 0
Scored_E_0 = prop_correct_multiple(data = datE,
                                   responses = "response",
                                   key = LongE$Answer,
                                   key.trial = LongE$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 0,
                                   flag = FALSE)
#Lev1
Scored_E_1 = prop_correct_multiple(data = datE,
                                   responses = "response",
                                   key = LongE$Answer,
                                   key.trial = LongE$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 1,
                                   flag = FALSE)
#lev2
Scored_E_2 = prop_correct_multiple(data = datE,
                                   responses = "response",
                                   key = LongE$Answer,
                                   key.trial = LongE$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 2,
                                   flag = FALSE)
#lev3
Scored_E_3 = prop_correct_multiple(data = datE,
                                   responses = "response",
                                   key = LongE$Answer,
                                   key.trial = LongE$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 3,
                                   flag = FALSE)
#lev4
Scored_E_4 = prop_correct_multiple(data = datE,
                                   responses = "response",
                                   key = LongE$Answer,
                                   key.trial = LongE$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 4,
                                   flag = FALSE)
#lev5
Scored_E_5 = prop_correct_multiple(data = datE,
                                   responses = "response",
                                   key = LongE$Answer,
                                   key.trial = LongE$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 5,
                                   flag = FALSE)

##And F
##Lev 0
Scored_F_0 = prop_correct_multiple(data = datF,
                                   responses = "response",
                                   key = LongF$Answer,
                                   key.trial = LongF$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 0,
                                   flag = FALSE)
#Lev1
Scored_F_1 = prop_correct_multiple(data = datF,
                                   responses = "response",
                                   key = LongF$Answer,
                                   key.trial = LongF$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 1,
                                   flag = FALSE)
#lev2
Scored_F_2 = prop_correct_multiple(data = datF,
                                   responses = "response",
                                   key = LongF$Answer,
                                   key.trial = LongF$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 2,
                                   flag = FALSE)
#lev3
Scored_F_3 = prop_correct_multiple(data = datF,
                                   responses = "response",
                                   key = LongF$Answer,
                                   key.trial = LongF$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 3,
                                   flag = FALSE)
#lev4
Scored_F_4 = prop_correct_multiple(data = datF,
                                   responses = "response",
                                   key = LongF$Answer,
                                   key.trial = LongF$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 4,
                                   flag = FALSE)
#lev5
Scored_F_5 = prop_correct_multiple(data = datF,
                                   responses = "response",
                                   key = LongF$Answer,
                                   key.trial = LongF$list_number,
                                   id = "Sub.ID",
                                   id.trial = "List_number",
                                   group.by = "List_Type",
                                   cutoff = 5,
                                   flag = FALSE)

##Add in version coding
Scored_A_0$DF_Scored$version = rep("A")
Scored_A_1$DF_Scored$version = rep("A")
Scored_A_2$DF_Scored$version = rep("A")
Scored_A_3$DF_Scored$version = rep("A")
Scored_A_4$DF_Scored$version = rep("A")
Scored_A_5$DF_Scored$version = rep("A")

Scored_B_0$DF_Scored$version = rep("B")
Scored_B_1$DF_Scored$version = rep("B")
Scored_B_2$DF_Scored$version = rep("B")
Scored_B_3$DF_Scored$version = rep("B")
Scored_B_4$DF_Scored$version = rep("B")
Scored_B_5$DF_Scored$version = rep("B")

Scored_C_0$DF_Scored$version = rep("C")
Scored_C_1$DF_Scored$version = rep("C")
Scored_C_2$DF_Scored$version = rep("C")
Scored_C_3$DF_Scored$version = rep("C")
Scored_C_4$DF_Scored$version = rep("C")
Scored_C_5$DF_Scored$version = rep("C")

Scored_D_0$DF_Scored$version = rep("D")
Scored_D_1$DF_Scored$version = rep("D")
Scored_D_2$DF_Scored$version = rep("D")
Scored_D_3$DF_Scored$version = rep("D")
Scored_D_4$DF_Scored$version = rep("D")
Scored_D_5$DF_Scored$version = rep("D")

Scored_E_0$DF_Scored$version = rep("E")
Scored_E_1$DF_Scored$version = rep("E")
Scored_E_2$DF_Scored$version = rep("E")
Scored_E_3$DF_Scored$version = rep("E")
Scored_E_4$DF_Scored$version = rep("E")
Scored_E_5$DF_Scored$version = rep("E")

Scored_F_0$DF_Scored$version = rep("F")
Scored_F_1$DF_Scored$version = rep("F")
Scored_F_2$DF_Scored$version = rep("F")
Scored_F_3$DF_Scored$version = rep("F")
Scored_F_4$DF_Scored$version = rep("F")
Scored_F_5$DF_Scored$version = rep("F")

##Now add in list_type coding
#version A

##Combine everything
#lev 0
final_trial_level0 = rbind(Scored_A_0$DF_Scored, Scored_B_0$DF_Scored, Scored_C_0$DF_Scored,
                           Scored_D_0$DF_Scored, Scored_E_0$DF_Scored, Scored_F_0$DF_Scored)
final_sub0 = rbind(Scored_F_0$DF_Participant, Scored_B_0$DF_Participant, Scored_C_0$DF_Participant,
                   Scored_D_0$DF_Participant, Scored_E_0$DF_Participant, Scored_F_0$DF_Participant)

#lev 1
final_trial_level1 = rbind(Scored_A_1$DF_Scored, Scored_B_1$DF_Scored, Scored_C_1$DF_Scored,
                           Scored_D_1$DF_Scored, Scored_E_1$DF_Scored, Scored_F_1$DF_Scored)
final_sub1 = rbind(Scored_A_1$DF_Participant, Scored_B_1$DF_Participant, Scored_C_1$DF_Participant,
                   Scored_D_1$DF_Participant, Scored_E_1$DF_Participant, Scored_F_1$DF_Participant)

#lev 2
final_trial_level2 = rbind(Scored_A_2$DF_Scored, Scored_B_2$DF_Scored, Scored_C_2$DF_Scored,
                           Scored_D_2$DF_Scored, Scored_E_2$DF_Scored, Scored_F_2$DF_Scored)
final_sub2 = rbind(Scored_A_2$DF_Participant, Scored_B_2$DF_Participant, Scored_C_2$DF_Participant,
                   Scored_D_2$DF_Participant, Scored_E_2$DF_Participant, Scored_F_2$DF_Participant)

#lev 3
final_trial_level3 = rbind(Scored_A_3$DF_Scored, Scored_B_3$DF_Scored, Scored_C_3$DF_Scored,
                           Scored_D_3$DF_Scored, Scored_E_3$DF_Scored, Scored_F_3$DF_Scored)
final_sub3 = rbind(Scored_A_3$DF_Participant, Scored_B_3$DF_Participant, Scored_C_3$DF_Participant,
                   Scored_D_3$DF_Participant, Scored_E_3$DF_Participant, Scored_F_3$DF_Participant)

#lev 4
final_trial_level4 = rbind(Scored_A_4$DF_Scored, Scored_B_4$DF_Scored, Scored_C_4$DF_Scored,
                           Scored_D_4$DF_Scored, Scored_E_4$DF_Scored, Scored_F_4$DF_Scored)
final_sub4 = rbind(Scored_A_4$DF_Participant, Scored_B_4$DF_Participant, Scored_C_4$DF_Participant,
                   Scored_D_4$DF_Participant, Scored_E_4$DF_Participant, Scored_F_4$DF_Participant)

#lev 5
final_trial_level5 = rbind(Scored_A_5$DF_Scored, Scored_B_5$DF_Scored, Scored_C_5$DF_Scored,
                           Scored_D_5$DF_Scored, Scored_E_5$DF_Scored, Scored_F_5$DF_Scored)
final_sub5 = rbind(Scored_A_5$DF_Participant, Scored_B_5$DF_Participant, Scored_C_5$DF_Participant,
                   Scored_D_5$DF_Participant, Scored_E_5$DF_Participant, Scored_F_5$DF_Participant)

#get list types
#trial level
final_trial_level0$List_Type = substr(final_trial_level0$List_Type, 1, nchar(final_trial_level0$List_Type)-3)
final_trial_level1$List_Type = substr(final_trial_level1$List_Type, 1, nchar(final_trial_level1$List_Type)-3)
final_trial_level2$List_Type = substr(final_trial_level2$List_Type, 1, nchar(final_trial_level2$List_Type)-3)
final_trial_level3$List_Type = substr(final_trial_level3$List_Type, 1, nchar(final_trial_level3$List_Type)-3)
final_trial_level4$List_Type = substr(final_trial_level4$List_Type, 1, nchar(final_trial_level4$List_Type)-3)
final_trial_level5$List_Type = substr(final_trial_level5$List_Type, 1, nchar(final_trial_level5$List_Type)-3)

#sub level
final_sub0$List_Type = substr(final_sub0$List_Type, 1, nchar(final_sub0$List_Type)-3)
final_sub1$List_Type = substr(final_sub1$List_Type, 1, nchar(final_sub1$List_Type)-3)
final_sub2$List_Type = substr(final_sub2$List_Type, 1, nchar(final_sub2$List_Type)-3)
final_sub3$List_Type = substr(final_sub3$List_Type, 1, nchar(final_sub3$List_Type)-3)
final_sub4$List_Type = substr(final_sub4$List_Type, 1, nchar(final_sub4$List_Type)-3)
final_sub5$List_Type = substr(final_sub5$List_Type, 1, nchar(final_sub5$List_Type)-3)

##check proportions
tapply(final_sub0$Proportion.Correct, final_sub0$List_Type, mean) * 100
tapply(final_sub1$Proportion.Correct, final_sub1$List_Type, mean) * 100
tapply(final_sub2$Proportion.Correct, final_sub2$List_Type, mean) * 100
tapply(final_sub3$Proportion.Correct, final_sub3$List_Type, mean) * 100
tapply(final_sub4$Proportion.Correct, final_sub4$List_Type, mean) * 100
tapply(final_sub5$Proportion.Correct, final_sub5$List_Type, mean) * 100

##write everything to .csv
#Trial level

##Subject level
write.csv(final_sub0, file = "4 Scored Datasets/Multi/multi0.csv", row.names = F)
write.csv(final_sub1, file = "4 Scored Datasets/Multi/multi1.csv", row.names = F)
write.csv(final_sub2, file = "4 Scored Datasets/Multi/multi2.csv", row.names = F)
write.csv(final_sub3, file = "4 Scored Datasets/Multi/multi3.csv", row.names = F)
write.csv(final_sub4, file = "4 Scored Datasets/Multi/multi4.csv", row.names = F)
write.csv(final_sub5, file = "4 Scored Datasets/Multi/multi5.csv", row.names = F)

