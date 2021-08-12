####Score the Huff et al. (2018) data####
##This script scores the Huff et al. data using the multi list free recall functions

#Read in the cleaned data
dat = read.csv("2 Processed Datasets/Huff et al cleaned.csv")

##load libraries
library(readxl)
library(lrd)
library(reshape)

keyA = readxl::read_excel("longkeys.xlsx", sheet = "Version A")
keyB = readxl::read_excel("longkeys.xlsx", sheet = "Version B")

key = rbind(keyA, keyB)

##Remove spaces from answer key
key = apply(key, 2, function(x)gsub(' ', '', x))
key = as.data.frame(key)

#lowercase
key$Key_Item = tolower(key$Key_Item)

##add list numbers
#going to need to subset for this
version1 = subset(dat,
                  dat$Version == "A" | dat$Version == "C" | dat$Version == "E") #THESE ALL USE THE SAME LISTS
version2 = subset(dat,
                  dat$Version == "B" | dat$Version == "D" | dat$Version == "F") #THESE ALL USE THE SAME LISTS

version1$List_Number = rep(c(1,2,3,3,1,2), times = 240/20)
version2$List_Number = rep(c(4,5,5,6,6,4), times = 240/20)

combined = rbind(version1, version2)

##get the dataset in long format
dat_long = arrange_data(combined, responses = "Response", sep = " ", id = "Username", repeated = c("List_Number", "List_Type", "Version"))

##Remove list coding from type
dat_long$List_Type = gsub('.{3}$', '', dat_long$List_Type)

##Okay, ready to score! Scoring will be iterative using a distances from 0 - 5
##Lev 0
Scored0 = prop_correct_multiple(data = dat_long,
                                   responses = "response",
                                   key = key$Key_Item,
                                   key.trial = key$List_Number,
                                   id = "Sub.ID",
                                   id.trial = "List_Number",
                                   group.by = "List_Type",
                                   cutoff = 0,
                                   flag = FALSE)

#lev1
Scored1 = prop_correct_multiple(data = dat_long,
                                responses = "response",
                                key = key$Key_Item,
                                key.trial = key$List_Number,
                                id = "Sub.ID",
                                id.trial = "List_Number",
                                group.by = "List_Type",
                                cutoff = 1,
                                flag = FALSE)

#lev2
Scored2 = prop_correct_multiple(data = dat_long,
                                responses = "response",
                                key = key$Key_Item,
                                key.trial = key$List_Number,
                                id = "Sub.ID",
                                id.trial = "List_Number",
                                group.by = "List_Type",
                                cutoff = 2,
                                flag = FALSE)

#lev3
Scored3 = prop_correct_multiple(data = dat_long,
                                responses = "response",
                                key = key$Key_Item,
                                key.trial = key$List_Number,
                                id = "Sub.ID",
                                id.trial = "List_Number",
                                group.by = "List_Type",
                                cutoff = 3,
                                flag = FALSE)

#lev4
Scored4 = prop_correct_multiple(data = dat_long,
                                responses = "response",
                                key = key$Key_Item,
                                key.trial = key$List_Number,
                                id = "Sub.ID",
                                id.trial = "List_Number",
                                group.by = "List_Type",
                                cutoff = 4,
                                flag = FALSE)

#lev5
Scored5 = prop_correct_multiple(data = dat_long,
                                responses = "response",
                                key = key$Key_Item,
                                key.trial = key$List_Number,
                                id = "Sub.ID",
                                id.trial = "List_Number",
                                group.by = "List_Type",
                                cutoff = 5,
                                flag = FALSE)

#check means
tapply(Scored0$DF_Participant$Proportion.Correct, Scored0$DF_Participant$List_Type, mean)
tapply(Scored1$DF_Participant$Proportion.Correct, Scored1$DF_Participant$List_Type, mean)
tapply(Scored2$DF_Participant$Proportion.Correct, Scored2$DF_Participant$List_Type, mean)
tapply(Scored3$DF_Participant$Proportion.Correct, Scored3$DF_Participant$List_Type, mean)
tapply(Scored4$DF_Participant$Proportion.Correct, Scored4$DF_Participant$List_Type, mean)
tapply(Scored5$DF_Participant$Proportion.Correct, Scored5$DF_Participant$List_Type, mean)

##Write trial level data to .csv for analyses
#write.csv(Scored0$DF_Scored, file = "4 Scored Datasets/Multi/M0.csv", row.names = F)
#write.csv(Scored1$DF_Scored, file = "4 Scored Datasets/Multi/M1.csv", row.names = F)
#write.csv(Scored2$DF_Scored, file = "4 Scored Datasets/Multi/M2.csv", row.names = F)
#write.csv(Scored3$DF_Scored, file = "4 Scored Datasets/Multi/M3.csv", row.names = F)
#write.csv(Scored4$DF_Scored, file = "4 Scored Datasets/Multi/M4.csv", row.names = F)
#write.csv(Scored5$DF_Scored, file = "4 Scored Datasets/Multi/M5.csv", row.names = F)
