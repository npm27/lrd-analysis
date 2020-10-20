####set up####
dat = read.csv("Maxwell_Huff output.csv")

colnames(dat)[3] = "Cue"
colnames(dat)[4] = "Key"
colnames(dat)[2] = "Sub.ID"
colnames(dat)[6] = "manually_coded"

##Get the data organized for scoring
#need to add trial ids
table(dat$Experiment)

library(lrd)

##Okay, subset by experiment
A = subset(dat,
           dat$Experiment == "JOL A")

B = subset(dat,
           dat$Experiment == "JOL B")

C = subset(dat,
           dat$Experiment == "JOL C")

D = subset(dat,
           dat$Experiment == "JOL D")

##Next, need to sort the data and add trial numbers
#160 different pair types
table(A$Sub.ID) #117, 121, 125, and 129 each have 144 observations for some reason
table(B$Sub.ID) #114, 118, 122, 126
table(C$Sub.ID) #116, 119, 123, 127
table(D$Sub.ID) #120, 124, 128

#make sure response and key are each lowercase
dat$Response = tolower(dat$Response)
dat$Key = tolower(dat$Key)

#Now compute the percent match
prop_correct_cued(dat, responses = "Response", key = "Key", 
                  id = "Sub.ID", id.trial = "Sub.ID", cutoff = 0)
