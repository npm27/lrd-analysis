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

#make sure response and key are each lowercase
dat$Response = tolower(dat$Response)
dat$Key = tolower(dat$Key)

#Now compute the percent match
prop_correct_cued(dat, responses = "Response", key = "Key", 
                  id = "Sub.ID", id.trial = "Sub.ID", cutoff = 0)
