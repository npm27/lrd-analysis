####set up####
dat = read.csv("Maxwell Buchanan 2019 part 2.csv")

colnames(dat)[3] = "Cue"
colnames(dat)[6] = "manually_coded"

library(lrd)

#make sure response and key are each lowercase
dat$Response = tolower(dat$Response)
dat$Key = tolower(dat$Key)

#Add the trial id
dat$Trial_id = rep(c(1:63))

#Now compute the percent match
prop_correct_cued(dat, responses = "Response", key = "Key", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 0)

prop_