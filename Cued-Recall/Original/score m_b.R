####set up####
dat = read.csv("Maxwell_Buchanan.csv")

colnames(dat)[3] = "Cue"
colnames(dat)[6] = "manually_coded"

table(dat$Sub.ID)

library(lrd)

#make sure response and key are each lowercase
dat$Response = tolower(dat$Response)
dat$Key = tolower(dat$Key)

#Add the trial id
dat$Trial_id = rep(c(1:63))

#Score all the things!
out0 = prop_correct_cued(dat, responses = "Response", key = "Key", key.trial = "Trial_id", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 0, flag = T)

out1 = prop_correct_cued(dat, responses = "Response", key = "Key", key.trial = "Trial_id", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 1, flag = T)

out2 = prop_correct_cued(dat, responses = "Response", key = "Key", key.trial = "Trial_id", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 2, flag = T)

out3 = prop_correct_cued(dat, responses = "Response", key = "Key", key.trial = "Trial_id", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 3, flag = T)

out4 = prop_correct_cued(dat, responses = "Response", key = "Key", key.trial = "Trial_id", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 4, flag = T)

out5 = prop_correct_cued(dat, responses = "Response", key = "Key", key.trial = "Trial_id", 
                  id = "Sub.ID", id.trial = "Trial_id", cutoff = 5, flag = T)

####Now get all the scored stuff back into the same df####
##First, get all the scored outputs
scored0 = out0$DF_Scored
scored1 = out1$DF_Scored
scored2 = out2$DF_Scored
scored3 = out3$DF_Scored
scored4 = out4$DF_Scored
scored5 = out5$DF_Scored

##Okay, now need to sort the rows back into the correct order
scored0 = scored0[order(scored0$Key), ]
scored0 = scored0[order(scored0$Sub.ID), ]

scored1 = scored1[order(scored1$Key), ]
scored1 = scored1[order(scored1$Sub.ID), ]

scored2 = scored2[order(scored2$Key), ]
scored2 = scored2[order(scored2$Sub.ID), ]

scored3 = scored3[order(scored3$Key), ]
scored3 = scored3[order(scored3$Sub.ID), ]

scored4 = scored4[order(scored4$Key), ]
scored4 = scored4[order(scored4$Sub.ID), ]

scored5 = scored5[order(scored5$Key), ]
scored5 = scored5[order(scored5$Sub.ID), ]

dat$scored0 = scored0$Scored
dat$scored1 = scored1$Scored
dat$scored2 = scored2$Scored
dat$scored3 = scored3$Scored
dat$scored4 = scored4$Scored
dat$scored5 = scored5$Scored

##Write the scored data to .csv
write.csv(dat, file = "maxwell_buchanan scored.csv", row.names = F)
