##set up
library(lrd)

##load test data
dat = read.csv("test_data.csv")

##make sure everything is lowercase
dat$Target = tolower(dat$Target)
dat$Answer = tolower(dat$Answer)

##replace NAs with blanks
dat$Answer[is.na(dat$Answer)] = ""

scored_cued = prop_correct_cued(dat, 
                                responses = "Answer",
                                id = "Sub.ID",
                                id.trial = "Trial_num",
                                key = "Target", 
                                key.trial =  "Trial_num",
                                cutoff = 1)

#View the output
View(scored_cued$DF_Scored) #Trial level
View(scored_cued$DF_Participant) #Participant proportion correct
